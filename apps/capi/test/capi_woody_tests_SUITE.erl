-module(capi_woody_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("capi_dummy_data.hrl").
-include_lib("jose/include/jose_jwk.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    woody_unexpected_test/1,
    woody_unavailable_test/1,
    woody_retry_test/1,
    woody_unknown_test/1
]).

-define(badresp(Code), {error, {invalid_response_code, Code}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, woody_errors}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {woody_errors, [],
            [
                woody_unexpected_test,
                woody_unavailable_test,
                woody_retry_test,
                woody_unknown_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    capi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = capi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(woody_errors, Config) ->
    BasePermissions = [
        {[invoices], write},
        {[invoices], read},
        {[party], write},
        {[party], read},
        {[invoices, payments], write},
        {[invoices, payments], read},
        {[customers], write},
        {[payouts], write},
        {[payouts], read}
    ],
    {ok, Token} = capi_ct_helper:issue_token(BasePermissions, unlimited),
    Context = capi_ct_helper:get_context(Token),
    [{context, Context} | Config];

init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, capi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    capi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec woody_unexpected_test(config()) ->
    _.

woody_unexpected_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> {ok, "spanish inquisition"} end}], Config),
    ?badresp(500) = capi_client_parties:get_my_party(?config(context, Config)).

-spec woody_unavailable_test(config()) ->
    _.

woody_unavailable_test(Config) ->
    _ = capi_ct_helper:start_app(capi_woody_client, [{service_urls, #{
        party_management => <<"http://spanish.inquision/v1/partymgmt">>
    }}]),
    ?badresp(503) = capi_client_parties:get_my_party(?config(context, Config)).

-spec woody_retry_test(config()) ->
    _.

woody_retry_test(Config) ->
    _ = capi_ct_helper:start_app(capi_woody_client, [
        {service_urls, #{
            party_management => <<"http://spanish.inquision/v1/partymgmt">>
        }},
        {service_retries, #{
            party_management    => #{
                'Get'   => {linear, 30, 1000},
                '_'     => finish
            }
        }},
        {service_deadlines, #{
            party_management => 5000
        }}
    ]),
    {Time, ?badresp(503)} = timer:tc(capi_client_parties, get_my_party, [?config(context, Config)]),
    true = (Time > 3000000) and (Time < 10000000).

-spec woody_unknown_test(config()) ->
    _.

woody_unknown_test(Config) ->
    _ = capi_ct_helper:mock_services([{party_management, fun('Get', _) -> timer:sleep(60000) end}], Config),
    ?badresp(504) = capi_client_parties:get_my_party(?config(context, Config)).
