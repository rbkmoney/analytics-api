-module(anapi_base_api_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").
-include_lib("dmsl/include/dmsl_accounter_thrift.hrl").
-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("dmsl/include/dmsl_webhooker_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").
-include_lib("dmsl/include/dmsl_reporting_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_tool_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payout_processing_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("anapi_dummy_data.hrl").
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
    search_invoices_ok_test/1,
    search_payments_ok_test/1,
    search_refunds_ok_test/1,
    search_payouts_ok_test/1
]).

-define(ANAPI_PORT                   , 8080).
-define(ANAPI_HOST_NAME              , "localhost").
-define(ANAPI_URL                    , ?ANAPI_HOST_NAME ++ ":" ++ integer_to_list(?ANAPI_PORT)).

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
        {group, operations_by_base_api_token}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {operations_by_base_api_token, [],
            [
                search_invoices_ok_test,
                search_payments_ok_test,
                search_refunds_ok_test,
                search_payouts_ok_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    anapi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = anapi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(operations_by_base_api_token, Config) ->
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
    {ok, Token} = anapi_ct_helper:issue_token(BasePermissions, unlimited),
    {ok, Token2} = anapi_ct_helper:issue_token(<<"TEST2">>, BasePermissions, unlimited, #{}),
    Config2 = [{context_with_diff_party, anapi_ct_helper:get_context(Token2)} | Config],
    [{context, anapi_ct_helper:get_context(Token)} | Config2];

init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, anapi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    anapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec search_invoices_ok_test(config()) ->
    _.
search_invoices_ok_test(Config) ->
    anapi_ct_helper:mock_services(
        [{merchant_stat, fun('GetInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end}],
        Config),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {invoiceStatus, <<"fulfilled">>},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.1">>},
        {shopID, ?STRING},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        % %%@FIXME cannot be used until getting the newest api client
        % swag generates an invalid regex
        % {pattern, "/^\\d{6,8}$/"} instead of
        % {pattern, "^\\d{6,8}$"} for the api
        % the handler is fine
        %
        % {first6, <<"424242">>},
        % {last4, <<"2222">>},
        % {rrn, <<"090909090909">>},
        {bankCardTokenProvider, <<"applepay">>},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>}
    ],

    {ok, _, _} = anapi_client_searches:search_invoices(?config(context, Config), Query).

-spec search_payments_ok_test(config()) ->
    _.
search_payments_ok_test(Config) ->
    anapi_ct_helper:mock_services(
        [{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end}],
        Config),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {payerEmail, <<"test@test.ru">>},
        {payerIP, <<"192.168.0.0.1">>},
        {shopID, ?STRING},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        % %%@FIXME cannot be used until getting the newest api client
        % swag generates an invalid regex
        % {pattern, "/^\\d{6,8}$/"} instead of
        % {pattern, "^\\d{6,8}$"} for the api
        % the handler is fine
        %
        % {first6, <<"424242">>},
        % {last4, <<"2222">>},
        % {rrn, <<"090909090909">>},
        % {approval_code, <<"808080">>},
        {bankCardTokenProvider, <<"applepay">>},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmount, 10000},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>}
    ],

    {ok, _, _} = anapi_client_searches:search_payments(?config(context, Config), Query).

-spec search_refunds_ok_test(config()) ->
    _.
search_refunds_ok_test(Config) ->
    anapi_ct_helper:mock_services([{merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {shopID, ?STRING},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {refundID, <<"testRefundID">>},
        {refundStatus, <<"succeeded">>},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>}
    ],

    {ok, _, _} = anapi_client_searches:search_refunds(?config(context, Config), Query).

-spec search_payouts_ok_test(config()) ->
    _.
search_payouts_ok_test(Config) ->
    anapi_ct_helper:mock_services([{merchant_stat, fun('GetPayouts', _) -> {ok, ?STAT_RESPONSE_PAYOUTS} end}], Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {shopID, ?STRING},
        {payoutID, <<"testPayoutID">>},
        {payoutToolType, <<"Wallet">>},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>}
    ],

    {ok, _, _} = anapi_client_searches:search_payouts(?config(context, Config), Query).
