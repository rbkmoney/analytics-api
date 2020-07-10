%%%
%%% Copyright 2019 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(anapi_ct_helper).

-include_lib("common_test/include/ct.hrl").
-include_lib("anapi_dummy_data.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([init_suite/2]).
-export([start_app/1]).
-export([start_app/2]).
-export([start_anapi/1]).
-export([issue_token/2]).
-export([issue_token/3]).
-export([issue_token/4]).
-export([get_context/1]).
-export([get_context/2]).
-export([get_keysource/2]).
-export([start_mocked_service_sup/1]).
-export([stop_mocked_service_sup/1]).
-export([mock_services/2]).
-export([mock_services_/2]).
-export([get_lifetime/0]).

-define(ANAPI_IP                     , "::").
-define(ANAPI_PORT                   , 8080).
-define(ANAPI_HOST_NAME              , "localhost").
-define(ANAPI_URL                    , ?ANAPI_HOST_NAME ++ ":" ++ integer_to_list(?ANAPI_PORT)).

%%
-type config()          :: [{atom(), any()}].
-type app_name() :: atom().

-spec init_suite(module(), config()) ->
    config().
init_suite(Module, Config) ->
    SupPid = start_mocked_service_sup(Module),
    Apps1 = start_app(woody),
    Apps2 = start_anapi(Config),
    [{apps, lists:reverse(Apps2 ++ Apps1)}, {suite_test_sup, SupPid} | Config].

-spec start_app(app_name()) ->
    [app_name()].

start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);

start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) ->
    [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_anapi(config()) ->
    [app_name()].

start_anapi(Config) ->
    AnapiEnv = [
        {ip, ?ANAPI_IP},
        {port, ?ANAPI_PORT},
        {service_type, real},
        {access_conf, #{
            jwt => #{
                keyset => #{
                    % TODO use crypto:generate_key here when move on 21 Erlang
                    anapi => {pem_file, get_keysource("keys/local/private.pem", Config)}
                }
            }
        }},
        {max_request_deadline, 3000}
    ],
    start_app(anapi, AnapiEnv).

-spec get_keysource(_, config()) ->
    _.

get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).

-spec issue_token(_, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(ACL, LifeTime) ->
    issue_token(ACL, LifeTime, #{}).

-spec issue_token(_, _, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(ACL, LifeTime, ExtraProperties) ->
    issue_token(?STRING, ACL, LifeTime, ExtraProperties). % ugly

-spec issue_token(_, _, _, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(PartyID, ACL, LifeTime, ExtraProperties) ->
    Claims = maps:merge(#{
        ?STRING => ?STRING,
        <<"exp">> => LifeTime,
        <<"resource_access">> => #{
            <<"common-api">> => uac_acl:from_list(ACL)
        }
    }, ExtraProperties),
    UniqueId = get_unique_id(),
    case uac_authorizer_jwt:issue(
        UniqueId,
        PartyID,
        Claims,
        anapi
    ) of
        {ok, Token} ->
            {ok, Token};
        {error, nonexistent_signee} ->
            {error, nonexistent_signee};
        {error, {invalid_signee, Reason}} ->
            error({invalid_signee, Reason})
    end.

-spec get_unique_id() ->
    binary().

get_unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

-spec get_context(binary()) ->
    anapi_client_lib:context().

get_context(Token) ->
    get_context(Token, #{}).

-spec get_context(binary(), map()) ->
    anapi_client_lib:context().

get_context(Token, ExtraProperties) ->
    anapi_client_lib:get_context(?ANAPI_URL, Token, 10000, ipv4, ExtraProperties).

% TODO move it to `anapi_dummy_service`, looks more appropriate

-spec start_mocked_service_sup(module()) ->
    pid().

start_mocked_service_sup(Module) ->
    {ok, SupPid} = supervisor:start_link(Module, []),
    _ = unlink(SupPid),
    SupPid.

-spec stop_mocked_service_sup(pid()) ->
    _.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

-spec mock_services(_, _) ->
    _.

mock_services(Services, SupOrConfig) ->
    start_woody_client(mock_services_(Services, SupOrConfig)).

start_woody_client(ServiceURLs) ->
    start_app(anapi_woody_client, [
        {service_urls, ServiceURLs},
        {service_deadlines, #{
            merchant_stat => 1000,
            reporting => 1000,
            analytics => 1000
        }}
    ]).

-spec mock_services_(_, _) ->
    _.

% TODO need a better name
mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));

mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?ANAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => {scoper_woody_event_handler, #{}},
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    lists:foldl(
        fun (Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, anapi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {anapi_dummy_service, #{function => Fun}}}}.

% TODO not so failproof, ideally we need to bind socket first and then give to a ranch listener
get_random_port() ->
    rand:uniform(32768) + 32767.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?ANAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

-spec get_lifetime() ->
    map().

get_lifetime() ->
    get_lifetime(0, 0, 7).

get_lifetime(YY, MM, DD) ->
    #{
       <<"years">>  => YY,
       <<"months">> => MM,
       <<"days">>   => DD
    }.
