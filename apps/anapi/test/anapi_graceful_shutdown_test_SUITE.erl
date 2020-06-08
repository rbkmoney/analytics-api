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

-module(anapi_graceful_shutdown_test_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").
-include_lib("anapi_dummy_data.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).
-define(NUMBER_OF_WORKERS, 10).

-export([
    shutdown_test/1,
    request_interrupt_test/1
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
        {group, all_tests}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {all_tests, [],
            [
                create_report_ok_test
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
init_per_group(all_tests, Config) ->
    BasePermissions = [
        {[invoices], read},
        {[party], read},
        {[party], write},
        {[invoices, payments], read}
    ],
    {ok, Token} = anapi_ct_helper:issue_token(BasePermissions, unlimited),
    [{context, anapi_ct_helper:get_context(Token)} | Config];

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
    _ = application:start(anapi),
    anapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

-define(QUERY, [
    {shopID, ?STRING},
    {from_time, {{2016, 03, 22}, {6, 12, 27}}},
    {to_time, {{2016, 03, 22}, {6, 12, 27}}},
    {reportType, ?REPORT_TYPE}
]).

%%% Tests

-spec shutdown_test(config()) ->
    _.
shutdown_test(Config) ->
    anapi_ct_helper:mock_services([
        {reporting, fun ('CreateReport', _)       -> ok = timer:sleep(2000), {ok, ?INTEGER}
                      ; ('GetReport', [?INTEGER]) -> {ok, ?REPORT}
                    end}
    ], Config),
    {ok, _} = anapi_client_reports:create_report(?config(context, Config), ?QUERY),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = application:stop(anapi),
    ok = receive_loop(fun(Result) -> {ok, _} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun(Result) -> {error, econnrefused} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

-spec request_interrupt_test(config()) ->
    _.
request_interrupt_test(Config) ->
    anapi_ct_helper:mock_services([
        {reporting, fun
                        ('CreateReport', _)       -> ok = timer:sleep(2000), {ok, ?INTEGER};
                        ('GetReport', [?INTEGER]) -> {ok, ?REPORT}
                    end}
    ], Config),
    {ok, _} = anapi_client_reports:create_report(?config(context, Config), ?QUERY),
    }} = wapi_client_payres:store_bank_card(Context, ?STORE_BANK_CARD_REQUEST(CardNumber)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = application:stop(anapi),
    ok = receive_loop(fun({error, closed}) -> ok end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun(Result) -> {error, econnrefused} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

%%%

receive_loop(_, N, _Timeout) when N =< 0 ->
    ok;
receive_loop(MatchFun, N, Timeout) ->
    receive
        {result, Result} ->
            MatchFun(Result)
    after Timeout ->
        error(timeout)
    end,
    receive_loop(MatchFun, N - 1, Timeout).

spawn_workers(_, _, N) when N =< 0 ->
    ok;
spawn_workers(Context, ParentPID, N) ->
    erlang:spawn_link(fun() -> worker(Context, ParentPID) end),
    spawn_workers(Context, ParentPID, N - 1).

worker(Context, ParentPID) ->
    Query0 = [
        {shopID, ?STRING},
        {from_time, {{2016, 03, 22}, {6, 12, 27}}},
        {to_time, {{2016, 03, 22}, {6, 12, 27}}},
        {reportType, ?REPORT_TYPE}
    ],
    Result = anapi_client_reports:create_report(?config(context, Config), ?QUERY),
    ParentPID ! {result, Result}.
