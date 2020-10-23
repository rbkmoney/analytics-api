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

%% @doc Top level supervisor.
%% @end

-module(anapi_sup).

-behaviour(supervisor).

-define(APP, anapi).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {LogicHandler, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthCheck = enable_health_logging(genlib_app:env(anapi, health_check, #{})),
    HealthRoutes = [{'_', [erl_health_handle:get_route(HealthCheck)]}],
    SwaggerHandlerOpts = genlib_app:env(?APP, swagger_handler_opts, #{}),
    SwaggerSpec = anapi_swagger_server:child_spec({HealthRoutes, LogicHandler, SwaggerHandlerOpts}),
    UacConf = get_uac_config(),
    ok = uac:configure(UacConf),
    {ok, {
        {one_for_all, 0, 1},
        LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []}.
get_logic_handler_info() ->
    case genlib_app:env(?APP, service_type) of
        real ->
            {anapi_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

get_uac_config() ->
    maps:merge(
        get_authorization_config(),
        #{access => anapi_auth:get_access_config()}
    ).

get_authorization_config() ->
    case genlib_app:env(anapi, access_conf) of
        undefined ->
            exit(undefined_access_configuration);
        Config ->
            Config
    end.
