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
    HealthRoutes = [{'_', [erl_health_handle:get_route(genlib_app:env(?APP, health_checkers, []))]}],
    SwaggerHandlerOpts = genlib_app:env(?APP, swagger_handler_opts, #{}),
    SwaggerSpec = anapi_swagger_server:child_spec({HealthRoutes, LogicHandler, SwaggerHandlerOpts}),
    UacConf = genlib_app:env(anapi, access_conf),
    ok = uac:configure(UacConf),
    {ok, {
        {one_for_all, 0, 1},
            LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []} .

get_logic_handler_info() ->
    case genlib_app:env(?APP, service_type) of
        real ->
            {anapi_handler, []};
        undefined ->
            exit(undefined_service_type)
    end.
