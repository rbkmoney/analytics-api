-module(anapi_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().

-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([fragments/0]).

-type prototypes() :: [
    {operation, prototype_operation()} |
    {reports, prototype_reports()}
].

-type prototype_operation() :: #{
    id => swag_server:operation_id(),
    party_id => entity_id(),
    shop_id => entity_id(),
    report_id => entity_id(),
    file_id => entity_id()
}.

-type prototype_reports() :: #{
    report => report_id() | report() | undefined
}.

-type report_id() :: reporter_reports_thrift:'ReportID'().
-type report() :: reporter_reports_thrift:'Report'().

-type entity_id() :: binary().

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).

-export([new/0]).
-export([build/3]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(anapi, deployment, undefined)}
    }).

-spec build(prototypes(), fragments(), woody_context:ctx()) -> fragments().
build(Prototype, {Acc0, External}, WoodyCtx) ->
    Acc1 = lists:foldl(fun({T, Params}, Acc) -> build(T, Params, Acc, WoodyCtx) end, Acc0, Prototype),
    {Acc1, External}.

build(operation, Params = #{id := OperationID}, Acc, _WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        anapi = #bctx_v1_ContextAnalyticsAPI{
            op = #bctx_v1_AnalyticsAPIOperation{
                id = operation_id_to_binary(OperationID),
                party = maybe_entity(party_id, Params),
                shop = maybe_entity(shop_id, Params),
                report = maybe_entity(report_id, Params),
                file = maybe_entity(file_id, Params)
            }
        }
    };
build(reports, Params = #{}, Acc, WoodyCtx) ->
    Acc#bctx_v1_ContextFragment{
        reports = #bctx_v1_ContextReports{
            report = maybe_with(
                report,
                Params,
                fun(V) -> build_report_ctx(V, WoodyCtx) end
            )
        }
    }.

%%

build_report_ctx(ID, WoodyCtx) when is_integer(ID) ->
    maybe_with_woody_result(reporting, 'GetReport', {ID}, WoodyCtx, fun build_report_ctx/1);
build_report_ctx(Report, _WoodyCtx) ->
    build_report_ctx(Report).

build_report_ctx(#reports_Report{report_id = ID, party_id = PartyID, shop_id = ShopID, files = Files}) ->
    #bctx_v1_Report{
        id = integer_to_binary(ID),
        party = build_entity(PartyID),
        shop = maybe(ShopID, fun build_entity/1),
        files = build_set(lists:map(fun build_report_file_ctx/1, Files))
    }.

build_report_file_ctx(#reports_FileMeta{file_id = ID}) ->
    build_entity(ID).

maybe(V, Then) when V /= undefined ->
    Then(V);
maybe(undefined, _Then) ->
    undefined.

maybe_with_woody_result(ServiceName, Function, Args, WoodyCtx, Then) ->
    case anapi_woody_client:call_service(ServiceName, Function, Args, WoodyCtx) of
        {ok, Result} ->
            Then(Result);
        {exception, _} ->
            undefined
    end.

maybe_with(Name, Params, Then) ->
    case maps:get(Name, Params, undefined) of
        V when V /= undefined ->
            Then(V);
        undefined ->
            undefined
    end.

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).

maybe_entity(Name, Params) ->
    maybe_with(Name, Params, fun build_entity/1).

build_entity(ID) when is_binary(ID) ->
    #bctx_v1_Entity{id = ID};
build_entity(ID) when is_integer(ID) ->
    #bctx_v1_Entity{id = integer_to_binary(ID)}.

build_set(L) when is_list(L) ->
    ordsets:from_list(L).
