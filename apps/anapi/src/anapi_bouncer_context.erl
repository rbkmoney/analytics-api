-module(anapi_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().

-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([fragments/0]).

-type prototypes() :: [
    {operation, prototype_operation()}
].

-type prototype_operation() :: #{
    id => swag_server:operation_id(),
    party => entity_id(),
    shop_ids => [entity_id()]
}.

-type entity_id() :: binary().

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).

-export([new/0]).
-export([build/2]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(capi, deployment, undefined)}
    }).

build(Params, {Acc, External}) ->
    {Acc#bctx_v1_ContextFragment{
        capi = #bctx_v1_ContextCommonAPI{
            op = #bctx_v1_AnalyticsAPIOperation{
                id = operation_id_to_binary(maps:get(id, Params)),
                party = maybe_entity(party_id, Params),
                shops = build_set(maybe_with(shop_ids, Params, fun(Shops) -> [build_entity(S) || S <- Shops] end))
            }
        }
    }, External}.

%%

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
