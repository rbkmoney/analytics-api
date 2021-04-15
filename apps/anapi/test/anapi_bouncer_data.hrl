-ifndef(anapi_bouncer_data_included__).
-define(anapi_bouncer_data_included__, ok).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-define(JUDGEMENT(Resolution), #bdcs_Judgement{resolution = Resolution}).
-define(ALLOWED, {allowed, #bdcs_ResolutionAllowed{}}).
-define(FORBIDDEN, {forbidden, #bdcs_ResolutionForbidden{}}).
-define(RESTRICTED(Restrictions), {forbidden, #bdcs_ResolutionRestricted{restrictions = Restrictions}}).

-define(CTX_ENTITY(ID), #bctx_v1_Entity{id = ID}).

-define(CTX_ANAPI(Op), #bctx_v1_ContextAnalyticsAPI{op = Op}).

-define(CTX_ANAPI_OP(ID), #bctx_v1_AnalyticsAPIOperation{id = ID}).

-define(CTX_PARTY_OP(ID, PartyID), #bctx_v1_AnalyticsAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_SHOP_OP(ID, PartyID, ShopID), #bctx_v1_AnalyticsAPIOperation{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID)
}).

-define(CTX_REPORT_OP(ID, ReportID, PartyID), #bctx_v1_AnalyticsAPIOperation{
    id = ID,
    report = ?CTX_ENTITY(ReportID),
    party = ?CTX_ENTITY(PartyID)
}).

-define(CTX_REPORT(ID, PartyID, ShopID, Files), #bctx_v1_Report{
    id = ID,
    party = ?CTX_ENTITY(PartyID),
    shop = ?CTX_ENTITY(ShopID),
    files = Files
}).

-define(assertContextMatches(Expect), fun(Context) ->
    ?assertMatch(Expect, Context),
    {ok, ?JUDGEMENT(?ALLOWED)}
end).

-endif.
