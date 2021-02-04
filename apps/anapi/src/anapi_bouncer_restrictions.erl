-module(anapi_bouncer_restrictions).

-include_lib("bouncer_proto/include/bouncer_restriction_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

%% API
-export([get_restricted_shop_ids/1]).

get_restricted_shop_ids(Restrictions) ->
    #brstn_Restrictions{
        anapi = #brstn_RestrictionsAnalyticsAPI{
            op = #brstn_AnalyticsAPIOperationRestrictions{
                shops = ShopEntities
            }
        }
    } = Restrictions,
    [ID || #bctx_v1_Entity{id = ID} <- ordsets:to_list(ShopEntities)].
