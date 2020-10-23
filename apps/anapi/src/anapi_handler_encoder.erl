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

-module(anapi_handler_encoder).

-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("analytics_proto/include/analytics_proto_analytics_thrift.hrl").

-export([encode_stat_request/2]).
-export([encode_analytics_request/2]).

-export_type([encode_data/0]).

-type encode_data() :: tuple().
-type merchant_filter() :: #analytics_MerchantFilter{}.
-type time_filter() :: #analytics_TimeFilter{}.
-type filter_request() :: #analytics_FilterRequest{}.
-type split_filter_request() :: #analytics_SplitFilterRequest{}.
-type analytics_request_type() :: merchant_filter | time_filter | filter_request | split_filter_request.

-spec encode_stat_request(map() | binary(), binary() | undefined) -> encode_data().
encode_stat_request(Dsl, ContinuationToken) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl), ContinuationToken);
encode_stat_request(Dsl, ContinuationToken) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl,
        continuation_token = ContinuationToken
    }.

-spec encode_analytics_request(analytics_request_type(), map()) ->
    merchant_filter() | time_filter() | filter_request() | split_filter_request().
encode_analytics_request(merchant_filter, #{
    party_id := PartyID,
    shop_ids := ShopIDs,
    exclude_shop_ids := ExcludeShopIDs
}) ->
    #analytics_MerchantFilter{
        party_id = PartyID,
        shop_ids = ShopIDs,
        exclude_shop_ids = ExcludeShopIDs
    };
encode_analytics_request(time_filter, #{
    from_time := FromTime,
    to_time := ToTime
}) ->
    #analytics_TimeFilter{
        from_time = FromTime,
        to_time = ToTime
    };
encode_analytics_request(filter_request, Query) ->
    #analytics_FilterRequest{
        merchant_filter = encode_analytics_request(merchant_filter, Query),
        time_filter = encode_analytics_request(time_filter, Query)
    };
encode_analytics_request(split_filter_request, #{split_unit := SplitUnit} = Query) ->
    #analytics_SplitFilterRequest{
        filter_request = encode_analytics_request(filter_request, Query),
        split_unit = SplitUnit
    }.
