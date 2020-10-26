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

-module(anapi_handler_utils).

-export([general_error/2]).
-export([logic_error/2]).
-export([server_error/1]).
-export([format_request_errors/1]).

-export([get_report_by_id/2]).

-export([service_call/2]).

-export([get_auth_context/1]).
-export([get_party_id/1]).

-export([merge_and_compact/2]).
-export([get_time/2]).

-export([enumerate_shop_ids/2]).

-export([create_dsl/3]).

-type processing_context() :: anapi_handler:processing_context().
-type response() :: anapi_handler:response().

-spec general_error(cowboy:http_status(), binary()) -> response().
general_error(Code, Message) ->
    create_error_resp(Code, #{<<"message">> => genlib:to_binary(Message)}).

-spec logic_error
    (term(), io_lib:chars() | binary()) -> response();
    (term(), {binary(), binary() | undefined}) -> response().
logic_error(Code, Message) ->
    Data = #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)},
    create_error_resp(400, Data).

create_error_resp(Code, Data) ->
    create_erorr_resp(Code, #{}, Data).

create_erorr_resp(Code, Headers, Data) ->
    {Code, Headers, Data}.

-spec server_error(integer()) -> {integer(), #{}, <<>>}.
server_error(Code) when Code >= 500 andalso Code < 600 ->
    {Code, #{}, <<>>}.

-spec format_request_errors(list()) -> binary().
format_request_errors([]) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

-spec get_report_by_id(binary(), processing_context()) -> woody:result().
get_report_by_id(ReportId, Context) ->
    Call = {reporting, 'GetReport', [ReportId]},
    service_call(Call, Context).

%%%

-spec service_call({atom(), atom(), list()}, processing_context()) -> woody:result().
service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    anapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

-spec get_auth_context(processing_context()) -> any().
get_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

-spec get_party_id(processing_context()) -> binary().
get_party_id(Context) ->
    uac_authorizer_jwt:get_subject_id(get_auth_context(Context)).

-spec merge_and_compact(map(), map()) -> map().
merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

-spec get_time(term(), map()) -> TimestampUTC :: binary() | undefined.
get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            anapi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

-spec create_dsl(atom(), map(), map()) -> map().
create_dsl(QueryType, QueryBody, QueryParams) ->
    merge_and_compact(
        #{<<"query">> => maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{})},
        QueryParams
    ).

-spec enumerate_shop_ids(anapi_handler:request_data(), processing_context()) -> [binary()].
enumerate_shop_ids(Req, Context) ->
    case get_request_shops(Req) of
        [] ->
            % Neither shopID nor shopIDs is set, will search using partyID & realm
            Realm = genlib_map:get('paymentInstitutionRealm', Req),
            PartyID = genlib_map:get('partyID', Req),
            UserID = get_party_id(Context),
            ok = validate_party_access(UserID, PartyID),
            get_party_shops(UserID, Realm, Context);
        ShopIDs ->
            ShopIDs
    end.

get_request_shops(Req) ->
    ShopIDs = genlib:define(genlib_map:get('shopIDs', Req), []),
    case genlib_map:get('shopID', Req) of
        undefined -> ShopIDs;
        ShopID -> [ShopID | ShopIDs]
    end.

get_party_shops(PartyID, undefined, Context) ->
    lists:append([
        get_party_shops(PartyID, test, Context),
        get_party_shops(PartyID, live, Context)
    ]);
get_party_shops(PartyID, Realm, Context) ->
    Call = {party_shop, 'GetShopsIds', [PartyID, Realm]},
    {ok, ShopIDs} = anapi_handler_utils:service_call(Call, Context),
    ShopIDs.

validate_party_access(_UserID, undefined) ->
    ok;
validate_party_access(UserID, PartyID) when UserID =:= PartyID ->
    ok;
validate_party_access(_UserID, PartyID) ->
    % One day there will be a service for checking party accesss
    throw({invalidPartyID, PartyID}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec enumerate_shop_ids_shopID_present_test() -> _.
enumerate_shop_ids_shopID_present_test() ->
    ShopID = <<"SHOP_ID">>,
    Req = #{
        'shopID' => ShopID
    },
    [ShopID] = enumerate_shop_ids(Req, #{}).

-spec enumerate_shop_ids_shopIDs_present_test() -> _.
enumerate_shop_ids_shopIDs_present_test() ->
    ShopIDs = [<<"SHOP_ID">>, <<"SHOP_ID_2">>],
    Req = #{
        'shopIDs' => ShopIDs
    },
    ShopIDs = enumerate_shop_ids(Req, #{}).

-spec enumerate_shop_ids_both_filters_present_test() -> _.
enumerate_shop_ids_both_filters_present_test() ->
    ShopID = <<"SHOP_ID">>,
    ShopIDs = [<<"SHOP_ID_2">>, <<"SHOP_ID_3">>],
    Req = #{
        'shopID'  => ShopID,
        'shopIDs' => ShopIDs
    },
    [ShopID | ShopIDs] = enumerate_shop_ids(Req, #{}).

-endif.
