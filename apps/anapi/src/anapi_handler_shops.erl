-module(anapi_handler_shops).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-behaviour(anapi_handler).
-export([process_request/3]).
-import(anapi_handler_utils, [general_error/2]).

-spec process_request(
    OperationID :: anapi_handler:operation_id(),
    Req         :: anapi_handler:request_data(),
    Context     :: anapi_handler:processing_context()
) ->
    {ok | error, anapi_handler:response() | noimpl}.

process_request('ActivateShop', Req, Context) ->
    Call = {party_management, 'ActivateShop', [maps:get(shopID, Req)]},
    case anapi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, general_error(404, <<"Shop not found">>)};
                #payproc_InvalidShopStatus{status = {suspension, {active, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;

process_request('SuspendShop', Req, Context) ->
    Call = {party_management, 'SuspendShop', [maps:get(shopID, Req)]},
    case anapi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, _R} ->
            {ok, {204, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_ShopNotFound{} ->
                    {ok, general_error(404, <<"Shop not found">>)};
                #payproc_InvalidShopStatus{status = {suspension, {suspended, _}}} ->
                    {ok, {204, #{}, undefined}}
            end
    end;

process_request('GetShops', _Req, Context) ->
    Party = anapi_utils:unwrap(anapi_handler_utils:get_my_party(Context)),
    {ok, {200, #{}, decode_shops_map(Party#domain_Party.shops)}};

process_request('GetShopByID', Req, Context) ->
    Call = {party_management, 'GetShop', [maps:get(shopID, Req)]},
    case anapi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, Shop} ->
            {ok, {200, #{}, decode_shop(Shop)}};
        {exception, #payproc_ShopNotFound{}} ->
            {ok, general_error(404, <<"Shop not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

decode_shops_map(Shops) ->
    anapi_handler_decoder_utils:decode_map(Shops, fun decode_shop/1).

decode_shop(Shop) ->
    genlib_map:compact(#{
        <<"id"          >> => Shop#domain_Shop.id,
        <<"createdAt"   >> => Shop#domain_Shop.created_at,
        <<"isBlocked"   >> => anapi_handler_decoder_party:is_blocked(Shop#domain_Shop.blocking),
        <<"isSuspended" >> => anapi_handler_decoder_party:is_suspended(Shop#domain_Shop.suspension),
        <<"categoryID"  >> => anapi_handler_decoder_utils:decode_category_ref(Shop#domain_Shop.category),
        <<"details"     >> => anapi_handler_decoder_party:decode_shop_details(Shop#domain_Shop.details),
        <<"location"    >> => anapi_handler_decoder_party:decode_shop_location(Shop#domain_Shop.location),
        <<"contractID"  >> => Shop#domain_Shop.contract_id,
        <<"payoutToolID">> => Shop#domain_Shop.payout_tool_id,
        <<"scheduleID"  >> =>
            anapi_handler_decoder_utils:decode_business_schedule_ref(Shop#domain_Shop.payout_schedule),
        <<"account"     >> => decode_shop_account(Shop#domain_Shop.account)
    }).

decode_shop_account(undefined) ->
    undefined;
decode_shop_account(#domain_ShopAccount{currency = Currency, settlement = SettlementID, guarantee = GuaranteeID}) ->
    #{
        <<"guaranteeID" >> => GuaranteeID,
        <<"settlementID">> => SettlementID,
        <<"currency"    >> => anapi_handler_decoder_utils:decode_currency(Currency)
    }.

