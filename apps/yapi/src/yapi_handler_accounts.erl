-module(yapi_handler_accounts).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-behaviour(yapi_handler).
-export([process_request/3]).
-import(yapi_handler_utils, [general_error/2]).

-spec process_request(
    OperationID :: yapi_handler:operation_id(),
    Req         :: yapi_handler:request_data(),
    Context     :: yapi_handler:processing_context()
) ->
    {ok | error, yapi_handler:response() | noimpl}.

process_request('GetAccountByID', Req, Context) ->
    Call = {party_management, 'GetAccountState', [genlib:to_int(maps:get('accountID', Req))]},
    case yapi_handler_utils:service_call_with([user_info, party_id, party_creation], Call, Context) of
        {ok, S} ->
            {ok, {200, #{}, decode_account_state(S)}};
        {exception, #payproc_AccountNotFound{}} ->
            {ok, general_error(404, <<"Account not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

decode_account_state(AccountState) ->
    #{
        <<"id"             >> => AccountState#payproc_AccountState.account_id,
        <<"ownAmount"      >> => AccountState#payproc_AccountState.own_amount,
        <<"availableAmount">> => AccountState#payproc_AccountState.available_amount,
        <<"currency"       >> => yapi_handler_decoder_utils:decode_currency(AccountState#payproc_AccountState.currency)
    }.
