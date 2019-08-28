-module(capi_handler_payments).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-behaviour(capi_handler).
-export([process_request/3]).
-import(capi_handler_utils, [general_error/2, logic_error/2]).

-spec process_request(
    OperationID :: capi_handler:operation_id(),
    Req         :: capi_handler:request_data(),
    Context     :: capi_handler:processing_context()
) ->
    {ok | error, capi_handler:response() | noimpl}.

process_request('CreatePayment' = OperationID, Req, Context) ->
    InvoiceID = maps:get('invoiceID', Req),
    PaymentParams = maps:get('PaymentParams', Req),
    PartyID = capi_handler_utils:get_party_id(Context),
    Result =
        try
            create_payment(InvoiceID, PartyID, PaymentParams, Context, OperationID)
        catch
            {external_id_conflict, _, _} = Error ->
                {error, Error};
            throw:Error when
                Error =:= invalid_token orelse
                Error =:= invalid_payment_session
            ->
                {error, Error}
        end,

    case Result of
        {ok, Payment} ->
            {ok, {201, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidInvoiceStatus{} ->
                    {ok, logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)};
                #payproc_InvoicePaymentPending{} ->
                    ErrorResp = logic_error(
                        invoicePaymentPending,
                        <<"Invoice payment pending">>
                    ),
                    {ok, ErrorResp};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvalidContractStatus{} ->
                    ErrorResp = logic_error(
                        invalidContractStatus,
                        <<"Invalid contract status">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidRecurrentParentPayment{} ->
                    ErrorResp = logic_error(
                        invalidRecurrentParent,
                        <<"Specified recurrent parent is invalid">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end;
        {error, invalid_token} ->
            {ok, logic_error(
                invalidPaymentToolToken,
                <<"Specified payment tool token is invalid">>
            )};
        {error, invalid_payment_session} ->
            {ok, logic_error(
                invalidPaymentSession,
                <<"Specified payment session is invalid">>
            )};
        {error, {external_id_conflict, PaymentID, ExternalID}} ->
            {ok, logic_error(externalIDConflict, {PaymentID, ExternalID})}
    end;

process_request('GetPayments', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    case capi_handler_utils:get_invoice_by_id(InvoiceID, Context) of
        {ok, #'payproc_Invoice'{payments = Payments}} ->
            {ok, {200, #{}, [decode_invoice_payment(InvoiceID, P, Context) || P <- Payments]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetPaymentByID', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    case capi_handler_utils:get_payment_by_id(InvoiceID, maps:get(paymentID, Req), Context) of
        {ok, Payment} ->
            {ok, {200, #{}, decode_invoice_payment(InvoiceID, Payment, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('CancelPayment', Req, Context) ->
    CallArgs = [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(<<"reason">>, maps:get('Reason', Req))],
    Call = {invoicing, 'CancelPayment', CallArgs},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, logic_error(invalidPaymentStatus, <<"Invalid payment status">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)}
            end
    end;

process_request('CapturePayment', Req, Context) ->
    CaptureParams = maps:get('CaptureParams', Req),
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    CallArgs = [
        InvoiceID,
        PaymentID,
        #payproc_InvoicePaymentCaptureParams{
            reason = maps:get(<<"reason">>, CaptureParams),
            cash = encode_optional_cash(CaptureParams, InvoiceID, PaymentID, Context)
        }
    ],
    Call = {invoicing, 'CapturePaymentNew', CallArgs},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvalidPaymentStatus{} ->
                    {ok, logic_error(invalidPaymentStatus, <<"Invalid payment status">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InconsistentCaptureCurrency{payment_currency = PaymentCurrency} ->
                    {ok, logic_error(
                        inconsistentCaptureCurrency,
                        io_lib:format("Correct currency: ~p", [PaymentCurrency])
                    )};
                #payproc_AmountExceededCaptureBalance{payment_amount = PaymentAmount} ->
                    {ok, logic_error(
                        amountExceededCaptureBalance,
                        io_lib:format("Max amount: ~p", [PaymentAmount])
                    )}
            end
    end;

process_request('CreateRefund', Req, Context) ->
    InvoiceID = maps:get(invoiceID, Req),
    PaymentID = maps:get(paymentID, Req),
    RefundParams = maps:get('RefundParams', Req),
    Params = #payproc_InvoicePaymentRefundParams{
        reason = genlib_map:get(<<"reason">>, RefundParams),
        cash = encode_optional_cash(RefundParams, InvoiceID, PaymentID, Context)
    },
    Call = {invoicing, 'RefundPayment', [InvoiceID, PaymentID, Params]},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, Refund} ->
            {ok, {201, #{}, capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvalidPartyStatus{} ->
                    {ok, logic_error(invalidPartyStatus, <<"Invalid party status">>)};
                #payproc_InvalidShopStatus{} ->
                    {ok, logic_error(invalidShopStatus, <<"Invalid shop status">>)};
                #payproc_InvalidContractStatus{} ->
                    ErrorResp = logic_error(
                        invalidContractStatus,
                         <<"Invalid contract status">>
                    ),
                    {ok, ErrorResp};
                #payproc_OperationNotPermitted{} ->
                    ErrorResp = logic_error(
                        operationNotPermitted,
                        <<"Operation not permitted">>
                    ),
                    {ok, ErrorResp};
                #payproc_InvalidPaymentStatus{} ->
                    ErrorResp = logic_error(
                        invalidPaymentStatus,
                        <<"Invalid invoice payment status">>
                    ),
                    {ok, ErrorResp};
                #payproc_InsufficientAccountBalance{} ->
                    {ok, logic_error(
                        insufficentAccountBalance,
                        <<"Operation can not be conducted because of insufficient funds on the merchant account">>
                    )};
                #payproc_InvoicePaymentAmountExceeded{} ->
                    ErrorResp = logic_error(
                        invoicePaymentAmountExceeded,
                        <<"Payment amount exceeded">>
                    ),
                    {ok, ErrorResp};
                #payproc_InconsistentRefundCurrency{} ->
                    ErrorResp = logic_error(
                        inconsistentRefundCurrency,
                        <<"Inconsistent refund currency">>
                    ),
                    {ok, ErrorResp};
                #'InvalidRequest'{errors = Errors} ->
                    FormattedErrors = capi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)}
            end
    end;

process_request('GetRefunds', Req, Context) ->
    case capi_handler_utils:get_payment_by_id(maps:get(invoiceID, Req), maps:get(paymentID, Req), Context) of
        {ok, #payproc_InvoicePayment{refunds = Refunds}} ->
            {ok, {200, #{}, [capi_handler_decoder_invoicing:decode_refund(R, Context) || R <- Refunds]}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

process_request('GetRefundByID', Req, Context) ->
    Call =
        {invoicing, 'GetPaymentRefund', [maps:get(invoiceID, Req), maps:get(paymentID, Req), maps:get(refundID, Req)]},
    case capi_handler_utils:service_call_with([user_info], Call, Context) of
        {ok, Refund} ->
            {ok, {200, #{}, capi_handler_decoder_invoicing:decode_refund(Refund, Context)}};
        {exception, Exception} ->
            case Exception of
                #payproc_InvoicePaymentRefundNotFound{} ->
                    {ok, general_error(404, <<"Invoice payment refund not found">>)};
                #payproc_InvoicePaymentNotFound{} ->
                    {ok, general_error(404, <<"Payment not found">>)};
                #payproc_InvoiceNotFound{} ->
                    {ok, general_error(404, <<"Invoice not found">>)};
                #payproc_InvalidUser{} ->
                    {ok, general_error(404, <<"Invoice not found">>)}
            end
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

%%

create_payment(InvoiceID, PartyID, PaymentParams, #{woody_context := WoodyCtx} = Context, BenderPrefix) ->
    ExternalID    = maps:get(<<"externalID">>, PaymentParams, undefined),
    IdempotentKey = capi_bender:get_idempotent_key(BenderPrefix, PartyID, ExternalID),
    Hash = erlang:phash2(PaymentParams),
    case capi_bender:gen_by_sequence(IdempotentKey, InvoiceID, Hash, WoodyCtx) of
        {ok, ID} ->
            Params = encode_invoice_payment_params(ID, ExternalID, PaymentParams),
            Call = {invoicing, 'StartPayment', [InvoiceID, Params]},
            capi_handler_utils:service_call_with([user_info], Call, Context);
        {error, {external_id_conflict, ID}} ->
            throw({external_id_conflict, ID, ExternalID})
   end.

encode_invoice_payment_params(ID, ExternalID, PaymentParams) ->
    Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
    #payproc_InvoicePaymentParams{
        id             = ID,
        external_id    = ExternalID,
        payer          = encode_payer_params(genlib_map:get(<<"payer">>, PaymentParams)),
        flow           = encode_flow(Flow),
        make_recurrent = genlib_map:get(<<"makeRecurrent">>, PaymentParams, false)
    }.

encode_payer_params(#{
    <<"payerType" >> := <<"CustomerPayer">>,
    <<"customerID">> := ID
}) ->
    {customer, #payproc_CustomerPayerParams{customer_id = ID}};

encode_payer_params(#{
    <<"payerType"       >> := <<"PaymentResourcePayer">>,
    <<"paymentToolToken">> := Token,
    <<"paymentSession"  >> := EncodedSession,
    <<"contactInfo"     >> := ContactInfo
}) ->
    PaymentTool = capi_handler_encoder:encode_payment_tool_token(Token),
    {ClientInfo, PaymentSession} = capi_handler_utils:unwrap_payment_session(EncodedSession),
    {payment_resource, #payproc_PaymentResourcePayerParams{
        resource = #domain_DisposablePaymentResource{
            payment_tool = PaymentTool,
            payment_session_id = PaymentSession,
            client_info = capi_handler_encoder:encode_client_info(ClientInfo)
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }};

encode_payer_params(#{
    <<"payerType"             >> := <<"RecurrentPayer">>,
    <<"recurrentParentPayment">> := RecurrentParent,
    <<"contactInfo"           >> := ContactInfo
}) ->
    #{
        <<"invoiceID">> := InvoiceID,
        <<"paymentID">> := PaymentID
    } = RecurrentParent,
    {recurrent, #payproc_RecurrentPayerParams{
        recurrent_parent = #domain_RecurrentParentPayment{
            invoice_id = InvoiceID,
            payment_id = PaymentID
        },
        contact_info = capi_handler_encoder:encode_contact_info(ContactInfo)
    }}.

encode_flow(#{<<"type">> := <<"PaymentFlowInstant">>}) ->
    {instant, #payproc_InvoicePaymentParamsFlowInstant{}};

encode_flow(#{<<"type">> := <<"PaymentFlowHold">>} = Entity) ->
    OnHoldExpiration = maps:get(<<"onHoldExpiration">>, Entity, <<"cancel">>),
    {hold, #payproc_InvoicePaymentParamsFlowHold{
        on_hold_expiration = binary_to_existing_atom(OnHoldExpiration, utf8)
    }}.

encode_optional_cash(Params = #{<<"amount">> := _, <<"currency">> := _}, _, _, _) ->
    capi_handler_encoder:encode_cash(Params);
encode_optional_cash(Params = #{<<"amount">> := _}, InvoiceID, PaymentID, Context) ->
    {ok, #payproc_InvoicePayment{
        payment = #domain_InvoicePayment{
            cost = #domain_Cash{currency = Currency}
        }
    }} = capi_handler_utils:get_payment_by_id(InvoiceID, PaymentID, Context),
    capi_handler_encoder:encode_cash(Params#{<<"currency">> => capi_handler_decoder_utils:decode_currency(Currency)});
encode_optional_cash(_, _, _, _) ->
    undefined.

%%

decode_invoice_payment(InvoiceID, #payproc_InvoicePayment{payment = Payment}, Context) ->
    capi_handler_decoder_invoicing:decode_payment(InvoiceID, Payment, Context).

