-module(anapi_handler_search).

-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-behaviour(anapi_handler).
-export([process_request/3]).
-import(anapi_handler_utils, [logic_error/2]).

-spec process_request(
    OperationID :: anapi_handler:operation_id(),
    Req         :: anapi_handler:request_data(),
    Context     :: anapi_handler:processing_context()
) ->
    {ok | error, anapi_handler:response() | noimpl}.

process_request('SearchInvoices', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => anapi_handler_utils:get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => anapi_handler_utils:get_time('fromTime', Req),
        <<"to_time"                  >> => anapi_handler_utils:get_time('toTime', Req),
        <<"invoice_status"           >> => genlib_map:get('invoiceStatus', Req),
        <<"payment_status"           >> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow"             >> => genlib_map:get('paymentFlow', Req),
        <<"payment_method"           >> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id"      >> => genlib_map:get('customerID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"payment_email"            >> => genlib_map:get('payerEmail', Req),
        <<"payment_ip"               >> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint"      >> => genlib_map:get('payerFingerprint', Req),
        <<"payment_amount"           >> => genlib_map:get('paymentAmount', Req),
        <<"invoice_amount"           >> => genlib_map:get('invoiceAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_rrn"              >> => genlib_map:get('rrn', Req),
        <<"payment_first6"           >> => genlib_map:get('first6', Req),
        <<"payment_last4"            >> => genlib_map:get('last4', Req),
        <<"exclude"                  >> => construct_exclude(Req)
    },
    Opts = #{
        thrift_fun => 'GetInvoices',
        decode_fun => fun decode_stat_invoice/2
    },
    process_search_request(invoices, Query, Req, Context, Opts);

process_request('SearchPayments', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => anapi_handler_utils:get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"from_time"                >> => anapi_handler_utils:get_time('fromTime', Req),
        <<"to_time"                  >> => anapi_handler_utils:get_time('toTime', Req),
        <<"payment_status"           >> => genlib_map:get('paymentStatus', Req),
        <<"payment_flow"             >> => genlib_map:get('paymentFlow', Req),
        <<"payment_method"           >> => encode_payment_method(genlib_map:get('paymentMethod', Req)),
        <<"payment_terminal_provider">> => genlib_map:get('paymentTerminalProvider', Req),
        <<"payment_customer_id"      >> => genlib_map:get('customerID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"payment_email"            >> => genlib_map:get('payerEmail', Req),
        <<"payment_ip"               >> => genlib_map:get('payerIP', Req),
        <<"payment_fingerprint"      >> => genlib_map:get('payerFingerprint', Req),
        <<"payment_amount"           >> => genlib_map:get('paymentAmount', Req),
        <<"payment_token_provider"   >> => genlib_map:get('bankCardTokenProvider', Req),
        <<"payment_system"           >> => genlib_map:get('bankCardPaymentSystem', Req),
        <<"payment_first6"           >> => genlib_map:get('first6', Req),
        <<"payment_last4"            >> => genlib_map:get('last4', Req),
        <<"payment_rrn"              >> => genlib_map:get('rrn', Req),
        <<"payment_approval_code"    >> => genlib_map:get('approvalCode', Req),
        <<"exclude"                  >> => construct_exclude(Req)
    },
    Opts = #{
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_payment/2
    },
    process_search_request(payments, Query, Req, Context, Opts);

process_request('SearchPayouts', Req, Context) ->
    Query = #{
        <<"merchant_id"    >> => anapi_handler_utils:get_party_id(Context),
        <<"shop_id"        >> => genlib_map:get('shopID', Req),
        <<"from_time"      >> => anapi_handler_utils:get_time('fromTime', Req),
        <<"to_time"        >> => anapi_handler_utils:get_time('toTime', Req),
        <<"payout_statuses">> => [<<"confirmed">>, <<"paid">>],
        <<"payout_id"      >> => genlib_map:get('payoutID', Req),
        <<"payout_type"    >> => encode_payout_type(genlib_map:get('payoutToolType', Req)),
        <<"exclude"        >> => construct_exclude(Req)
    },
    Opts = #{
        thrift_fun => 'GetPayouts',
        decode_fun => fun decode_stat_payout/2
    },
    process_search_request(payouts, Query, Req, Context, Opts);

process_request('SearchRefunds', Req, Context) ->
    Query = #{
        <<"merchant_id"              >> => anapi_handler_utils:get_party_id(Context),
        <<"shop_id"                  >> => genlib_map:get('shopID', Req),
        <<"invoice_id"               >> => genlib_map:get('invoiceID', Req),
        <<"payment_id"               >> => genlib_map:get('paymentID', Req),
        <<"refund_id"                >> => genlib_map:get('refundID', Req),
        <<"from_time"                >> => anapi_handler_utils:get_time('fromTime', Req),
        <<"to_time"                  >> => anapi_handler_utils:get_time('toTime', Req),
        <<"refund_status"            >> => genlib_map:get('refundStatus', Req),
        <<"exclude"                  >> => construct_exclude(Req)
    },
    Opts = #{
        %% TODO no special fun for refunds so we can use any
        %% should be fixed in new magista
        thrift_fun => 'GetPayments',
        decode_fun => fun decode_stat_refund/2
    },
    process_search_request(refunds, Query, Req, Context, Opts);

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

process_search_request(QueryType, Query, Req, Context, Opts = #{thrift_fun := ThriftFun}) ->
    QueryParams = #{
        <<"size">> => genlib_map:get('limit', Req),
        <<"from">> => genlib_map:get('offset', Req)
    },
    ContinuationToken = genlib_map:get('continuationToken', Req),
    Call = {
        merchant_stat,
        ThriftFun,
        [
            anapi_handler_encoder:encode_stat_request(
                anapi_handler_utils:create_dsl(QueryType, Query, QueryParams),
                ContinuationToken
            )
        ]
    },
    process_search_request_result(QueryType, anapi_handler_utils:service_call(Call, Context), Context, Opts).

process_search_request_result(QueryType, Result, Context, #{decode_fun := DecodeFun}) ->
    case Result of
        {ok, #merchstat_StatResponse{
            data = {QueryType, Data},
            total_count = TotalCount,
            continuation_token = ContinuationToken
        }} ->
            DecodedData = [DecodeFun(D, Context) || D <- Data],
            Resp = genlib_map:compact(#{
                <<"result">> => DecodedData,
                <<"totalCount">> => TotalCount,
                <<"continuationToken">> => ContinuationToken
            }),
            {ok, {200, #{}, Resp}};
        {exception, #'InvalidRequest'{errors = Errors}} ->
            FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
            {ok, logic_error(invalidRequest, FormattedErrors)};
        {exception, #merchstat_BadToken{}} ->
            {ok, logic_error(invalidRequest, <<"Invalid token">>)}
    end.

%%

encode_payment_method('bankCard'       ) -> <<"bank_card">>;
encode_payment_method('paymentTerminal') -> <<"payment_terminal">>;
encode_payment_method(undefined        ) -> undefined.

encode_payout_type('PayoutAccount') -> <<"bank_account">>;
encode_payout_type('Wallet'       ) -> <<"wallet">>;
encode_payout_type(undefined      ) -> undefined.

%%

decode_stat_invoice(Invoice, _Context) ->
    anapi_handler_utils:merge_and_compact(#{
        <<"id"         >> => Invoice#merchstat_StatInvoice.id,
        <<"shopID"     >> => Invoice#merchstat_StatInvoice.shop_id,
        <<"createdAt"  >> => Invoice#merchstat_StatInvoice.created_at,
        <<"dueDate"    >> => Invoice#merchstat_StatInvoice.due,
        <<"amount"     >> => Invoice#merchstat_StatInvoice.amount,
        <<"currency"   >> => Invoice#merchstat_StatInvoice.currency_symbolic_code,
        <<"metadata"   >> => anapi_handler_decoder_utils:decode_context(Invoice#merchstat_StatInvoice.context),
        <<"product"    >> => Invoice#merchstat_StatInvoice.product,
        <<"description">> => Invoice#merchstat_StatInvoice.description,
        <<"cart"       >> => anapi_handler_decoder_invoicing:decode_invoice_cart(Invoice#merchstat_StatInvoice.cart)
    }, decode_stat_invoice_status(Invoice#merchstat_StatInvoice.status)).

decode_stat_invoice_status({Status, StatusInfo}) ->
    Reason =
        case StatusInfo of
            #merchstat_InvoiceCancelled{details = Details} -> Details;
            #merchstat_InvoiceFulfilled{details = Details} -> Details;
            _ -> undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"reason">> => Reason
    }.

decode_stat_payment(Stat, Context) ->
    anapi_handler_utils:merge_and_compact(#{
        <<"id"             >> => Stat#merchstat_StatPayment.id,
        <<"shortID"        >> => Stat#merchstat_StatPayment.short_id,
        <<"invoiceID"      >> => Stat#merchstat_StatPayment.invoice_id,
        <<"shopID"         >> => Stat#merchstat_StatPayment.shop_id,
        <<"createdAt"      >> => Stat#merchstat_StatPayment.created_at,
        <<"amount"         >> => Stat#merchstat_StatPayment.amount,
        <<"flow"           >> => decode_stat_payment_flow(Stat#merchstat_StatPayment.flow),
        <<"fee"            >> => Stat#merchstat_StatPayment.fee,
        <<"currency"       >> => Stat#merchstat_StatPayment.currency_symbolic_code,
        <<"payer"          >> => decode_stat_payer(Stat#merchstat_StatPayment.payer),
        <<"geoLocationInfo">> => decode_geo_location_info(Stat#merchstat_StatPayment.location_info),
        <<"metadata"       >> => anapi_handler_decoder_utils:decode_context(Stat#merchstat_StatPayment.context),
        <<"transactionInfo">> => decode_stat_tx_info(Stat#merchstat_StatPayment.additional_transaction_info),
        <<"makeRecurrent"  >> => anapi_handler_decoder_invoicing:decode_make_recurrent(
            Stat#merchstat_StatPayment.make_recurrent
        ),
        <<"statusChangedAt">> => decode_status_changed_at(Stat#merchstat_StatPayment.status)
    }, decode_stat_payment_status(Stat#merchstat_StatPayment.status, Context)).

decode_stat_tx_info(undefined) ->
    undefined;
decode_stat_tx_info(TransactionInfo) ->
    RRN = TransactionInfo#domain_AdditionalTransactionInfo.rrn,
    AAC = TransactionInfo#domain_AdditionalTransactionInfo.approval_code,
    ParsedTransactionInfo = #{
        <<"rrn"         >> => RRN,
        <<"approvalCode">> => AAC
    },
    genlib_map:compact(ParsedTransactionInfo).

decode_stat_payer({customer, #merchstat_CustomerPayer{customer_id = ID, payment_tool = PaymentTool}}) ->
    #{
        <<"payerType" >> => <<"CustomerPayer">>,
        <<"customerID">> => ID,
        <<"paymentToolToken">> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool)
    };
decode_stat_payer({recurrent, RecurrentPayer}) ->
    #merchstat_RecurrentPayer{
        recurrent_parent = RecurrentParent,
        phone_number = PhoneNumber,
        email = Email,
        payment_tool = PaymentTool
    } = RecurrentPayer,
    #{
        <<"payerType">> => <<"RecurrentPayer">>,
        <<"contactInfo">> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email"      >> => Email
        }),
        <<"recurrentParentPayment">> => anapi_handler_decoder_invoicing:decode_recurrent_parent(RecurrentParent),
        <<"paymentToolToken">> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool)
    };
decode_stat_payer({payment_resource, PaymentResource}) ->
    #merchstat_PaymentResourcePayer{
        payment_tool = PaymentTool,
        session_id = PaymentSession,
        fingerprint = Fingerprint,
        ip_address = IP,
        phone_number = PhoneNumber,
        email = Email
    } = PaymentResource,
    genlib_map:compact(#{
        <<"payerType"         >> => <<"PaymentResourcePayer">>,
        <<"paymentToolToken"  >> => decode_stat_payment_tool_token(PaymentTool),
        <<"paymentSession"    >> => PaymentSession,
        <<"paymentToolDetails">> => decode_stat_payment_tool_details(PaymentTool),
        <<"clientInfo"        >> => genlib_map:compact(#{
            <<"ip"         >> => IP,
            <<"fingerprint">> => Fingerprint
        }),
        <<"contactInfo"       >> => genlib_map:compact(#{
            <<"phoneNumber">> => PhoneNumber,
            <<"email"      >> => Email
        })
    }).

decode_stat_payment_flow({instant, _}) ->
    #{<<"type">> => <<"PaymentFlowInstant">>};

decode_stat_payment_flow({hold, #merchstat_InvoicePaymentFlowHold{
    on_hold_expiration = OnHoldExpiration,
    held_until = HeldUntil
}}) ->
    #{
        <<"type"            >> => <<"PaymentFlowHold">>,
        <<"onHoldExpiration">> => atom_to_binary(OnHoldExpiration, utf8),
        <<"heldUntil"       >> => HeldUntil
    }.

decode_stat_payment_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #merchstat_InvoicePaymentFailed{failure = OperationFailure} ->
                anapi_handler_decoder_invoicing:decode_payment_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

decode_stat_payment_tool_token({bank_card, BankCard}) ->
    decode_bank_card(BankCard);
decode_stat_payment_tool_token({payment_terminal, PaymentTerminal}) ->
    decode_payment_terminal(PaymentTerminal);
decode_stat_payment_tool_token({digital_wallet, DigitalWallet}) ->
    decode_digital_wallet(DigitalWallet).

decode_bank_card(#merchstat_BankCard{
    'token'          = Token,
    'payment_system' = PaymentSystem,
    'bin'            = Bin,
    'masked_pan'     = MaskedPan,
    'token_provider' = TokenProvider
}) ->
    anapi_utils:map_to_base64url(genlib_map:compact(#{
        <<"type"          >> => <<"bank_card">>,
        <<"token"         >> => Token,
        <<"payment_system">> => PaymentSystem,
        <<"bin"           >> => Bin,
        <<"masked_pan"    >> => MaskedPan,
        <<"token_provider">> => TokenProvider,
        <<"issuer_country">> => undefined,
        <<"bank_name"     >> => undefined,
        <<"metadata"      >> => undefined
    })).

decode_payment_terminal(#merchstat_PaymentTerminal{
    terminal_type = Type
}) ->
    anapi_utils:map_to_base64url(#{
        <<"type"         >> => <<"payment_terminal">>,
        <<"terminal_type">> => Type
    }).

decode_digital_wallet(#merchstat_DigitalWallet{
    provider = Provider,
    id = ID
}) ->
    anapi_utils:map_to_base64url(#{
        <<"type"    >> => <<"digital_wallet">>,
        <<"provider">> => atom_to_binary(Provider, utf8),
        <<"id"      >> => ID
    }).

decode_stat_payment_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsBankCard">>});
decode_stat_payment_tool_details({payment_terminal, V}) ->
    decode_payment_terminal_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsPaymentTerminal">>});
decode_stat_payment_tool_details({digital_wallet, V}) ->
    decode_digital_wallet_details(V, #{<<"detailsType">> => <<"PaymentToolDetailsDigitalWallet">>}).

decode_bank_card_details(BankCard, V) ->
    LastDigits = anapi_handler_decoder_utils:decode_last_digits(BankCard#merchstat_BankCard.masked_pan),
    Bin = BankCard#merchstat_BankCard.bin,
    anapi_handler_utils:merge_and_compact(V, #{
        <<"lastDigits">>     => LastDigits,
        <<"bin">>            => Bin,
        <<"cardNumberMask">> => anapi_handler_decoder_utils:decode_masked_pan(Bin, LastDigits),
        <<"paymentSystem" >> => genlib:to_binary(BankCard#merchstat_BankCard.payment_system),
        <<"tokenProvider" >> => decode_token_provider(BankCard#merchstat_BankCard.token_provider)
    }).

decode_token_provider(Provider) when Provider /= undefined ->
    genlib:to_binary(Provider);
decode_token_provider(undefined) ->
    undefined.

decode_payment_terminal_details(#merchstat_PaymentTerminal{terminal_type = Type}, V) ->
    V#{
        <<"provider">> => genlib:to_binary(Type)
    }.

decode_digital_wallet_details(#merchstat_DigitalWallet{provider = qiwi, id = ID}, V) ->
    V#{
        <<"digitalWalletDetailsType">> => <<"DigitalWalletDetailsQIWI">>,
        <<"phoneNumberMask"         >> => mask_phone_number(ID)
    }.

mask_phone_number(PhoneNumber) ->
    anapi_utils:redact(PhoneNumber, <<"^\\+\\d(\\d{1,10}?)\\d{2,4}$">>).

decode_geo_location_info(#geo_ip_LocationInfo{city_geo_id = CityID, country_geo_id = CountryID}) ->
    #{
        <<"cityGeoID">> => CityID,
        <<"countryGeoID">> => CountryID
    };
decode_geo_location_info(undefined) ->
    undefined.

decode_status_changed_at({_, #merchstat_InvoicePaymentPending{}}) ->
    undefined;
decode_status_changed_at({_, #merchstat_InvoicePaymentProcessed{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentCaptured{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentCancelled{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentRefunded{at = ChangedAt}}) ->
    ChangedAt;
decode_status_changed_at({_, #merchstat_InvoicePaymentFailed{at = ChangedAt}}) ->
    ChangedAt.

decode_stat_payout(Payout, _Context) ->
    anapi_handler_utils:merge_and_compact(#{
        <<"id"               >> => Payout#merchstat_StatPayout.id,
        <<"shopID"           >> => Payout#merchstat_StatPayout.shop_id,
        <<"createdAt"        >> => Payout#merchstat_StatPayout.created_at,
        <<"amount"           >> => Payout#merchstat_StatPayout.amount,
        <<"fee"              >> => Payout#merchstat_StatPayout.fee,
        <<"currency"         >> => Payout#merchstat_StatPayout.currency_symbolic_code,
        <<"payoutToolDetails">> => decode_stat_payout_tool_details(Payout#merchstat_StatPayout.type),
        <<"payoutSummary"    >> => decode_stat_payout_summary(Payout#merchstat_StatPayout.summary)
    }, decode_stat_payout_status(Payout#merchstat_StatPayout.status)).

decode_stat_payout_status({cancelled, #merchstat_PayoutCancelled{details = Details}}) ->
    #{
        <<"status"             >> => <<"cancelled">>,
        <<"cancellationDetails">> => genlib:to_binary(Details)
    };
decode_stat_payout_status({Status, _}) ->
    #{
        <<"status">> => genlib:to_binary(Status)
    }.

decode_stat_payout_tool_details({bank_card, #merchstat_PayoutCard{card = BankCard}}) ->
    decode_stat_payout_tool_details({bank_card, BankCard});
decode_stat_payout_tool_details({bank_account, {russian_payout_account, PayoutAccount}}) ->
    #merchstat_RussianPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    decode_stat_payout_tool_details({russian_bank_account, BankAccount});
decode_stat_payout_tool_details({bank_account, {international_payout_account, PayoutAccount}}) ->
    #merchstat_InternationalPayoutAccount{bank_account = BankAccount} = PayoutAccount,
    decode_stat_payout_tool_details({international_bank_account, BankAccount});

decode_stat_payout_tool_details({bank_card, V}) ->
    decode_bank_card_details(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankCard">>});
decode_stat_payout_tool_details({russian_bank_account, V}) ->
    decode_russian_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsBankAccount">>});
decode_stat_payout_tool_details({international_bank_account, V}) ->
    decode_international_bank_account(V, #{<<"detailsType">> => <<"PayoutToolDetailsInternationalBankAccount">>});
decode_stat_payout_tool_details({wallet, V}) ->
    #{
        <<"detailsType">> => <<"PayoutToolDetailsWalletInfo">>,
        <<"walletID">> => V#merchstat_Wallet.wallet_id
    }.

decode_russian_bank_account(BankAccount, V) ->
    V#{
        <<"account"        >> => BankAccount#merchstat_RussianBankAccount.account,
        <<"bankName"       >> => BankAccount#merchstat_RussianBankAccount.bank_name,
        <<"bankPostAccount">> => BankAccount#merchstat_RussianBankAccount.bank_post_account,
        <<"bankBik"        >> => BankAccount#merchstat_RussianBankAccount.bank_bik
    }.

decode_international_bank_account(undefined, _) ->
    undefined;
decode_international_bank_account(BankAccount, V) ->
    genlib_map:compact(V#{
        <<"number">>                   => BankAccount#merchstat_InternationalBankAccount.number,
        <<"iban">>                     => BankAccount#merchstat_InternationalBankAccount.iban,
        <<"bankDetails">>              => decode_international_bank_details(
            BankAccount#merchstat_InternationalBankAccount.bank
        ),
        <<"correspondentBankAccount">> => decode_international_bank_account(
            BankAccount#merchstat_InternationalBankAccount.correspondent_account, #{}
        )
    }).

decode_international_bank_details(undefined) ->
    undefined;
decode_international_bank_details(Bank) ->
    genlib_map:compact(#{
         <<"bic">>         => Bank#merchstat_InternationalBankDetails.bic,
         <<"abartn">>      => Bank#merchstat_InternationalBankDetails.aba_rtn,
         <<"name">>        => Bank#merchstat_InternationalBankDetails.name,
         <<"countryCode">> =>
            anapi_handler_decoder_party:decode_residence(Bank#merchstat_InternationalBankDetails.country),
         <<"address">>     => Bank#merchstat_InternationalBankDetails.address
    }).

decode_stat_payout_summary(PayoutSummary) when is_list(PayoutSummary) ->
    [decode_stat_payout_summary_item(PayoutSummaryItem) || PayoutSummaryItem <- PayoutSummary];
decode_stat_payout_summary(undefined) ->
    undefined.

decode_stat_payout_summary_item(PayoutSummary) ->
    genlib_map:compact(#{
        <<"amount"  >> => PayoutSummary#merchstat_PayoutSummaryItem.amount,
        <<"fee"     >> => PayoutSummary#merchstat_PayoutSummaryItem.fee,
        <<"currency">> => PayoutSummary#merchstat_PayoutSummaryItem.currency_symbolic_code,
        <<"count"   >> => PayoutSummary#merchstat_PayoutSummaryItem.count,
        <<"fromTime">> => PayoutSummary#merchstat_PayoutSummaryItem.from_time,
        <<"toTime"  >> => PayoutSummary#merchstat_PayoutSummaryItem.to_time,
        <<"type"    >> => genlib:to_binary(PayoutSummary#merchstat_PayoutSummaryItem.operation_type)
    }).

decode_stat_refund(Refund, Context) ->
    anapi_handler_utils:merge_and_compact(
        #{
            <<"invoiceID">> => Refund#merchstat_StatRefund.invoice_id,
            <<"paymentID">> => Refund#merchstat_StatRefund.payment_id,
            <<"id">>        => Refund#merchstat_StatRefund.id,
            <<"createdAt">> => Refund#merchstat_StatRefund.created_at,
            <<"amount">>    => Refund#merchstat_StatRefund.amount,
            <<"currency">>  => Refund#merchstat_StatRefund.currency_symbolic_code,
            <<"reason">>    => Refund#merchstat_StatRefund.reason
        },
        decode_stat_refund_status(Refund#merchstat_StatRefund.status, Context)
    ).

decode_stat_refund_status({Status, StatusInfo}, Context) ->
    Error =
        case StatusInfo of
            #merchstat_InvoicePaymentRefundFailed{failure = OperationFailure} ->
                anapi_handler_decoder_utils:decode_operation_failure(OperationFailure, Context);
            _ ->
                undefined
        end,
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"error" >> => Error
    }.

construct_exclude(Req) ->
    % can be extended upon need
    genlib_map:compact(#{
        <<"shop_id">> => genlib_map:get('excludedShops', Req)
    }).
