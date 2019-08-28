-define(STRING, <<"TEST">>).
-define(RUB, <<"RUB">>).
-define(USD, <<"USD">>).
-define(BANKID_RU, <<"PUTIN">>).
-define(BANKID_US, <<"TRAMP">>).
-define(WALLET_TOOL, <<"TOOL">>).
-define(JSON, <<"{}">>).
-define(INTEGER, 10000).
-define(INTEGER_BINARY, <<"10000">>).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).
-define(MD5, <<"033BD94B1168D7E4F0D644C3C95E35BF">>).
-define(SHA256, <<"94EE059335E587E501CC4BF90613E0814F00A7B08BC7C648FD865A2AF6A22CC2">>).

-define(DETAILS, #domain_InvoiceDetails{
        product = ?STRING,
        description = ?STRING
    }).

-define(CASH, #domain_Cash{
    amount = ?INTEGER,
    currency = #domain_CurrencyRef{
        symbolic_code = ?RUB
    }
}).

-define(CONTENT, #'Content'{
    type = <<"application/json">>,
    data = ?JSON
}).

-define(LIFETIME_INTERVAL, #domain_LifetimeInterval{
    years = ?INTEGER,
    months = ?INTEGER,
    days = ?INTEGER
}).

-define(TPL_CASH, {fixed, ?CASH}).

-define(
    INVOICE_STATUS(Status),
    case Status of
        unpaid ->
            {unpaid, #domain_InvoiceUnpaid{}};
        paid ->
            {paid, #domain_InvoicePaid{}};
        cancelled ->
            {cancelled, #domain_InvoiceCancelled{details = ?STRING}};
        fulfilled ->
            {fulfilled, #domain_InvoiceFulfilled{details = ?STRING}}
    end
).

-define(INVOICE, ?INVOICE(?STRING, undefined)).

-define(INVOICE(ID, EID), #domain_Invoice{
    id          = ID,
    created_at  = ?TIMESTAMP,
    status      = ?INVOICE_STATUS(unpaid),
    due         = ?TIMESTAMP,
    details     = ?DETAILS,
    cost        = ?CASH,
    context     = ?CONTENT,
    shop_id     = ?STRING,
    owner_id    = ?STRING,
    template_id = ?STRING,
    external_id = EID
}).

-define(PAYPROC_INVOICE(Payments), #payproc_Invoice{
    invoice = ?INVOICE,
    payments = Payments
}).
-define(PAYPROC_INVOICE, ?PAYPROC_INVOICE([])).

-define(PAYPROC_INVOICE_WITH_ID(ID), #payproc_Invoice{
    invoice  = ?INVOICE(ID, undefined),
    payments = []
}).

-define(PAYPROC_INVOICE_WITH_ID(ID, EID), #payproc_Invoice{
    invoice  = ?INVOICE(ID, EID),
    payments = []
}).

-define(INVOICE_LINE, #domain_InvoiceLine{
    product = ?STRING,
    quantity = ?INTEGER,
    price = ?CASH,
    metadata = #{?STRING => {obj, #{}}}
}).

-define(INVOICE_TPL, #domain_InvoiceTemplate{
    id          = ?STRING,
    details     = {product, #domain_InvoiceTemplateProduct{
        product = ?STRING,
        price = ?TPL_CASH,
        metadata = #{?STRING => {obj, #{}}}
    }},
    product     = ?STRING,
    context     = ?CONTENT,
    shop_id     = ?STRING,
    owner_id    = ?STRING,
    invoice_lifetime = ?LIFETIME_INTERVAL
}).

-define(BANK_CARD, #domain_BankCard{
    token = ?STRING,
    payment_system = visa,
    bin = <<"411111">>,
    masked_pan = <<"411111******1111">>
}).

-define(CONTACT_INFO, #domain_ContactInfo{
        phone_number = ?STRING,
        email = <<"test@test.ru">>
    }).

-define(DISP_PAYMENT_RESOURCE, #domain_DisposablePaymentResource{
    payment_tool = {bank_card, ?BANK_CARD},
    payment_session_id = ?STRING,
    client_info = #domain_ClientInfo{
        fingerprint = ?STRING,
        ip_address = ?STRING
    }
}).

-define(PAYMENT_RESOURCE_PAYER, #domain_PaymentResourcePayer{
    resource = ?DISP_PAYMENT_RESOURCE,
    contact_info = ?CONTACT_INFO
}).

-define(RECURRENT_PAYER, {recurrent, #domain_RecurrentPayer{
    payment_tool = {bank_card, ?BANK_CARD},
    recurrent_parent = #domain_RecurrentParentPayment{
        invoice_id = ?STRING,
        payment_id = ?STRING
    },
    contact_info = ?CONTACT_INFO
}}).

-define(PAYER, {payment_resource, ?PAYMENT_RESOURCE_PAYER}).

-define(PAYMENT(ID, IED, Status), #domain_InvoicePayment{
    id               = ID,
    created_at       = ?TIMESTAMP,
    domain_revision  = ?INTEGER,
    status           = Status,
    payer            = ?PAYER,
    cost             = ?CASH,
    flow             = {instant, #domain_InvoicePaymentFlowInstant{}},
    context          = ?CONTENT,
    make_recurrent   = false,
    external_id      = IED
}).

-define(PAYMENT, ?PAYMENT(?STRING, undefined, {pending, #domain_InvoicePaymentPending{}})).

-define(PAYMENT(ID, IED), ?PAYMENT(ID, IED, {pending, #domain_InvoicePaymentPending{}})).

-define(RECURRENT_PAYMENT(Status), #domain_InvoicePayment{
    id               = ?STRING,
    created_at       = ?TIMESTAMP,
    domain_revision  = ?INTEGER,
    status           = Status,
    payer            = ?RECURRENT_PAYER,
    cost             = ?CASH,
    flow             = {instant, #domain_InvoicePaymentFlowInstant{}},
    context          = ?CONTENT,
    make_recurrent   = true
}).
-define(RECURRENT_PAYMENT, ?RECURRENT_PAYMENT({pending, #domain_InvoicePaymentPending{}})).

-define(PAYPROC_PAYMENT(Payment, Refunds, Adjustments), #payproc_InvoicePayment{
    payment     = Payment,
    refunds     = Refunds,
    adjustments = Adjustments
}).

-define(PAYPROC_PAYMENT, ?PAYPROC_PAYMENT(?PAYMENT, [?REFUND], [?ADJUSTMENT])).

-define(PAYPROC_PAYMENT(ID, IED), ?PAYPROC_PAYMENT(?PAYMENT(ID, IED), [?REFUND], [?ADJUSTMENT])).

-define(FAILED_PAYMENT(Failure), ?PAYMENT(?STRING, ?STRING, {failed, #domain_InvoicePaymentFailed{failure = Failure}})).

-define(PAYPROC_FAILED_PAYMENT(Failure), ?PAYPROC_PAYMENT(?FAILED_PAYMENT(Failure), [], [])).

-define(ACCOUNT_STATE, #payproc_AccountState{
    account_id = ?INTEGER,
    own_amount = ?INTEGER,
    available_amount = ?INTEGER,
    currency = #domain_Currency{
        name = ?STRING,
        symbolic_code = ?RUB,
        numeric_code = ?INTEGER,
        exponent = ?INTEGER
    }
}).

-define(REFUND, #domain_InvoicePaymentRefund{
    id = ?STRING,
    status = {pending, #domain_InvoicePaymentRefundPending{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?STRING,
    cash = ?CASH
}).

-define(CONTRACT, #domain_Contract{
    id = ?STRING,
    contractor = ?CONTRACTOR,
    payment_institution = #domain_PaymentInstitutionRef{id = ?INTEGER},
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    status = {active, #domain_ContractActive{}},
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER},
    adjustments = [?CONTRACT_ADJUSTMENT],
    payout_tools = [
        ?PAYOUT_TOOL(?BANKID_RU, ?RUSSIAN_BANK_ACCOUNT),
        ?PAYOUT_TOOL(?BANKID_US, ?INTERNATIONAL_BANK_ACCOUNT),
        ?PAYOUT_TOOL(?WALLET_TOOL, ?WALLET_INFO)
    ]
}).

-define(CONTRACTOR, {registered_user, #domain_RegisteredUser{email = ?STRING}}).

-define(BLOCKING, {unblocked, #domain_Unblocked{
        reason = ?STRING,
        since = ?TIMESTAMP
}}).

-define(SUSPENTION, {active, #domain_Active{since = ?TIMESTAMP}}).

-define(SHOP, #domain_Shop{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    details = ?SHOP_DETAILS,
    location = ?SHOP_LOCATION,
    category = #domain_CategoryRef{id = ?INTEGER},
    contract_id = ?STRING
}).

-define(SHOP_LOCATION, {url, ?STRING}).

-define(SHOP_DETAILS, #domain_ShopDetails{name = ?STRING}).

-define(PARTY_CONTRACTOR, #domain_PartyContractor{
    id = ?STRING,
    contractor = {private_entity, {russian_private_entity, #domain_RussianPrivateEntity{
        first_name = ?STRING,
        second_name = ?STRING,
        middle_name = ?STRING,
        contact_info = #domain_ContactInfo{}
    }}},
    status = none,
    identity_documents = []
}).

-define(WALLET_CONTRACT_ID, <<"WALLET_CONTRACT_ID">>).

-define(WALLET_CONTRACT, #domain_Contract{
    id = ?WALLET_CONTRACT_ID,
    contractor_id = ?STRING,
    payment_institution = #domain_PaymentInstitutionRef{id = ?INTEGER},
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    status = {active, #domain_ContractActive{}},
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER},
    adjustments = [],
    payout_tools = []
}).

-define(WALLET, #domain_Wallet{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    contract = ?WALLET_CONTRACT_ID
}).

-define(PARTY, #domain_Party{
    id = ?STRING,
    contact_info = #domain_PartyContactInfo{email = ?STRING},
    created_at = ?TIMESTAMP,
    blocking = ?BLOCKING,
    suspension = ?SUSPENTION,
    contracts = #{
        ?STRING => ?CONTRACT,
        ?WALLET_CONTRACT_ID => ?WALLET_CONTRACT
    },
    shops = #{?STRING => ?SHOP},
    contractors = #{?STRING => ?PARTY_CONTRACTOR},
    wallets = #{?STRING => ?WALLET},
    revision = 0
}).

-define(CLAIM(Changeset), #payproc_Claim{
    id = ?INTEGER,
    revision = ?INTEGER,
    created_at = ?TIMESTAMP,
    updated_at = ?TIMESTAMP,
    status = {pending, #payproc_ClaimPending{}},
    changeset = Changeset
}).

-define(CLAIM_CHANGESET, [
    %% contract modifications
    {contract_modification, #payproc_ContractModificationUnit{
        id = ?STRING,
        modification = {creation, #payproc_ContractParams{
            contractor = ?CONTRACTOR,
            payment_institution = #domain_PaymentInstitutionRef{id = ?INTEGER}
        }}
    }},
    {contract_modification, #payproc_ContractModificationUnit{
        id = ?STRING,
        modification = {termination, #payproc_ContractTermination{
            reason = ?STRING
        }}
    }},
    {contract_modification, #payproc_ContractModificationUnit{
        id = ?STRING,
        modification = {adjustment_modification, #payproc_ContractAdjustmentModificationUnit{
            adjustment_id = ?STRING,
            modification = {creation, #payproc_ContractAdjustmentParams{
                template = #domain_ContractTemplateRef{id = ?INTEGER}
            }}
        }}
    }},
    {contract_modification, #payproc_ContractModificationUnit{
        id = ?STRING,
        modification = {payout_tool_modification, #payproc_PayoutToolModificationUnit{
            payout_tool_id = ?STRING,
            modification = {creation, #payproc_PayoutToolParams{
                currency = #domain_CurrencyRef{symbolic_code = ?RUB},
                tool_info = ?RUSSIAN_BANK_ACCOUNT
            }}
        }}
    }},
    {contract_modification, #payproc_ContractModificationUnit{
        id = ?STRING,
        modification = {legal_agreement_binding, #domain_LegalAgreement{
            signed_at = ?TIMESTAMP,
            legal_agreement_id = ?STRING,
            valid_until = ?TIMESTAMP
        }}
    }},
    {contract_modification, #payproc_ContractModificationUnit{
        id = ?STRING,
        modification = {report_preferences_modification, #domain_ReportPreferences{
            service_acceptance_act_preferences = #domain_ServiceAcceptanceActPreferences{
                schedule = #domain_BusinessScheduleRef{id = ?INTEGER},
                signer = #domain_Representative{
                    position  = ?STRING,
                    full_name = ?STRING,
                    document  = {articles_of_association, #domain_ArticlesOfAssociation{}}
                }
            }
        }}
    }},
    %% shop modifications
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {creation, #payproc_ShopParams{
            location =  ?SHOP_LOCATION,
            details = ?SHOP_DETAILS,
            contract_id = ?STRING,
            payout_tool_id = ?STRING
        }}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {category_modification, #domain_CategoryRef{id = ?INTEGER}}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {details_modification, ?SHOP_DETAILS}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {contract_modification, #payproc_ShopContractModification{
            contract_id = ?STRING,
            payout_tool_id = ?STRING
        }}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {payout_tool_modification, ?STRING}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {location_modification, ?SHOP_LOCATION}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {shop_account_creation, #payproc_ShopAccountParams{
            currency = #domain_CurrencyRef{symbolic_code = ?RUB}
        }}
    }},
    {shop_modification, #payproc_ShopModificationUnit{
        id = ?STRING,
        modification = {payout_schedule_modification, #payproc_ScheduleModification{
            schedule = #domain_BusinessScheduleRef{id = ?INTEGER}
        }}
    }}
]).

-define(CONTRACTOR_CLAIM_CHANGESET, [
    %% contractor modifications
    {contractor_modification, #payproc_ContractorModificationUnit{
        id = ?STRING,
        modification = {creation, ?CONTRACTOR}
    }},
    {contractor_modification, #payproc_ContractorModificationUnit{
        id = ?STRING,
        modification = {identification_level_modification, partial}
    }},
    {contractor_modification, #payproc_ContractorModificationUnit{
        id = ?STRING,
        modification = {identity_documents_modification, #payproc_ContractorIdentityDocumentsModification{
            identity_documents = []
        }}
    }}
]).

-define(WALLET_CLAIM_CHANGESET, [
    %% wallet modifications
    {wallet_modification, #payproc_WalletModificationUnit{
        id = ?STRING,
        modification = {creation, #payproc_WalletParams{
            name = ?STRING,
            contract_id = ?WALLET_CONTRACT_ID
        }}
    }}
]).

-define(ADJUSTMENT, #domain_InvoicePaymentAdjustment{
    id = ?STRING,
    status = {pending, #domain_InvoicePaymentAdjustmentPending{}},
    created_at = ?TIMESTAMP,
    domain_revision = ?INTEGER,
    reason = ?STRING,
    new_cash_flow = [],
    old_cash_flow_inverse = []
}).

-define(CONTRACT_ADJUSTMENT, #domain_ContractAdjustment{
    id = ?STRING,
    created_at = ?TIMESTAMP,
    valid_since = ?TIMESTAMP,
    valid_until = ?TIMESTAMP,
    terms = #domain_TermSetHierarchyRef{id = ?INTEGER}
}).

-define(PAYOUT_TOOL(ID, ToolInfo), #domain_PayoutTool{
    id = ID,
    created_at = ?TIMESTAMP,
    currency = #domain_CurrencyRef{symbolic_code = ?RUB},
    payout_tool_info = ToolInfo
}).

-define(RUSSIAN_BANK_ACCOUNT, {russian_bank_account, #domain_RussianBankAccount{
    account = <<"12345678901234567890">>,
    bank_name = ?STRING,
    bank_post_account = <<"12345678901234567890">>,
    bank_bik = <<"123456789">>
}}).

-define(INTERNATIONAL_BANK_ACCOUNT, {international_bank_account, #domain_InternationalBankAccount{
    number = <<"12345678901234567890">>,
    bank = ?INTERNATIONAL_BANK_DETAILS,
    correspondent_account = #domain_InternationalBankAccount{number = <<"00000000000000000000">>},
    iban = <<"GR1601101250000000012300695">>,
    account_holder = ?STRING
}}).

-define(INTERNATIONAL_BANK_DETAILS, #domain_InternationalBankDetails{
    %% In reality either bic or aba_rtn should be used, not both.
    bic = <<"DEUTDEFF500">>,
    country = usa,
    name = ?STRING,
    address = ?STRING,
    aba_rtn = <<"129131673">>
}).

-define(WALLET_INFO, {wallet_info, #domain_WalletInfo{
    wallet_id = ?STRING
}}).

-define(WEBHOOK, #webhooker_Webhook{
    id = ?INTEGER,
    party_id = ?STRING,
    event_filter = {invoice, #webhooker_InvoiceEventFilter{
        shop_id = ?STRING,
        types = ordsets:from_list([
            {created,
             #webhooker_InvoiceCreated{}}

          , {status_changed,
             #webhooker_InvoiceStatusChanged{
                value = {
                    paid,
                    #webhooker_InvoicePaid{}}}}
          , {status_changed,
             #webhooker_InvoiceStatusChanged{
                value = {
                    cancelled,
                    #webhooker_InvoiceCancelled{}}}}
          , {status_changed,
             #webhooker_InvoiceStatusChanged{
                value = {
                    fulfilled,
                    #webhooker_InvoiceFulfilled{}}}}

          , {payment, {
               created,
               #webhooker_InvoicePaymentCreated{}}}
          , {payment, {
               status_changed,
               #webhooker_InvoicePaymentStatusChanged{
                  value = {
                      processed,
                      #webhooker_InvoicePaymentProcessed{}}}}}
          , {payment, {
               status_changed,
               #webhooker_InvoicePaymentStatusChanged{
                  value = {
                      captured,
                      #webhooker_InvoicePaymentCaptured{}}}}}
          , {payment, {
               status_changed,
               #webhooker_InvoicePaymentStatusChanged{
                  value = {
                      cancelled,
                      #webhooker_InvoicePaymentCancelled{}}}}}
          , {payment, {
               status_changed,
               #webhooker_InvoicePaymentStatusChanged{
                  value = {
                      refunded,
                      #webhooker_InvoicePaymentRefunded{}}}}}
          , {payment, {
               status_changed,
               #webhooker_InvoicePaymentStatusChanged{
                  value = {
                      failed,
                      #webhooker_InvoicePaymentFailed{}}}}}
          , {payment, {
               invoice_payment_refund_change, {
                   invoice_payment_refund_created,
                   #webhooker_InvoicePaymentRefundCreated{}}}}
          , {payment, {
               invoice_payment_refund_change, {
                   invoice_payment_refund_status_changed,
                   #webhooker_InvoicePaymentRefundStatusChanged{
                      value = {
                          failed,
                          #webhooker_InvoicePaymentRefundFailed{}}}}}}
          , {payment, {
               invoice_payment_refund_change, {
                   invoice_payment_refund_status_changed,
                   #webhooker_InvoicePaymentRefundStatusChanged{
                      value = {
                          succeeded,
                          #webhooker_InvoicePaymentRefundSucceeded{}}}}}}
          ])
    }},
    url = ?STRING,
    pub_key = ?STRING,
    enabled = true
}).

-define (STAT_RESPONSE(Data), #merchstat_StatResponse{
    data = Data,
    total_count = ?INTEGER,
    continuation_token = ?STRING
}).

-define(STAT_RESPONSE_INVOICES, ?STAT_RESPONSE({invoices, [?STAT_INVOICE]})).

-define(STAT_RESPONSE_PAYMENTS, ?STAT_RESPONSE({payments,
    [
        ?STAT_PAYMENT(?STAT_PAYER({bank_card, ?STAT_BANK_CARD}), ?STAT_PAYMENT_STATUS_CAPTURED),
        ?STAT_PAYMENT(?STAT_PAYER({bank_card, ?STAT_BANK_CARD_WITH_TP}), ?STAT_PAYMENT_STATUS_PENDING),
        ?STAT_PAYMENT(?STAT_PAYER({bank_card, ?STAT_BANK_CARD}, undefined), ?STAT_PAYMENT_STATUS_CAPTURED)
    ]
})).

-define(STAT_RESPONSE_RECORDS, ?STAT_RESPONSE({records, [?STAT_RECORD]})).

-define(STAT_RESPONSE_REFUNDS, ?STAT_RESPONSE({refunds, [?STAT_REFUND]})).

-define(STAT_RESPONSE_PAYOUTS, ?STAT_RESPONSE({payouts,
    [
        ?STAT_PAYOUT({wallet, #merchstat_Wallet{wallet_id = ?STRING}}, []),
        ?STAT_PAYOUT({bank_card, #merchstat_PayoutCard{card = ?STAT_BANK_CARD}}, [?PAYOUT_SUMMARY_ITEM]),
        ?STAT_PAYOUT({bank_card, #merchstat_PayoutCard{card = ?STAT_BANK_CARD_WITH_TP}}, [?PAYOUT_SUMMARY_ITEM]),
        ?STAT_PAYOUT({bank_account, ?STAT_PAYOUT_BANK_ACCOUNT_RUS}, undefined),
        ?STAT_PAYOUT({bank_account, ?STAT_PAYOUT_BANK_ACCOUNT_INT}, [?PAYOUT_SUMMARY_ITEM])
    ]
})).

-define(STAT_INVOICE, #merchstat_StatInvoice{
    id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {unpaid, #merchstat_InvoiceUnpaid{}},
    product = ?STRING,
    description = ?STRING,
    due  = ?TIMESTAMP,
    amount = ?INTEGER,
    currency_symbolic_code = ?RUB,
    context = ?CONTENT
}).

-define(STAT_PAYMENT(Payer, Status), #merchstat_StatPayment{
    id = ?STRING,
    invoice_id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = Status,
    amount = ?INTEGER,
    flow = {instant, #merchstat_InvoicePaymentFlowInstant{}},
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    payer = Payer,
    context = ?CONTENT,
    domain_revision = ?INTEGER,
    additional_transaction_info = ?TX_INFO
}).

-define(TX_INFO, #domain_AdditionalTransactionInfo{
    rrn = <<"090909090909">>,
    approval_code = <<"808080">>
}).

-define (STAT_PAYER(PaymentTool), ?STAT_PAYER(PaymentTool, ?STRING)).

-define (STAT_PAYER(PaymentTool, SessionId), {payment_resource, #merchstat_PaymentResourcePayer{
    payment_tool = PaymentTool,
    ip_address = ?STRING,
    fingerprint = ?STRING,
    phone_number = ?STRING,
    email = <<"test@test.ru">>,
    session_id = SessionId
}}).

-define(STAT_PAYMENT_STATUS_PENDING, {pending, #merchstat_InvoicePaymentPending{}}).

-define(STAT_PAYMENT_STATUS_CAPTURED, {captured, #merchstat_InvoicePaymentCaptured{at = ?TIMESTAMP}}).

-define(STAT_RECORD, #{
    <<"offset">> => ?INTEGER_BINARY,
    <<"successful_count">> => ?INTEGER_BINARY,
    <<"total_count">> => ?INTEGER_BINARY,
    <<"conversion">> => ?INTEGER_BINARY,
    <<"city_id">> => ?INTEGER_BINARY,
    <<"currency_symbolic_code">> => ?RUB,
    <<"amount_with_fee">> => ?INTEGER_BINARY,
    <<"amount_without_fee">> => ?INTEGER_BINARY,
    <<"unic_count">> => ?INTEGER_BINARY,
    <<"payment_system">> => <<"visa">>
}).

-define(STAT_REFUND, #merchstat_StatRefund{
    id = ?STRING,
    payment_id = ?STRING,
    invoice_id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    status = {succeeded, #merchstat_InvoicePaymentRefundSucceeded{at = ?TIMESTAMP}},
    created_at = ?TIMESTAMP,
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB

}).

-define(STAT_PAYOUT(Type, PayoutSummary), #merchstat_StatPayout{
    id = ?STRING,
    party_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {paid, #merchstat_PayoutPaid{}},
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    type = Type,
    summary = PayoutSummary
}).

-define(STAT_PAYOUT_BANK_ACCOUNT_RUS, {russian_payout_account, #merchstat_RussianPayoutAccount{
    bank_account = #merchstat_RussianBankAccount{
        account = <<"12345678901234567890">>,
        bank_name = ?STRING,
        bank_post_account = <<"12345678901234567890">>,
        bank_bik = <<"123456789">>
    },
    inn = ?STRING,
    purpose = ?STRING
}}).

-define(STAT_PAYOUT_BANK_ACCOUNT_INT, {international_payout_account, #merchstat_InternationalPayoutAccount{
    bank_account = #merchstat_InternationalBankAccount{
        number = <<"12345678901234567890">>,
        bank = ?STAT_PAYOUT_BANK_DETAILS_INT,
        correspondent_account = #merchstat_InternationalBankAccount{number = <<"00000000000000000000">>},
        iban = <<"GR1601101250000000012300695">>,
        account_holder = ?STRING
    },
    purpose = ?STRING
}}).

-define(STAT_PAYOUT_BANK_DETAILS_INT, #merchstat_InternationalBankDetails{
    %% In reality either bic or aba_rtn should be used, not both.
    bic = <<"DEUTDEFF500">>,
    country = usa,
    name = ?STRING,
    address = ?STRING,
    aba_rtn = <<"129131673">>
}).

-define(STAT_BANK_CARD, #merchstat_BankCard{
    token = ?STRING,
    payment_system = visa,
    bin = <<"411111">>,
    masked_pan = <<"411111******1111">>

}).

-define(STAT_BANK_CARD_WITH_TP, #merchstat_BankCard{
    token = ?STRING,
    payment_system = visa,
    bin = <<"411111">>,
    masked_pan = <<"411111******1111">>,
    token_provider = applepay
}).

-define(PAYOUT_SUMMARY_ITEM, #merchstat_PayoutSummaryItem{
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    from_time = ?TIMESTAMP,
    to_time = ?TIMESTAMP,
    operation_type = payment,
    count = ?INTEGER
}).

-define(REPORT_TYPE, <<"paymentRegistry">>).

-define(REPORT, #reports_Report{
    report_id = ?INTEGER,
    time_range = #reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = provision_of_service,
    status = created,
    files = [
        #reports_FileMeta{
            file_id = ?STRING,
            filename = ?STRING,
            signature = #reports_Signature{
                md5 = ?MD5,
                sha256 = ?SHA256
            }
        }
    ]
}).

-define(SNAPSHOT, #'Snapshot'{
    version = ?INTEGER,
    domain = #{
        {category, #domain_CategoryRef{id = ?INTEGER}} =>
        {category, #domain_CategoryObject{
            ref = #domain_CategoryRef{id = ?INTEGER},
            data = #domain_Category{
                name = ?STRING,
                description = ?STRING
            }
        }},
        {business_schedule, #domain_BusinessScheduleRef{id = ?INTEGER}} =>
        {business_schedule, #domain_BusinessScheduleObject{
            ref = #domain_BusinessScheduleRef{id = ?INTEGER},
            data = #domain_BusinessSchedule{
                name = ?STRING,
                description = ?STRING,
                schedule = #'Schedule'{
                    year = {every, #'ScheduleEvery'{}},
                    month = {every, #'ScheduleEvery'{}},
                    day_of_month = {every, #'ScheduleEvery'{}},
                    day_of_week = {every, #'ScheduleEvery'{}},
                    hour = {every, #'ScheduleEvery'{}},
                    minute = {every, #'ScheduleEvery'{}},
                    second = {every, #'ScheduleEvery'{}}
                },
                delay = #'TimeSpan'{},
                policy = #domain_PayoutCompilationPolicy{
                    assets_freeze_for = #'TimeSpan'{}
                }
            }
        }},
        {globals, #domain_GlobalsRef{}} =>
        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, #domain_ExternalAccountSetRef{id = ?INTEGER}},
                payment_institutions = [#domain_PaymentInstitutionRef{id = ?INTEGER}]
            }
        }},
        {payment_institution, #domain_PaymentInstitutionRef{id = ?INTEGER}} =>
        {payment_institution, #domain_PaymentInstitutionObject{
            ref = #domain_PaymentInstitutionRef{id = ?INTEGER},
            data = #domain_PaymentInstitution{
                name = ?STRING,
                description = ?STRING,
                system_account_set = {value, #domain_SystemAccountSetRef{id = ?INTEGER}},
                default_contract_template = {value, #domain_ContractTemplateRef{id = ?INTEGER}},
                providers = {value, []},
                inspector = {value, #domain_InspectorRef{id = ?INTEGER}},
                realm = test,
                residences = [rus]
            }
        }}
    }
}).

-define(INVOICE_EVENT(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    payload =  {invoice_changes,
        [
            {invoice_created, #payproc_InvoiceCreated{invoice = ?INVOICE}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(unpaid)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(paid)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(cancelled)}},
            {invoice_status_changed, #payproc_InvoiceStatusChanged{status = ?INVOICE_STATUS(fulfilled)}}
        ]
    },
    source =  {invoice_id, ?STRING}
}).

-define(INVOICE_EVENT_PRIVATE(ID), #payproc_Event{
    id = ID,
    created_at = ?TIMESTAMP,
    payload =  {invoice_changes,
        [
            {invoice_payment_change, #payproc_InvoicePaymentChange{
                id = <<"1">>,
                payload = {invoice_payment_session_change, #payproc_InvoicePaymentSessionChange{
                    target = {processed, #domain_InvoicePaymentProcessed{}},
                    payload = {session_started, #payproc_SessionStarted{}}}
                }}
            }
        ]
    },
    source =  {invoice_id, ?STRING}
}).

-define(TERM_SET, #domain_TermSet{
    payouts = ?PAYOUTS_SERVICE_TERMS,
    payments = ?PAYMENTS_SERVICE_TERMS
}).

-define(PAYOUTS_SERVICE_TERMS, #domain_PayoutsServiceTerms{}).

-define(PAYMENTS_SERVICE_TERMS, #domain_PaymentsServiceTerms{
    payment_methods = {value,
        [
            #domain_PaymentMethodRef{
                id = {bank_card, mastercard}
            },
            #domain_PaymentMethodRef{
                id = {bank_card, visa}
            },
            #domain_PaymentMethodRef{
                id = {tokenized_bank_card, #domain_TokenizedBankCard{
                    payment_system = mastercard,
                    token_provider = applepay
                }}
            },
            #domain_PaymentMethodRef{
                id = {tokenized_bank_card, #domain_TokenizedBankCard{
                    payment_system = visa,
                    token_provider = applepay
                }}
            }
        ]
    }
}).

-define(CUSTOMER, #payproc_Customer{
    id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    status = {ready, #payproc_CustomerReady{}},
    created_at = ?TIMESTAMP,
    bindings = [?CUSTOMER_BINDING],
    contact_info = ?CONTACT_INFO,
    metadata = {obj, #{}}
}).

-define(CUSTOMER_BINDING, #payproc_CustomerBinding{
    id = ?STRING,
    rec_payment_tool_id = ?STRING,
    payment_resource = ?DISP_PAYMENT_RESOURCE,
    status = {succeeded, #payproc_CustomerBindingSucceeded{}}
}).

-define(PUT_CARD_RESULT, #'PutCardResult'{
    bank_card = ?BANK_CARD
}).

-define(UNWRAPPED_PAYMENT_TOOL(Details),
    ?UNWRAPPED_PAYMENT_TOOL(
        Details,
        {tokenized_card, #paytoolprv_TokenizedCard{
            dpan = ?STRING,
            exp_date = #paytoolprv_ExpDate{
                month = 10,
                year = 2018
            },
            auth_data = {auth_3ds, #paytoolprv_Auth3DS{
                cryptogram = ?STRING,
                eci = ?STRING
            }}
        }}
    )
).

-define(UNWRAPPED_PAYMENT_TOOL(Details, PaymentData), #paytoolprv_UnwrappedPaymentTool{
    payment_data = PaymentData,
    card_info = #paytoolprv_CardInfo{
        display_name = <<"Visa 1234">>,
        cardholder_name = ?STRING,
        last_4_digits = <<"1234">>,
        card_class = debit,
        payment_system = mastercard
    },
    details = Details
}).

-define(APPLE_PAY_DETAILS, {apple, #paytoolprv_ApplePayDetails{
    transaction_id = ?STRING,
    amount = ?INTEGER,
    currency_numeric_code = 643,
    device_id = ?STRING
}}).

-define(GOOGLE_PAY_DETAILS, {google, #paytoolprv_GooglePayDetails{
    message_id = ?STRING,
    message_expiration = ?TIMESTAMP
}}).

-define(BINBASE_LOOKUP_RESULT, ?BINBASE_LOOKUP_RESULT(<<"MASTERCARD">>)).

-define(BINBASE_LOOKUP_RESULT(PaymentSystem), #'binbase_ResponseData'{
    bin_data = #'binbase_BinData' {
        payment_system = PaymentSystem,
        bank_name = ?STRING,
        iso_country_code = <<"KAZ">>,
        card_type = debit
    },
    version = ?INTEGER
}).

-define(PAYOUT(Type, PayoutSummary), #'payout_processing_Payout'{
    id = ?STRING,
    party_id = ?STRING,
    shop_id = ?STRING,
    contract_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {paid, #'payout_processing_PayoutPaid'{}},
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency = #domain_CurrencyRef{
        symbolic_code = ?RUB
    },
    payout_flow = [],
    type = Type,
    summary = PayoutSummary,
    metadata = #{
        <<"someKey">>   => {str, <<"someBinary">>},
        <<"someInt">>   => {i, 5},
        <<"someList">>  => {arr, [{str, <<"list_1">>}, {str, <<"list_2">>}]},
        <<"someMap">>   => {obj, #{{str, <<"someKey">>} => {i, 123}}},
        <<"someNil">>   => {nl, #msgpack_Nil{}}
    }
}).

-define(WALLET_PAYOUT_TYPE, {wallet, #payout_processing_Wallet{
    wallet_id = ?STRING
}}).

-define(PAYOUT_PROC_PAYOUT_SUMMARY_ITEM, #payout_processing_PayoutSummaryItem{
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    from_time = ?TIMESTAMP,
    to_time = ?TIMESTAMP,
    operation_type = payment,
    count = ?INTEGER
}).
