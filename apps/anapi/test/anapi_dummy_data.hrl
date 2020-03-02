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

-include_lib("damsel/include/dmsl_geo_ip_thrift.hrl").
-include_lib("damsel/include/dmsl_merch_stat_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").

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

-define(CONTENT, #'Content'{
    type = <<"application/json">>,
    data = ?JSON
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
    flow = ?INSTANT_INVOICE_PAYMENT_FLOW,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    payer = Payer,
    context = ?CONTENT,
    domain_revision = ?INTEGER,
    additional_transaction_info = ?TX_INFO,
    location_info = ?LOCATION_INFO,
    short_id = ?STRING,
    make_recurrent = false,
    cart = ?INVOICE_CART
}).

-define(INSTANT_INVOICE_PAYMENT_FLOW, {instant, #merchstat_InvoicePaymentFlowInstant{}}).

-define(LOCATION_INFO, #geo_ip_LocationInfo{
    city_geo_id = ?INTEGER,
    country_geo_id = ?INTEGER,
    raw_response = ?STRING
}).

-define(INVOICE_CART, #domain_InvoiceCart{
    lines = []
}).

-define(CASH, #domain_Cash{
    amount = ?INTEGER,
    currency = ?CURRENCY
}).

-define(CURRENCY, #domain_CurrencyRef{
    symbolic_code = ?RUB
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
-define(REPORT_TYPE_ALT, <<"provisionOfService">>).

-define(REPORT, #reports_Report{
    report_id = ?INTEGER,
    time_range = #reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = <<"payment_registry">>,
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
    ],
    shop_id = ?STRING,
    party_id = ?STRING
}).

-define(REPORT_WITHOUT_SHOP_ID, #reports_Report{
    report_id = ?INTEGER,
    time_range = #reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = <<"payment_registry">>,
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
    ],
    party_id = ?STRING
}).

-define(REPORT_ALT, #reports_Report{
    report_id = ?INTEGER,
    time_range = #reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = <<"payment_registry">>,
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
    ],
    shop_id = ?STRING,
    party_id = <<"ALT_PARTY_ID">>
}).

-define(FOUND_REPORTS, #'reports_StatReportResponse'{
    reports = [?REPORT]
}).
