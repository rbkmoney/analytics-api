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
-include_lib("analytics_proto/include/analytics_proto_analytics_thrift.hrl").

-define(STRING, <<"TEST">>).
-define(RUB, <<"RUB">>).
-define(USD, <<"USD">>).
-define(BANKID_RU, <<"PUTIN">>).
-define(BANKID_US, <<"TRAMP">>).
-define(WALLET_TOOL, <<"TOOL">>).
-define(JSON, <<"{}">>).
-define(INTEGER, 10000).
-define(PERCENT, 99.0).
-define(INTEGER_BINARY, <<"10000">>).
-define(TIMESTAMP, <<"2016-03-22T06:12:27Z">>).
-define(MD5, <<"033BD94B1168D7E4F0D644C3C95E35BF">>).
-define(SHA256, <<"94EE059335E587E501CC4BF90613E0814F00A7B08BC7C648FD865A2AF6A22CC2">>).
-define(TEST_USER_REALM, <<"external">>).
-define(TEST_RULESET_ID, <<"test/api">>).

-define(CONTENT, #'Content'{
    type = <<"application/json">>,
    data = ?JSON
}).

-define(STAT_RESPONSE(Data), #merchstat_StatResponse{
    data = Data,
    total_count = ?INTEGER,
    continuation_token = ?STRING
}).

-define(STAT_RESPONSE_INVOICES, ?STAT_RESPONSE({invoices, [?STAT_INVOICE]})).

-define(STAT_RESPONSE_PAYMENTS,
    ?STAT_RESPONSE(
        {payments, [
            ?STAT_PAYMENT(?STAT_PAYER({bank_card, ?STAT_BANK_CARD}), ?STAT_PAYMENT_STATUS_CAPTURED),
            ?STAT_PAYMENT(?STAT_PAYER({bank_card, ?STAT_BANK_CARD_WITH_TP}), ?STAT_PAYMENT_STATUS_PENDING),
            ?STAT_PAYMENT(?STAT_PAYER({bank_card, ?STAT_BANK_CARD}, undefined), ?STAT_PAYMENT_STATUS_CAPTURED)
        ]}
    )
).

-define(STAT_RESPONSE_RECORDS, ?STAT_RESPONSE({records, [?STAT_RECORD]})).

-define(STAT_RESPONSE_REFUNDS, ?STAT_RESPONSE({refunds, [?STAT_REFUND]})).

-define(STAT_RESPONSE_PAYOUTS,
    ?STAT_RESPONSE(
        {payouts, [
            ?STAT_PAYOUT({wallet_info, #domain_WalletInfo{wallet_id = ?STRING}}),
            ?STAT_PAYOUT({russian_bank_account, ?STAT_PAYOUT_BANK_ACCOUNT_RUS}),
            ?STAT_PAYOUT({international_bank_account, ?STAT_PAYOUT_BANK_ACCOUNT_INT})
        ]}
    )
).

-define(STAT_RESPONSE_CHARGEBACKS, ?STAT_RESPONSE({chargebacks, [?STAT_CHARGEBACK]})).

-define(STAT_INVOICE, #merchstat_StatInvoice{
    id = ?STRING,
    owner_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {unpaid, #merchstat_InvoiceUnpaid{}},
    product = ?STRING,
    description = ?STRING,
    due = ?TIMESTAMP,
    amount = ?INTEGER,
    currency_symbolic_code = ?RUB,
    context = ?CONTENT,
    external_id = ?STRING
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
    cart = ?INVOICE_CART,
    external_id = ?STRING
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

-define(CURRENCY_OBJ, #domain_Currency{
    name = ?STRING,
    symbolic_code = ?RUB,
    numeric_code = 42,
    exponent = 42
}).

-define(TX_INFO, #domain_AdditionalTransactionInfo{
    rrn = <<"090909090909">>,
    approval_code = <<"808080">>
}).

-define(STAT_PAYER(PaymentTool), ?STAT_PAYER(PaymentTool, ?STRING)).

-define(STAT_PAYER(PaymentTool, SessionId),
    {payment_resource, #merchstat_PaymentResourcePayer{
        payment_tool = PaymentTool,
        ip_address = ?STRING,
        fingerprint = ?STRING,
        phone_number = ?STRING,
        email = <<"test@test.ru">>,
        session_id = SessionId
    }}
).

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
    currency_symbolic_code = ?RUB,
    external_id = ?STRING
}).

-define(STAT_PAYOUT(Type), #merchstat_StatPayout{
    id = ?STRING,
    party_id = ?STRING,
    shop_id = ?STRING,
    created_at = ?TIMESTAMP,
    status = {paid, #merchstat_PayoutPaid{}},
    amount = ?INTEGER,
    fee = ?INTEGER,
    currency_symbolic_code = ?RUB,
    payout_tool_info = Type
}).

-define(STAT_PAYOUT_BANK_ACCOUNT_RUS, #domain_RussianBankAccount{
    account = <<"12345678901234567890">>,
    bank_name = ?STRING,
    bank_post_account = <<"12345678901234567890">>,
    bank_bik = <<"123456789">>
}).

-define(STAT_PAYOUT_BANK_ACCOUNT_INT, #domain_InternationalBankAccount{
    number = <<"12345678901234567890">>,
    bank = ?STAT_PAYOUT_BANK_DETAILS_INT,
    correspondent_account = #domain_InternationalBankAccount{number = <<"00000000000000000000">>},
    iban = <<"GR1601101250000000012300695">>,
    account_holder = ?STRING
}).

-define(STAT_PAYOUT_BANK_DETAILS_INT, #domain_InternationalBankDetails{
    %% In reality either bic or aba_rtn should be used, not both.
    bic = <<"DEUTDEFF500">>,
    country = usa,
    name = ?STRING,
    address = ?STRING,
    aba_rtn = <<"129131673">>
}).

-define(STAT_BANK_CARD, #merchstat_BankCard{
    token = ?STRING,
    payment_system_deprecated = visa,
    bin = <<"411111">>,
    masked_pan = <<"411111******1111">>
}).

-define(STAT_BANK_CARD_WITH_TP, #merchstat_BankCard{
    token = ?STRING,
    payment_system_deprecated = visa,
    bin = <<"411111">>,
    masked_pan = <<"411111******1111">>,
    token_provider_deprecated = applepay
}).

-define(STAT_CHARGEBACK, #merchstat_StatChargeback{
    invoice_id = ?STRING,
    payment_id = ?STRING,
    chargeback_id = ?STRING,
    party_id = ?STRING,
    shop_id = ?STRING,
    chargeback_status = {pending, #domain_InvoicePaymentChargebackPending{}},
    created_at = ?TIMESTAMP,
    chargeback_reason = #domain_InvoicePaymentChargebackReason{
        code = <<"authorisation">>,
        category = {authorisation, #domain_InvoicePaymentChargebackCategoryAuthorisation{}}
    },
    levy_amount = ?INTEGER,
    levy_currency_code = ?CURRENCY_OBJ,
    amount = ?INTEGER,
    currency_code = ?CURRENCY_OBJ,
    fee = ?INTEGER,
    provider_fee = ?INTEGER,
    external_fee = ?INTEGER,
    stage = {arbitration, #domain_InvoicePaymentChargebackStageArbitration{}},
    content = ?CONTENT,
    external_id = ?STRING
}).

-define(REPORT_TYPE, <<"paymentRegistry">>).
-define(REPORT_TYPE_ALT, <<"provisionOfService">>).

-define(REPORT, ?REPORT(<<"payment_registry">>)).

-define(REPORT(ReportType), #reports_Report{
    report_id = ?INTEGER,
    time_range = #reports_ReportTimeRange{
        from_time = ?TIMESTAMP,
        to_time = ?TIMESTAMP
    },
    created_at = ?TIMESTAMP,
    report_type = ReportType,
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

-define(SPLIT_UNIT, minute).

-define(ANALYTICS_PAYMENT_TOOL_DISTRIBUTION_RESP, #analytics_PaymentToolDistributionResponse{
    payment_tools_distributions = [?ANALYTICS_NAMING_DISTRIBUTION, ?ANALYTICS_NAMING_DISTRIBUTION]
}).

-define(ANALYTICS_NAMING_DISTRIBUTION, #analytics_NamingDistribution{
    name = ?STRING,
    percents = ?PERCENT
}).

-define(ANALYTICS_SUB_ERROR, #analytics_SubError{
    code = ?STRING,
    sub_error = ?ANALYTICS_SUB_ERROR_SECOND
}).

-define(ANALYTICS_SUB_ERROR_SECOND, #analytics_SubError{
    code = ?STRING
}).

-define(ANALYTICS_ERROR_DISTRIBUTION, #analytics_ErrorDistribution{
    error = ?ANALYTICS_SUB_ERROR,
    percents = ?PERCENT
}).

-define(ANALYTICS_AMOUNT_RESP, #analytics_AmountResponse{
    groups_amount = [?ANALYTICS_CURRENCY_GROUPED_AMOUNT, ?ANALYTICS_CURRENCY_GROUPED_AMOUNT]
}).

-define(ANALYTICS_SHOP_AMOUNT_RESP, #analytics_ShopAmountResponse{
    groups_amount = [?ANALYTICS_SHOP_GROUPED_AMOUNT, ?ANALYTICS_SHOP_GROUPED_AMOUNT]
}).

-define(ANALYTICS_CURRENCY_GROUPED_AMOUNT, #analytics_CurrencyGroupedAmount{
    amount = ?INTEGER,
    currency = ?RUB
}).

-define(ANALYTICS_SHOP_GROUPED_AMOUNT, #analytics_ShopGroupedAmount{
    amount = ?INTEGER,
    shop_id = ?STRING,
    currency = ?RUB
}).

-define(ANALYTICS_COUNT_RESP, #analytics_CountResponse{
    groups_count = [?ANALYTICS_CURRENCY_GROUP_COUNT, ?ANALYTICS_CURRENCY_GROUP_COUNT]
}).

-define(ANALYTICS_CURRENCY_GROUP_COUNT, #analytics_CurrecyGroupCount{
    currency = ?RUB,
    count = ?INTEGER
}).

-define(ANALYTICS_ERROR_DISTRIBUTION_RESP, #analytics_ErrorDistributionsResponse{
    error_distributions = [?ANALYTICS_NAMING_DISTRIBUTION, ?ANALYTICS_NAMING_DISTRIBUTION]
}).

-define(ANALYTICS_SUB_ERROR_DISTRIBUTION_RESP, #analytics_SubErrorDistributionsResponse{
    error_distributions = [?ANALYTICS_ERROR_DISTRIBUTION, ?ANALYTICS_ERROR_DISTRIBUTION]
}).

-define(ANALYTICS_SPLIT_AMOUNT_RESP, #analytics_SplitAmountResponse{
    result_split_unit = ?SPLIT_UNIT,
    grouped_currency_amounts = [?ANALYTICS_GROUPED_CURRENCY_OFFSET_AMOUNT, ?ANALYTICS_GROUPED_CURRENCY_OFFSET_AMOUNT]
}).

-define(ANALYTICS_GROUPED_CURRENCY_OFFSET_AMOUNT, #analytics_GroupedCurrencyOffsetAmount{
    currency = ?RUB,
    offset_amounts = [?ANALYTICS_OFFSET_AMOUNT, ?ANALYTICS_OFFSET_AMOUNT]
}).

-define(ANALYTICS_OFFSET_AMOUNT, #analytics_OffsetAmount{
    amount = ?INTEGER,
    offset = ?INTEGER
}).

-define(ANALYTICS_SPLIT_COUNT_RESP, #analytics_SplitCountResponse{
    result_split_unit = ?SPLIT_UNIT,
    payment_tools_destrobutions = [?ANALYTICS_GROUPED_CURRENCY_OFFSET_COUNT, ?ANALYTICS_GROUPED_CURRENCY_OFFSET_COUNT]
}).

-define(ANALYTICS_GROUPED_CURRENCY_OFFSET_COUNT, #analytics_GroupedCurrencyOffsetCount{
    currency = ?RUB,
    offset_amounts = [?ANALYTICS_GROUPED_STATUS_OFFSET_COUNT, ?ANALYTICS_GROUPED_STATUS_OFFSET_COUNT]
}).

-define(ANALYTICS_GROUPED_STATUS_OFFSET_COUNT, #analytics_GroupedStatusOffsetCount{
    status = pending,
    offsetCounts = [?ANALYTICS_OFFSET_COUNT, ?ANALYTICS_OFFSET_COUNT]
}).

-define(ANALYTICS_OFFSET_COUNT, #analytics_OffsetCount{
    count = ?INTEGER,
    offset = ?INTEGER
}).
