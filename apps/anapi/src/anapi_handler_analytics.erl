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

-module(anapi_handler_analytics).

-include_lib("analytics_proto/include/analytics_proto_analytics_thrift.hrl").

-behaviour(anapi_handler).
-export([process_request/3]).

-spec process_request(
    OperationID :: anapi_handler:operation_id(),
    Req         :: anapi_handler:request_data(),
    Context     :: anapi_handler:processing_context()
) ->
    {ok | error, anapi_handler:response() | noimpl}.

process_request('GetPaymentsToolDistribution', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetPaymentsToolDistribution',
        decode_fun => fun decode_payment_tool_distribution_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

process_request('GetPaymentsAmount', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetPaymentsAmount',
        decode_fun => fun decode_amount_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

process_request('GetAveragePayment', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetAveragePayment',
        decode_fun => fun decode_amount_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

process_request('GetPaymentsCount', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetPaymentsCount',
        decode_fun => fun decode_count_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

process_request('GetPaymentsErrorDistribution', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetPaymentsErrorDistribution',
        decode_fun => fun decode_error_distributions_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

process_request('GetPaymentsSplitAmount', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetPaymentsSplitAmount',
        decode_fun => fun decode_split_amount_response/1
    },
    process_analytics_request(split_filter_request, Query, Context, Opts);

process_request('GetPaymentsSplitCount', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetPaymentsSplitCount',
        decode_fun => fun decode_split_count_response/1
    },
    process_analytics_request(split_filter_request, Query, Context, Opts);

process_request('GetRefundsAmount', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetRefundsAmount',
        decode_fun => fun decode_amount_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

process_request('GetCurrentBalances', Req, Context) ->
    Query = make_query(Req, Context),
    Opts = #{
        thrift_fun => 'GetCurrentBalances',
        decode_fun => fun decode_amount_response/1
    },
    process_analytics_request(filter_request, Query, Context, Opts);

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

process_analytics_request(QueryType, Query, Context, Opts = #{thrift_fun := ThriftFun}) ->
    Call = {
        analytics,
        ThriftFun,
        [
            anapi_handler_encoder:encode_analytics_request(QueryType, Query)
        ]
    },
    process_analytics_request_result(anapi_handler_utils:service_call(Call, Context), Opts).

process_analytics_request_result(Result, #{decode_fun := DecodeFun}) ->
    case Result of
        {ok, Data} ->
            DecodedData = DecodeFun(Data),
            Resp = #{
                <<"result">> => DecodedData
            },
            {ok, {200, #{}, Resp}}
    end.

%%

make_query(Req, Context) ->
    #{
        party_id   => anapi_handler_utils:get_party_id(Context),
        shop_ids   => genlib_map:get('shopIDs', Req),
        from_time  => anapi_handler_utils:get_time('fromTime', Req),
        to_time    => anapi_handler_utils:get_time('toTime', Req),
        split_unit => genlib_map:get('splitUnit', Req)
    }.

%%

decode_payment_tool_distribution_response(PaymentToolDistribution) ->
    [#{
        <<"name">> => Name,
        <<"percents">> => Percents
    } || #analytics_NamingDistribution{
        name = Name,
        percents = Percents
    } <- PaymentToolDistribution#analytics_PaymentToolDistributionResponse.payment_tools_distributions].

decode_amount_response(Amounts) ->
    [#{
        <<"amount">> => Amount,
        <<"currency">> => Currency
    } || #analytics_CurrencyGroupedAmount{
        amount = Amount,
        currency = Currency
    } <- Amounts#analytics_AmountResponse.groups_amount].

decode_count_response(Counts) ->
    [#{
        <<"count">> => Count,
        <<"currency">> => Currency
    } || #analytics_CurrecyGroupCount{
        count = Count,
        currency = Currency
    } <- Counts#analytics_CountResponse.groups_count].

decode_error_distributions_response(ErrorDistributions) ->
    [#{
        <<"error">> => Name,
        <<"percents">> => Percents
    } || #analytics_NamingDistribution{
        name = Name,
        percents = Percents
    } <- ErrorDistributions#analytics_ErrorDistributionsResponse.error_distributions].

decode_split_amount_response(SplitAmounts) ->
    SplitUnit = decode_split_unit(SplitAmounts#analytics_SplitAmountResponse.result_split_unit),
    [#{
        <<"splitUnit">> => SplitUnit,
        <<"currency">> => Currency,
        <<"offsetAmounts">> => [decode_offset_amount(OffsetAmount) || OffsetAmount <- OffsetAmounts]
    } || #analytics_GroupedCurrencyOffsetAmount{
        currency = Currency,
        offset_amounts = OffsetAmounts
    } <- SplitAmounts#analytics_SplitAmountResponse.grouped_currency_amounts].

decode_offset_amount(#analytics_OffsetAmount{
    amount = Amount,
    offset = Offset
}) ->
    #{
        <<"amount">> => Amount,
        <<"offset">> => Offset
    }.

decode_split_count_response(SplitCounts) ->
    SplitUnit = decode_split_unit(SplitCounts#analytics_SplitCountResponse.result_split_unit),
    [#{
        <<"splitUnit">> => SplitUnit,
        <<"currency">> => Currency,
        <<"statusOffsetCounts">> => [decode_grouped_status_offset_count(OffsetCount)
            || OffsetCount <- GroupedStatusOffsetCounts]
    } || #analytics_GroupedCurrencyOffsetCount{
        currency = Currency,
        offset_amounts = GroupedStatusOffsetCounts
    } <- SplitCounts#analytics_SplitCountResponse.payment_tools_destrobutions].

decode_grouped_status_offset_count(#analytics_GroupedStatusOffsetCount{
    status = Status,
    offsetCounts = OffsetCounts
}) ->
    #{
        <<"status">> => genlib:to_binary(Status),
        <<"offsetCount">> => [decode_offset_count(OffsetCount) || OffsetCount <- OffsetCounts]
    }.

decode_offset_count(#analytics_OffsetCount{
    count = Count,
    offset = Offset
}) ->
    #{
        <<"count">> => Count,
        <<"offset">> => Offset
    }.

decode_split_unit(minute) ->
    <<"minute">>;
decode_split_unit(hour) ->
    <<"hour">>;
decode_split_unit(day) ->
    <<"day">>;
decode_split_unit(week) ->
    <<"week">>;
decode_split_unit(month) ->
    <<"month">>;
decode_split_unit(year) ->
    <<"year">>.