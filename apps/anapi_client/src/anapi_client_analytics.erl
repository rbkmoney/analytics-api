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

-module(anapi_client_analytics).

-export([get_payments_tool_distribution/2]).
-export([get_payments_amount/2]).
-export([get_average_payment/2]).
-export([get_payments_count/2]).
-export([get_payments_error_distribution/2]).
-export([get_payments_sub_error_distribution/2]).
-export([get_payments_split_amount/2]).
-export([get_payments_split_count/2]).
-export([get_refunds_amount/2]).
-export([get_current_balances/2]).
-export([get_current_balances_group_by_shop/2]).

-type context() :: anapi_client_lib:context().
-type analytics_query() :: anapi_client_lib:analytics_query().

-spec get_payments_tool_distribution(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_tool_distribution(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_tool_distribution(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentToolDistribution}} ->
            {ok, PaymentToolDistribution};
        {error, Error} ->
            {error, Error}
    end.

-spec get_payments_amount(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_amount(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_amount(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentsAmount}} ->
            {ok, PaymentsAmount};
        {error, Error} ->
            {error, Error}
    end.

-spec get_average_payment(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_average_payment(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_average_payment(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := AveragePayment}} ->
            {ok, AveragePayment};
        {error, Error} ->
            {error, Error}
    end.

-spec get_payments_count(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_count(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_count(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentsCount}} ->
            {ok, PaymentsCount};
        {error, Error} ->
            {error, Error}
    end.

-spec get_payments_error_distribution(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_error_distribution(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_error_distribution(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentErrorDistribution}} ->
            {ok, PaymentErrorDistribution};
        {error, Error} ->
            {error, Error}
    end.

-spec get_payments_sub_error_distribution(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_sub_error_distribution(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_sub_error_distribution(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentErrorDistribution}} ->
            {ok, PaymentErrorDistribution};
        {error, Error} ->
            {error, Error}
    end.

-spec get_payments_split_amount(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_split_amount(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_split_amount(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentsSplitAmount}} ->
            {ok, PaymentsSplitAmount};
        {error, Error} ->
            {error, Error}
    end.

-spec get_payments_split_count(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_payments_split_count(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_payments_split_count(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := PaymentsSplitCount}} ->
            {ok, PaymentsSplitCount};
        {error, Error} ->
            {error, Error}
    end.

-spec get_refunds_amount(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_refunds_amount(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_refunds_amount(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := RefundsAmount}} ->
            {ok, RefundsAmount};
        {error, Error} ->
            {error, Error}
    end.

-spec get_current_balances(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_current_balances(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_current_balances(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := RefundsAmount}} ->
            {ok, RefundsAmount};
        {error, Error} ->
            {error, Error}
    end.

-spec get_current_balances_group_by_shop(context(), analytics_query()) -> {ok, term()} | {error, term()}.
get_current_balances_group_by_shop(Context, Query) ->
    Qs = anapi_client_lib:make_analytics_query_string(Query),
    Params = #{qs_val => Qs},
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_analytics_api:get_current_balances_group_by_shop(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"result">> := RefundsAmount}} ->
            {ok, RefundsAmount};
        {error, Error} ->
            {error, Error}
    end.
