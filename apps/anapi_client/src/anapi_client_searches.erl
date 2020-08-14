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

-module(anapi_client_searches).

-export([search_invoices/2]).
-export([search_payments/2]).
-export([search_refunds/2]).
-export([search_payouts/2]).
-export([search_chargebacks/2]).

-type context() :: anapi_client_lib:context().
-type search_query() :: anapi_client_lib:search_query().

-spec search_invoices(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_invoices(Context, Query) ->
    Qs = anapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_invoices(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Invoices}} ->
            {ok, TotalCount, Invoices};
        {error, Error} -> {error, Error}
    end.

-spec search_payments(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payments(Context, Query) ->
    Qs = anapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payments(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} -> {error, Error}
    end.

-spec search_refunds(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_refunds(Context, Query) ->
    Qs = anapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_refunds(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} -> {error, Error}
    end.

-spec search_payouts(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payouts(Context, Query) ->
    Qs = anapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payouts(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} -> {error, Error}
    end.

-spec search_chargebacks(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_chargebacks(Context, Query) ->
    Qs = anapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_chargebacks(Url, PreparedParams, Opts),
    case anapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Chargebacks}} ->
            {ok, TotalCount, Chargebacks};
        {error, Error} -> {error, Error}
    end.
