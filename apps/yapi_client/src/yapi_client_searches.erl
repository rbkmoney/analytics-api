-module(yapi_client_searches).

-export([search_invoices/2]).
-export([search_payments/2]).
-export([search_refunds/2]).
-export([search_payouts/2]).

-type context() :: yapi_client_lib:context().
-type search_query() :: yapi_client_lib:search_query().

-spec search_invoices(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_invoices(Context, Query) ->
    Qs = yapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = yapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_invoices(Url, PreparedParams, Opts),
    case yapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Invoices}} ->
            {ok, TotalCount, Invoices};
        {error, Error} -> {error, Error}
    end.

-spec search_payments(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payments(Context, Query) ->
    Qs = yapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = yapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payments(Url, PreparedParams, Opts),
    case yapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} -> {error, Error}
    end.

-spec search_refunds(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_refunds(Context, Query) ->
    Qs = yapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = yapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_refunds(Url, PreparedParams, Opts),
    case yapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} -> {error, Error}
    end.

-spec search_payouts(context(), search_query()) -> {ok, term(), term()} | {error, term()}.
search_payouts(Context, Query) ->
    Qs = yapi_client_lib:make_search_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = yapi_client_lib:make_request(Context, Params),
    Response = swag_client_search_api:search_payouts(Url, PreparedParams, Opts),
    case yapi_client_lib:handle_response(Response) of
        {ok, #{<<"totalCount">> := TotalCount, <<"result">> := Payments}} ->
            {ok, TotalCount, Payments};
        {error, Error} -> {error, Error}
    end.
