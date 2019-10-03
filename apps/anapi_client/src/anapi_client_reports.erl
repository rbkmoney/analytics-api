-module(anapi_client_reports).

-export([get_reports/2]).
-export([get_report/3]).
-export([create_report/2]).
-export([download_file/3]).

-type context() :: anapi_client_lib:context().
-type reporting_query() :: anapi_client_lib:reporting_query().

-spec get_reports(context(), reporting_query()) -> {ok, list()} | {error, term()}.
get_reports(Context, Query) ->
    Qs = anapi_client_lib:make_reporting_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:get_reports(Url, PreparedParams, Opts),
    anapi_client_lib:handle_response(Response).

-spec get_report(context(), binary(), reporting_query()) -> {ok, list()} | {error, term()}.
get_report(Context, ReportID, Query) ->
    Qs = anapi_client_lib:make_reporting_query_string(Query),
    Params = #{
        binding => #{
            <<"reportID">> => ReportID
        },
        qs_val => Qs
    },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:get_report(Url, PreparedParams, Opts),
    anapi_client_lib:handle_response(Response).

-spec create_report(context(), reporting_query()) ->
    {ok, list()} | {error, term()}.
create_report(Context, Query) ->
    Qs = anapi_client_lib:make_reporting_query_string(Query),
    Params = #{ qs_val => Qs },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:create_report(Url, PreparedParams, Opts),
    anapi_client_lib:handle_response(Response).

-spec download_file(context(), binary(), binary()) -> {ok, redirect} | {error, term()}.
download_file(Context, ReportID, FileID) ->
    Params = #{
        binding => #{
            <<"reportID">> => ReportID,
            <<"fileID">> => FileID
        }
    },
    {Url, PreparedParams, Opts} = anapi_client_lib:make_request(Context, Params),
    Response = swag_client_reports_api:download_file(Url, PreparedParams, Opts),
    anapi_client_lib:handle_response(Response).
