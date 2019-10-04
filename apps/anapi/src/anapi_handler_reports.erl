-module(anapi_handler_reports).

-include_lib("reporter_proto/include/reporter_base_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").

-behaviour(anapi_handler).
-export([process_request/3]).
-import(anapi_handler_utils, [general_error/2, logic_error/2]).

-define(DEFAULT_URL_LIFETIME, 60). % seconds

-spec process_request(
    OperationID :: anapi_handler:operation_id(),
    Req         :: anapi_handler:request_data(),
    Context     :: anapi_handler:processing_context()
) ->
    {ok | error, anapi_handler:response() | noimpl}.

process_request('GetReports', Req, Context) ->
    PartyId = anapi_handler_utils:get_party_id(Context),
    case maps:get(partyID, Req) of
        PartyId ->
            ReportRequest = #reports_ReportRequest{
                party_id   = PartyId,
                shop_id    = maps:get(shopID, Req),
                time_range =
                #reports_ReportTimeRange{
                    from_time = anapi_handler_utils:get_time('fromTime', Req),
                    to_time   = anapi_handler_utils:get_time('toTime'  , Req)
                }
            },
            ReportTypes = [encode_report_type(F) || F <- maps:get(reportTypes, Req)],
            Call = {reporting, 'GetReports', [ReportRequest, ReportTypes]},
            case anapi_handler_utils:service_call(Call, Context) of
                {ok, Reports} ->
                    {ok, {200, #{}, [decode_report(R) || R <- Reports]}};
                {exception, Exception} ->
                    case Exception of
                        #reporter_base_InvalidRequest{errors = Errors} ->
                            FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
                            {ok, logic_error(invalidRequest, FormattedErrors)};
                        #reports_DatasetTooBig{limit = Limit} ->
                            {ok, logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit]))}
                    end
            end;
        _WrongPartyId ->
            {ok, logic_error(invalidRequest, <<"Party not found">>)}
    end;

process_request('GetReport', Req, Context) ->
    PartyId = anapi_handler_utils:get_party_id(Context),
    ReportId = maps:get(reportID, Req),
    Call = {reporting, 'GetReport', [ReportId]},
    case anapi_handler_utils:service_call(Call, Context) of
        {ok, Report = #'reports_Report'{party_id = PartyId}} ->
            {ok, {200, #{}, decode_report(Report)}};
        {ok, _WrongReport} ->
            {ok, general_error(404, <<"Report not found">>)};
        {exception, #reports_ReportNotFound{}} ->
            {ok, general_error(404, <<"Report not found">>)}
    end;

process_request('CreateReport', Req, Context) ->
    PartyId = anapi_handler_utils:get_party_id(Context),
    case maps:get(partyID, Req) of
        PartyId ->
            ShopId = maps:get(shopID, Req),
            ReportRequest = #reports_ReportRequest{
                party_id   = PartyId,
                shop_id    = ShopId,
                time_range =
                #reports_ReportTimeRange{
                    from_time = anapi_handler_utils:get_time('fromTime', Req),
                    to_time   = anapi_handler_utils:get_time('toTime'  , Req)
                }
            },
            ReportType = encode_report_type(maps:get(reportType, Req)),
            case anapi_handler_utils:service_call({reporting, 'CreateReport', [ReportRequest, ReportType]}, Context) of
                {ok, ReportId} ->
                    {ok, Report} = anapi_handler_utils:service_call(
                        {reporting, 'GetReport', [ReportId]},
                        Context
                    ),
                    {ok, {201, #{}, decode_report(Report)}};
                {exception, Exception} ->
                    case Exception of
                        #reporter_base_InvalidRequest{errors = Errors} ->
                            FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
                            {ok, logic_error(invalidRequest, FormattedErrors)};
                        #reports_ShopNotFound{} ->
                            {ok, logic_error(invalidShopID, <<"Shop not found">>)}
                    end
            end;
        _WrongPartyId ->
            {ok, logic_error(invalidRequest, <<"Party not found">>)}
    end;

process_request('DownloadFile', Req, Context) ->
    Call = {
        reporting,
        'GetReport',
        [maps:get(reportID, Req)]
    },
    case anapi_handler_utils:service_call(Call, Context) of
        {ok, #reports_Report{status = created, files = Files}} ->
            FileID = maps:get(fileID, Req),
            case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
                true ->
                    generate_report_presigned_url(FileID, Context);
                false ->
                    {ok, general_error(404, <<"File not found">>)}
            end;
        {exception, #reports_ReportNotFound{}} ->
            {ok, general_error(404, <<"Report not found">>)}
    end;

%%

process_request(_OperationID, _Req, _Context) ->
    {error, noimpl}.

generate_report_presigned_url(FileID, Context) ->
    ExpiresAt = get_default_url_lifetime(),
    Call = {reporting, 'GeneratePresignedUrl', [FileID, ExpiresAt]},
    case anapi_handler_utils:service_call(Call, Context) of
        {ok, URL} ->
            {ok, {200, #{}, #{<<"url">> => URL}}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_FileNotFound{}->
                    {ok, general_error(404, <<"File not found">>)}
            end
    end.

get_default_url_lifetime() ->
    Now      = erlang:system_time(second),
    Lifetime = application:get_env(capi, reporter_url_lifetime, ?DEFAULT_URL_LIFETIME),
    anapi_utils:unwrap(rfc3339:format(Now + Lifetime, second)).

%%

encode_report_type(provisionOfService) -> <<"provision_of_service">>;
encode_report_type(paymentRegistry) -> <<"payment_registry">>.

%%

decode_report(Report) ->
    #reports_ReportTimeRange{from_time = FromTime, to_time = ToTime} = Report#reports_Report.time_range,
    #{
        <<"id"        >> => Report#reports_Report.report_id,
        <<"createdAt" >> => Report#reports_Report.created_at,
        <<"fromTime"  >> => FromTime,
        <<"toTime"    >> => ToTime,
        <<"status"    >> => decode_report_status(Report#reports_Report.status),
        <<"reportType">> => decode_report_type(Report#reports_Report.report_type),
        <<"files"     >> => [decode_report_file(F) || F <- Report#reports_Report.files],
        <<"partyID"   >> => Report#reports_Report.party_id,
        <<"shopID"    >> => Report#reports_Report.shop_id
    }.

decode_report_status(pending) -> <<"pending">>;
decode_report_status(created) -> <<"created">>.

decode_report_type(<<"provision_of_service">>) -> <<"provisionOfService">>;
decode_report_type(<<"payment_registry">>) -> <<"paymentRegistry">>.

decode_report_file(#reports_FileMeta{file_id = ID, filename = Filename, signature = Signature}) ->
    #{
        <<"id"        >> => ID,
        <<"filename"  >> => Filename,
        <<"signatures">> => decode_report_file_signature(Signature)
    }.

decode_report_file_signature(#reports_Signature{md5 = MD5, sha256 = SHA256}) ->
    #{<<"md5">> => MD5, <<"sha256">> => SHA256}.
