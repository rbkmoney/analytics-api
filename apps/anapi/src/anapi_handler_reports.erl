-module(anapi_handler_reports).

-include_lib("reporter_proto/include/reporter_base_thrift.hrl").
-include_lib("reporter_proto/include/reporter_reports_thrift.hrl").

-behaviour(anapi_handler).

-export([prepare/3]).

-import(anapi_handler_utils, [general_error/2, logic_error/2]).

% seconds
-define(DEFAULT_URL_LIFETIME, 60).

-spec prepare(
    OperationID :: anapi_handler:operation_id(),
    Req :: anapi_handler:request_data(),
    Context :: anapi_handler:processing_context()
) -> {ok, anapi_handler:request_state()} | {error, noimpl}.
prepare(OperationID, Req, Context) when OperationID =:= 'SearchReports' ->
    OperationContext = make_authorization_query(OperationID, Req, maps:get(partyID, Req)),
    Authorize = fun() ->
        {ok, anapi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun(undefined) ->
        Params = #{
            party_id => maps:get('partyID', Req),
            shop_id => genlib_map:get(shopID, Req),
            shop_ids => anapi_handler_utils:enumerate_shop_ids(Req, Context),
            from_time => anapi_handler_utils:get_time(fromTime, Req),
            to_time => anapi_handler_utils:get_time(toTime, Req),
            report_types => [encode_report_type(F) || F <- maps:get(reportTypes, Req)],
            continuation_token => genlib_map:get(continuationToken, Req),
            limit => maps:get(limit, Req, undefined)
        },
        process_search_reports(Params, Context)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'GetReport' ->
    ReportId = maps:get(reportID, Req),
    Report =
        case anapi_handler_utils:get_report_by_id(ReportId, Context) of
            {ok, R} ->
                R;
            {exception, #reports_ReportNotFound{}} ->
                {error, general_error(404, <<"Report not found">>)}
        end,
    OperationContext = make_authorization_query(OperationID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, OperationContext}, {reports, #{report => maybe_woody_reply(Report)}}],
        {ok, anapi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun(undefined) ->
        anapi_handler:respond_if_error(Report),
        {ok, {200, #{}, decode_report(Report)}}
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'CreateReport' ->
    OperationContext = make_authorization_query(OperationID, Req, maps:get(partyID, Req)),
    Authorize = fun() ->
        {ok, anapi_auth:authorize_operation([{operation, OperationContext}], Context)}
    end,
    Process = fun(undefined) ->
        Params = #{
            party_id => maps:get('partyID', Req),
            shop_id => genlib_map:get(shopID, Req),
            from_time => anapi_handler_utils:get_time(fromTime, Req),
            to_time => anapi_handler_utils:get_time(toTime, Req),
            report_type => encode_report_type(maps:get(reportType, Req))
        },
        process_create_report(Params, Context)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'CancelReport' ->
    ReportId = maps:get(reportID, Req),
    Report =
        case anapi_handler_utils:get_report_by_id(ReportId, Context) of
            {ok, R} ->
                case can_cancel_report(R) of
                    true -> R;
                    false -> {error, logic_error(invalidRequest, <<"Invalid report type">>)}
                end;
            {exception, #reports_ReportNotFound{}} ->
                {error, general_error(404, <<"Report not found">>)}
        end,
    OperationContext = make_authorization_query(OperationID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, OperationContext}, {reports, #{report => maybe_woody_reply(Report)}}],
        {ok, anapi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun(undefined) ->
        anapi_handler:respond_if_error(Report),
        cancel_report(ReportId, Context)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(OperationID, Req, Context) when OperationID =:= 'DownloadFile' ->
    ReportId = maps:get(reportID, Req),
    Report =
        case anapi_handler_utils:get_report_by_id(ReportId, Context) of
            {ok, R} ->
                R;
            {exception, #reports_ReportNotFound{}} ->
                {error, general_error(404, <<"Report not found">>)}
        end,
    OperationContext = make_authorization_query(OperationID, Req),
    Authorize = fun() ->
        Prototypes = [{operation, OperationContext}, {reports, #{report => maybe_woody_reply(Report)}}],
        {ok, anapi_auth:authorize_operation(Prototypes, Context)}
    end,
    Process = fun(undefined) ->
        anapi_handler:respond_if_error(Report),
        #reports_Report{files = Files} = Report,
        FileID = maps:get(fileID, Req),
        case lists:keymember(FileID, #reports_FileMeta.file_id, Files) of
            true ->
                generate_report_presigned_url(FileID, Context);
            false ->
                {error, general_error(404, <<"File not found">>)}
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(_OperationID, _Req, _Context) ->
    {error, noimpl}.

make_authorization_query(OperationID, Req, PartyID) ->
    Query = make_authorization_query(OperationID, Req),
    Query#{party_id => PartyID}.

make_authorization_query(OperationID, Req) ->
    genlib_map:compact(#{
        id => OperationID,
        shop_id => genlib_map:get(shopID, Req),
        report_id => genlib_map:get(reportID, Req),
        file_id => genlib_map:get(fileID, Req)
    }).

process_create_report(Params, Context) ->
    ReportRequest = #reports_ReportRequest{
        party_id = maps:get(party_id, Params),
        shop_id = maps:get(shop_id, Params),
        time_range = #reports_ReportTimeRange{
            from_time = maps:get(from_time, Params),
            to_time = maps:get(to_time, Params)
        }
    },
    ReportType = maps:get(report_type, Params),
    case anapi_handler_utils:service_call({reporting, 'CreateReport', {ReportRequest, ReportType}}, Context) of
        {ok, ReportId} ->
            {ok, Report} = anapi_handler_utils:service_call({reporting, 'GetReport', {ReportId}}, Context),
            {ok, {201, #{}, decode_report(Report)}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_ShopNotFound{} ->
                    {ok, logic_error(invalidShopID, <<"Shop not found">>)}
            end
    end.

cancel_report(ReportId, Context) ->
    Call = {reporting, 'CancelReport', {ReportId}},
    case anapi_handler_utils:service_call(Call, Context) of
        {ok, _} ->
            {ok, {202, #{}, undefined}};
        {exception, #reports_ReportNotFound{}} ->
            {ok, general_error(404, <<"Report not found">>)}
    end.

process_search_reports(Params, Context) ->
    ReportRequest = #reports_ReportRequest{
        party_id = maps:get(party_id, Params),
        shop_id = maps:get(shop_id, Params),
        shop_ids = maps:get(shop_ids, Params),
        time_range = #reports_ReportTimeRange{
            from_time = maps:get(from_time, Params),
            to_time = maps:get(to_time, Params)
        }
    },
    ReportTypes = maps:get(report_types, Params),
    ContinuationToken = maps:get(continuation_token, Params),
    StatReportRequest = #reports_StatReportRequest{
        request = ReportRequest,
        continuation_token = ContinuationToken,
        report_types = ReportTypes,
        limit = maps:get(limit, Params, undefined)
    },
    Call = {reporting, 'GetReports', {StatReportRequest}},
    case anapi_handler_utils:service_call(Call, Context) of
        {ok, #reports_StatReportResponse{reports = Reports, continuation_token = CT}} ->
            Res = genlib_map:compact(#{
                <<"result">> => [decode_report(R) || R <- Reports],
                <<"continuationToken">> => CT
            }),
            {ok, {200, #{}, Res}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_BadToken{} ->
                    {ok, logic_error(invalidRequest, <<"Invalid token">>)};
                #reports_DatasetTooBig{limit = Limit} ->
                    {ok, logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit]))}
            end
    end.

generate_report_presigned_url(FileID, Context) ->
    ExpiresAt = get_default_url_lifetime(),
    Call = {reporting, 'GeneratePresignedUrl', {FileID, ExpiresAt}},
    case anapi_handler_utils:service_call(Call, Context) of
        {ok, URL} ->
            {ok, {200, #{}, #{<<"url">> => URL}}};
        {exception, Exception} ->
            case Exception of
                #reporter_base_InvalidRequest{errors = Errors} ->
                    FormattedErrors = anapi_handler_utils:format_request_errors(Errors),
                    {ok, logic_error(invalidRequest, FormattedErrors)};
                #reports_FileNotFound{} ->
                    {ok, general_error(404, <<"File not found">>)}
            end
    end.

get_default_url_lifetime() ->
    Now = erlang:system_time(second),
    Lifetime = application:get_env(capi, reporter_url_lifetime, ?DEFAULT_URL_LIFETIME),
    genlib_rfc3339:format(Now + Lifetime, second).

%%

can_cancel_report(#reports_Report{report_type = <<"provision_of_service">>}) ->
    false;
can_cancel_report(_) ->
    true.

encode_report_type(provisionOfService) -> <<"provision_of_service">>;
encode_report_type(paymentRegistry) -> <<"payment_registry">>;
encode_report_type(paymentRegistryByPayout) -> <<"payment_registry_by_payout">>.

%%

decode_report(Report) ->
    #reports_ReportTimeRange{from_time = FromTime, to_time = ToTime} = Report#reports_Report.time_range,
    genlib_map:compact(#{
        <<"id">> => Report#reports_Report.report_id,
        <<"createdAt">> => Report#reports_Report.created_at,
        <<"fromTime">> => FromTime,
        <<"toTime">> => ToTime,
        <<"status">> => decode_report_status(Report#reports_Report.status),
        <<"reportType">> => decode_report_type(Report#reports_Report.report_type),
        <<"files">> => [decode_report_file(F) || F <- Report#reports_Report.files],
        <<"partyID">> => Report#reports_Report.party_id,
        <<"shopID">> => Report#reports_Report.shop_id
    }).

decode_report_status(pending) -> <<"pending">>;
decode_report_status(created) -> <<"created">>.

decode_report_type(<<"provision_of_service">>) -> <<"provisionOfService">>;
decode_report_type(<<"payment_registry">>) -> <<"paymentRegistry">>;
decode_report_type(<<"payment_registry_by_payout">>) -> <<"paymentRegistryByPayout">>.

decode_report_file(#reports_FileMeta{file_id = ID, filename = Filename, signature = Signature}) ->
    #{
        <<"id">> => ID,
        <<"filename">> => Filename,
        <<"signatures">> => decode_report_file_signature(Signature)
    }.

decode_report_file_signature(#reports_Signature{md5 = MD5, sha256 = SHA256}) ->
    #{<<"md5">> => MD5, <<"sha256">> => SHA256}.

maybe_woody_reply({error, _}) ->
    undefined;
maybe_woody_reply(Reply) ->
    Reply.
