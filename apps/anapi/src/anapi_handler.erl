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

-module(anapi_handler).

-behaviour(swag_server_logic_handler).

%% API callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% Handler behaviour

-export_type([operation_id/0]).
-export_type([request_data/0]).
-export_type([request_context/0]).
-export_type([response/0]).
-export_type([processing_context/0]).

-callback process_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    Context     :: processing_context()
) ->
    {ok | error, response() | noimpl}.

-import(anapi_handler_utils, [logic_error/2, server_error/1]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-define(SWAG_HANDLER_SCOPE, swag_handler).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key(), handler_opts()) ->
    Result :: false | {true, uac:context()}.

authorize_api_key(OperationID, ApiKey, _HandlerOpts) ->
    scoper:scope(?SWAG_HANDLER_SCOPE, #{operation_id => OperationID},
        fun() ->
            _ = logger:debug("Api key authorization started"),
            case uac:authorize_api_key(ApiKey, get_verification_options()) of
                {ok, Context} ->
                    _ = logger:debug("Api key authorization successful"),
                    {true, Context};
                {error, Error} ->
                    _ = logger:info("Api key authorization failed due to ~p", [Error]),
                    false
            end
        end).

-type request_data()        :: #{atom() | binary() => term()}.

-type operation_id()        :: swag_server:operation_id().
-type request_context()     :: swag_server:request_context().
-type response()            :: swag_server:response().
-type handler_opts()        :: swag_server:handler_opts(_).
-type processing_context()  :: #{
    swagger_context := swag_server:request_context(),
    woody_context   := woody_context:ctx()
}.

get_handlers() ->
    [
        anapi_handler_search,
        anapi_handler_reports,
        anapi_handler_analytics
    ].

get_verification_options() ->
    #{}.

-spec handle_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    SwagContext :: request_context(),
    HandlerOpts :: handler_opts()
) ->
    {ok | error,   response()}.

handle_request(OperationID, Req, SwagContext, _HandlerOpts) ->
    scoper:scope(
        ?SWAG_HANDLER_SCOPE,
        #{
            operation_id => OperationID
        },
        fun() -> handle_request_(OperationID, Req, SwagContext) end
    ).

handle_request_(OperationID, Req, SwagContext = #{auth_context := AuthContext}) ->
    try
        WoodyContext = attach_deadline(Req, create_woody_context(Req, AuthContext)),
        _ = logger:debug("Processing request"),
        OperationACL = anapi_auth:get_operation_access(OperationID, Req),
        case uac:authorize_operation(OperationACL, AuthContext) of
            ok ->
                Context = create_processing_context(SwagContext, WoodyContext),
                process_request(OperationID, Req, Context, get_handlers());
            {error, _} = Error ->
                _ = logger:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {ok, {401, #{}, undefined}}
        end
    catch
        throw:{bad_deadline, _Deadline} ->
            {ok, logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>)};
        throw:{handler_function_clause, _OperationID} ->
            _ = logger:error("Operation ~p failed due to missing handler", [OperationID]),
            {error, {501, #{}, undefined}};
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        Class:Reason:Stacktrace ->
            process_general_error(Class, Reason, Stacktrace, OperationID, Req, SwagContext)
    end.

-spec process_request(
    OperationID :: operation_id(),
    Req         :: request_data(),
    Context     :: processing_context(),
    Handlers    :: list(module())
) ->
    {ok | error,   response()}.

process_request(OperationID, _Req, _Context, []) ->
    erlang:throw({handler_function_clause, OperationID});
process_request(OperationID, Req, Context, [Handler | Rest]) ->
    case Handler:process_request(OperationID, Req, Context) of
        {error, noimpl} ->
            process_request(OperationID, Req, Context, Rest);
        Response ->
            Response
    end.

%%

create_processing_context(SwaggerContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwaggerContext
    }.

create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => anapi_auth:get_subject_id(AuthContext),
        realm    => ?REALM,
        email    => anapi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => anapi_auth:get_claim(<<"name">> , AuthContext, undefined)
    }).

attach_deadline(#{'X-Request-Deadline' := undefined}, Context) ->
    Context;
attach_deadline(#{'X-Request-Deadline' := Header}, Context) ->
    case anapi_utils:parse_deadline(Header) of
        {ok, Deadline} when Deadline /= undefined ->
            woody_context:set_deadline(Deadline, Context);
        _ ->
            throw({bad_deadline, Header})
    end.

process_woody_error(_Source, result_unexpected   , _Details) ->
    {error, server_error(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, server_error(503)};
process_woody_error(_Source, result_unknown      , _Details) ->
    {error, server_error(504)}.

process_general_error(Class, Reason, Stacktrace, OperationID, Req, SwagContext) ->
    _ = logger:error(
        "Operation ~p failed due to ~p:~p given req: ~p and context: ~p",
        [OperationID, Class, Reason, Req, SwagContext],
        #{error => #{
            class       => genlib:to_binary(Class),
            reason      => genlib:format(Reason),
            stack_trace => genlib_format:format_stacktrace(Stacktrace)
        }}
    ),
    {error, server_error(500)}.
