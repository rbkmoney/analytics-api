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

-type error_type() :: swag_server_logic_handler:error_type().

%% API callbacks
-export([authorize_api_key/3]).
-export([handle_request/4]).
-export([map_error/2]).

%% Handler behaviour

-export_type([operation_id/0]).
-export_type([request_data/0]).
-export_type([request_context/0]).
-export_type([response/0]).
-export_type([processing_context/0]).
-export_type([processing_context/1]).
-export_type([resolution/0]).
-export_type([preprocess_context/0]).
-export_type([preprocess_context/1]).

-callback preprocess_request(
    OperationID :: operation_id(),
    Req :: request_data(),
    Context :: processing_context()
) -> {ok, preprocess_context()} | {error, response() | noimpl}.

-callback process_request(
    OperationID :: operation_id(),
    Req :: request_data(),
    Context :: processing_context()
) -> {ok | error, response() | noimpl}.

-import(anapi_handler_utils, [logic_error/2, server_error/1]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-define(SWAG_HANDLER_SCOPE, swag_handler).

-define(DOMAIN, <<"common-api">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key(), swag_server:handler_opts(_)) ->
    Result :: false | {true, uac:context()}.
authorize_api_key(OperationID, ApiKey, _HandlerOpts) ->
    scoper:scope(
        ?SWAG_HANDLER_SCOPE,
        #{operation_id => OperationID},
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
        end
    ).

-spec map_error(error_type(), swag_server_validation:error()) -> swag_server:error_reason().
map_error(validation_error, Error) ->
    Type = genlib:to_binary(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message =
        case maps:get(description, Error, undefined) of
            undefined ->
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
            Description ->
                DescriptionBin = genlib:to_binary(Description),
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary, ", description: ",
                    DescriptionBin/binary>>
        end,
    jsx:encode(#{
        <<"code">> => <<"invalidRequest">>,
        <<"message">> => Message
    }).

-type request_data() :: #{atom() | binary() => term()}.

-type operation_id() :: swag_server:operation_id().
-type request_context() :: swag_server:request_context().
-type response() :: swag_server:response().
-type processing_context(T) :: #{
    swagger_context := swag_server:request_context(),
    woody_context := woody_context:ctx(),
    preprocess_context => preprocess_context(T),
    restrictions_context => restrictions_context()
}.

-type processing_context() :: processing_context(map()).

-type preprocess_context(T) :: T.
-type preprocess_context() :: preprocess_context(map()).

-type restrictions_context(T) :: T.
-type restrictions_context() :: restrictions_context(map()).

-type resolution() ::
    allowed
    | {restricted, _Restrictions}
    | forbidden.


get_handlers() ->
    [
        anapi_handler_search,
        anapi_handler_reports,
        anapi_handler_analytics
    ].

get_verification_options() ->
    #{
        domains_to_decode => [?DOMAIN]
    }.

-spec handle_request(
    OperationID :: operation_id(),
    Req :: swag_server:object(),
    ReqCtx :: request_context(),
    HandlerOpts :: handler_opts()
) -> {ok | error, response()}.
handle_request(OperationID, Req, ReqCtx, _HandlerOpts) ->
    scoper:scope(
        ?SWAG_HANDLER_SCOPE,
        #{
            operation_id => OperationID
        },
        fun() -> handle_request_(OperationID, Req, ReqCtx) end
    ).

handle_request_(OperationID, Req, ReqCtx = #{auth_context := AuthCtx}) ->
    _ = logger:debug("Processing request: ~p", [OperationID]),
    try
        WoodyCtx = attach_deadline(Req, create_woody_context(Req, AuthCtx)),
        Context0 = create_processing_context(ReqCtx, WoodyCtx),
        Handlers = get_handlers(),
        case preprocess_request(OperationID, Req, Context0, Handlers) of
            {ok, PreprocessedContext, Handler} ->
                Context1 = add_preprocess_context(PreprocessedContext, Context0),
                case authorize_operation(Context1) of
                    {restricted, Restrictions} ->
                        Context2 = add_restrictions_context(Restrictions, Context1),
                        process_request(OperationID, Req, Context2, Handler)
                end;
            {error, no_impl} ->
                erlang:throw({handler_function_clause, OperationID})
        end
    catch
        throw:{bad_deadline, _Deadline} ->
            {ok, logic_error(invalidDeadline, <<"Invalid data in X-Request-Deadline header">>)};
        throw:{handler_function_clause, _OperationID} ->
            _ = logger:error("Operation ~p failed due to missing handler", [OperationID]),
            {error, {501, #{}, undefined}};
        throw:{invalidPartyID, _PartyID} ->
            {ok, logic_error(invalidPartyID, <<"Given party is either inaccessible or does not exist">>)};
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details);
        Class:Reason:Stacktrace ->
            process_general_error(Class, Reason, Stacktrace, OperationID, Req, ReqCtx)
    end.

-spec process_request(
    OperationID :: operation_id(),
    Req :: request_data(),
    Context :: processing_context(),
    Handler :: module()
) -> {ok | error, response()}.
process_request(OperationID, Req, Context, Handler) ->
    case Handler:process_request(OperationID, Req, Context) of
        {error, noimpl} ->
            erlang:throw({handler_function_clause, OperationID});
        Response ->
            Response
    end.

%%

create_processing_context(ReqCtx, WoodyCtx) ->
    #{
        woody_context => WoodyCtx,
        swagger_context => ReqCtx
    }.

create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => uac_authorizer_jwt:get_subject_id(AuthContext),
        realm => ?REALM,
        email => uac_authorizer_jwt:get_claim(<<"email">>, AuthContext, undefined),
        username => uac_authorizer_jwt:get_claim(<<"name">>, AuthContext, undefined)
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

process_woody_error(_Source, result_unexpected, _Details) ->
    {error, server_error(500)};
process_woody_error(_Source, resource_unavailable, _Details) ->
    {error, server_error(503)};
process_woody_error(_Source, result_unknown, _Details) ->
    {error, server_error(504)}.

process_general_error(Class, Reason, Stacktrace, OperationID, Req, SwagContext) ->
    _ = logger:error(
        "Operation ~p failed due to ~p:~p given req: ~p and context: ~p",
        [OperationID, Class, Reason, Req, SwagContext],
        #{
            error => #{
                class => genlib:to_binary(Class),
                reason => genlib:format(Reason),
                stack_trace => genlib_format:format_stacktrace(Stacktrace)
            }
        }
    ),
    {error, server_error(500)}.

-spec preprocess_request(
    OperationID :: operation_id(),
    Req :: request_data(),
    Context :: processing_context(),
    Handlers :: list(module())
) -> {ok, preprocess_context()} | {error, response() | noimpl}.
preprocess_request(_OperationID, _Req, _Context, []) ->
    {error, no_impl};
preprocess_request(OperationID, Req, Context, [Handler | Rest]) ->
    case Handler:preprocess_request(OperationID, Req, Context) of
        {error, noimpl} ->
            preprocess_request(OperationID, Req, Context, Rest);
        Response ->
            {ok, Response, Handler}
    end.

-spec add_preprocess_context(preprocess_context(), processing_context()) -> processing_context().
add_preprocess_context(PreprocessContext, Context) ->
    Context#{preprocess_context => PreprocessContext}.

-spec add_preprocess_context(restrictions_context(), processing_context()) -> processing_context().
add_restrictions_context(RestrictionsContext, Context) ->
    Context#{restrictions_context => RestrictionsContext}.

-spec authorize_operation(
    Context :: capi_handler:processing_context()
) -> resolution() | no_return().
authorize_operation(
    #{
        swagger_context := ReqCtx,
        woody_context := WoodyCtx,
        preprocess_context := PreprocessedContext
    }) ->
    case anapi_bouncer:extract_context_fragments(ReqCtx, WoodyCtx) of
        Fragments when Fragments /= undefined ->
            Fragments1 = anapi_bouncer_context:build(PreprocessedContext, Fragments),
            anapi_bouncer:judge(Fragments1, WoodyCtx);
        undefined ->
            forbidden
    end.
