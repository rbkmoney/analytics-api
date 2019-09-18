-module(anapi_handler_utils).

-export([logic_error/2]).
-export([server_error/1]).
-export([format_request_errors/1]).

-export([service_call/2]).

-export([get_auth_context/1]).
-export([get_party_id/1]).

-export([merge_and_compact/2]).
-export([get_time/2]).

-export([create_dsl/3]).

-type processing_context() :: anapi_handler:processing_context().
-type response()           :: anapi_handler:response().

-spec logic_error
    (term(), io_lib:chars() | binary()) -> response();
    (term(), {binary(), binary() | undefined}) -> response().

logic_error(Code, Message) ->
    Data = #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)},
    create_error_resp(400, Data).

create_error_resp(Code, Data) ->
    create_erorr_resp(Code, #{}, Data).
create_erorr_resp(Code, Headers, Data) ->
    {Code, Headers, Data}.

-spec server_error(integer()) ->
    {integer(), #{}, <<>>}.

server_error(Code) when Code >= 500 andalso Code < 600 ->
    {Code, #{}, <<>>}.

-spec format_request_errors(list()) ->
    binary().

format_request_errors([]    ) -> <<>>;
format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

%%%

-spec service_call({atom(), atom(), list()}, processing_context()) ->
    woody:result().

service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
    anapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

-spec get_auth_context(processing_context()) ->
    any().

get_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

-spec get_party_id(processing_context()) ->
    binary().

get_party_id(Context) ->
    anapi_auth:get_subject_id(get_auth_context(Context)).

-spec merge_and_compact(map(), map()) ->
    map().

merge_and_compact(M1, M2) ->
    genlib_map:compact(maps:merge(M1, M2)).

-spec get_time(term(), map()) ->
    TimestampUTC :: binary() | undefined.

get_time(Key, Req) ->
    case genlib_map:get(Key, Req) of
        Timestamp when is_binary(Timestamp) ->
            anapi_utils:to_universal_time(Timestamp);
        undefined ->
            undefined
    end.

-spec create_dsl(atom(), map(), map()) ->
    map().

create_dsl(QueryType, QueryBody, QueryParams) ->
    merge_and_compact(
        #{<<"query">> => maps:put(genlib:to_binary(QueryType), genlib_map:compact(QueryBody), #{})},
        QueryParams
    ).
