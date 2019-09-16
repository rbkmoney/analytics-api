-module(anapi_auth).

-export([authorize_api_key/2]).
-export([authorize_operation/3]).

-export([get_subject_id/1]).
-export([get_claims/1]).
-export([get_claim/2]).
-export([get_claim/3]).
-export([get_consumer/1]).

-export([get_resource_hierarchy/0]).

-type context () :: anapi_authorizer_jwt:t().
-type claims  () :: anapi_authorizer_jwt:claims().
-type consumer() :: client | merchant | provider.

-export_type([context /0]).
-export_type([claims  /0]).
-export_type([consumer/0]).

-spec authorize_api_key(
    OperationID :: swag_server:operation_id(),
    ApiKey      :: swag_server:api_key()
) -> {true, Context :: context()} | false.

authorize_api_key(OperationID, ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, {Type, Credentials}} ->
            case authorize_api_key(OperationID, Type, Credentials) of
                {ok, Context} ->
                    {true, Context};
                {error, Error} ->
                    _ = log_auth_error(OperationID, Error),
                    false
            end;
        {error, Error} ->
            _ = log_auth_error(OperationID, Error),
            false
    end.

log_auth_error(OperationID, Error) ->
    logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]).

-spec parse_api_key(ApiKey :: swag_server:api_key()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_api_key(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec authorize_api_key(
    OperationID :: swag_server:operation_id(),
    Type :: atom(),
    Credentials :: binary()
) ->
    {ok, Context :: context()} | {error, Reason :: atom()}.

authorize_api_key(_OperationID, bearer, Token) ->
    % NOTE
    % We are knowingly delegating actual request authorization to the logic handler
    % so we could gather more data to perform fine-grained access control.
    anapi_authorizer_jwt:verify(Token).

%%

% TODO
% We need shared type here, exported somewhere in swagger app
-type request_data() :: #{atom() | binary() => term()}.

-spec authorize_operation(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Auth :: anapi_authorizer_jwt:t()
) ->
    ok | {error, unauthorized}.

authorize_operation(OperationID, Req, {{_SubjectID, ACL}, _}) ->
    Access = get_operation_access(OperationID, Req),
    case lists:all(
        fun ({Scope, Permission}) ->
            lists:member(Permission, anapi_acl:match(Scope, ACL))
        end,
        Access
    ) of
        true ->
            ok;
        false ->
            {error, unauthorized}
    end.

%%

-spec get_subject_id(context()) -> binary().

get_subject_id({{SubjectID, _ACL}, _}) ->
    SubjectID.

-spec get_claims(context()) -> claims().

get_claims({_Subject, Claims}) ->
    Claims.

-spec get_claim(binary(), context()) -> term().

get_claim(ClaimName, {_Subject, Claims}) ->
    maps:get(ClaimName, Claims).

-spec get_claim(binary(), context(), term()) -> term().

get_claim(ClaimName, {_Subject, Claims}, Default) ->
    maps:get(ClaimName, Claims, Default).

%%

-spec get_operation_access(swag_server:operation_id(), request_data()) ->
    [{anapi_acl:scope(), anapi_acl:permission()}].

get_operation_access('SearchInvoices'            , _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments'            , _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchRefunds'             , _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchPayouts'             , _) ->
    [{[party], read}].

-spec get_resource_hierarchy() -> #{atom() => map()}.

get_resource_hierarchy() ->
    #{
        party               => #{invoice_templates => #{invoice_template_invoices => #{}}},
        customers           => #{bindings => #{}},
        invoices            => #{payments => #{}},
        payment_resources   => #{},
        payouts             => #{}
    }.

-spec get_consumer(claims()) ->
    consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client"  >> -> client;
        <<"provider">> -> provider
    end.
