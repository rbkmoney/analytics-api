-module(yapi_client_tokens).

-export([create_payment_resource/2]).

-type context() :: yapi_client_lib:context().

-spec create_payment_resource(context(), map()) -> {ok, #{binary() => _}} | {error, term()}.
create_payment_resource(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = yapi_client_lib:make_request(Context, Params),
    Response = swag_client_tokens_api:create_payment_resource(Url, PreparedParams, Opts),
    yapi_client_lib:handle_response(Response).
