-module(yapi_client_accounts).

-export([get_account_by_id/2]).

-type context() :: yapi_client_lib:context().

-spec get_account_by_id(context(), integer()) -> {ok, term()} | {error, term()}.
get_account_by_id(Context, AccountID) ->
    Params = #{
        binding => #{
            <<"accountID">> => AccountID
        }
    },
    {Url, PreparedParams, Opts} = yapi_client_lib:make_request(Context, Params),
    Response = swag_client_accounts_api:get_account_by_id(Url, PreparedParams, Opts),
    yapi_client_lib:handle_response(Response).
