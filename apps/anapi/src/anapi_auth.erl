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

-module(anapi_auth).

-export([get_operation_access/2]).
-export([get_consumer/1]).
-export([get_access_config/0]).

-export([get_subject_id/1]).

-export([authorize_operation/2]).

-type consumer() :: client | merchant | provider.

-define(DOMAIN, <<"common-api">>).

%%

-type request_data() :: #{atom() | binary() => term()}.

-spec get_operation_access(swag_server:operation_id(), request_data()) -> [{uac_acl:scope(), uac_acl:permission()}].
get_operation_access('SearchInvoices', _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments', _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchRefunds', _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchPayouts', _) ->
    [{[party], read}];
get_operation_access('SearchReports', _) ->
    [{[party], read}];
get_operation_access('SearchChargebacks', _) ->
    [{[party], read}];
get_operation_access('GetReport', _) ->
    [{[party], read}];
get_operation_access('CreateReport', _) ->
    [{[party], write}];
get_operation_access('CancelReport', _) ->
    [{[party], write}];
get_operation_access('DownloadFile', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsToolDistribution', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsAmount', _) ->
    [{[party], read}];
get_operation_access('GetAveragePayment', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsCount', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsErrorDistribution', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsSplitAmount', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsSplitCount', _) ->
    [{[party], read}];
get_operation_access('GetRefundsAmount', _) ->
    [{[party], read}];
get_operation_access('GetCurrentBalances', _) ->
    [{[party], read}];
get_operation_access('GetPaymentsSubErrorDistribution', _) ->
    [{[party], read}];
get_operation_access('GetCurrentBalancesGroupByShop', _) ->
    [{[party], read}].

-spec get_consumer(uac:claims()) -> consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client">> -> client;
        <<"provider">> -> provider
    end.

-spec get_access_config() -> map().
get_access_config() ->
    #{
        domain_name => ?DOMAIN,
        resource_hierarchy => get_resource_hierarchy()
    }.

get_resource_hierarchy() ->
    #{
        invoices => #{
            payments => #{}
        },
        payments => #{},
        party => #{}
    }.

-spec get_subject_id(context()) -> binary().
get_subject_id({Claims, _}) ->
    uac_authorizer_jwt:get_subject_id(Claims).

-spec authorize_operation(
    Prototypes :: capi_bouncer_context:prototypes(),
    Context :: capi_handler:processing_context()
) -> resolution() | no_return().
authorize_operation([], _) ->
    undefined;
authorize_operation(Prototypes, #{swagger_context := ReqCtx, woody_context := WoodyCtx}) ->
    case anapi_bouncer:extract_context_fragments(ReqCtx, WoodyCtx) of
        Fragments when Fragments /= undefined ->
            Fragments1 = anapi_bouncer_context:build(Prototypes, Fragments, WoodyCtx),
            anapi_bouncer:judge(Fragments1, WoodyCtx);
        undefined ->
            undefined
    end.
