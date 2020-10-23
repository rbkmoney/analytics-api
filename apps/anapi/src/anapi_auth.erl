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

-type consumer() :: client | merchant | provider.

%%

-type request_data() :: #{atom() | binary() => term()}.

-spec get_operation_access(swag_server:operation_id(), request_data()) ->
    [{uac_acl:scope(), uac_acl:permission()}].

get_operation_access('SearchInvoices', _) ->
    [{[invoices], read}];
get_operation_access('SearchPayments', _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchRefunds' , _) ->
    [{[invoices, payments], read}];
get_operation_access('SearchPayouts' , _) ->
    [{[party], read}];
get_operation_access('SearchReports' , _) ->
    [{[party], read}];
get_operation_access('SearchChargebacks' , _) ->
    [{[party], read}];

get_operation_access('GetReport'     , _) ->
    [{[party], read}];
get_operation_access('CreateReport'  , _) ->
    [{[party], write}];
get_operation_access('CancelReport'  , _) ->
    [{[party], write}];
get_operation_access('DownloadFile'  , _) ->
    [{[party], read}];

get_operation_access('GetPaymentsToolDistribution'  , _) ->
    [{[party], read}];
get_operation_access('GetPaymentsAmount'  , _) ->
    [{[party], read}];
get_operation_access('GetAveragePayment'  , _) ->
    [{[party], read}];
get_operation_access('GetPaymentsCount'  , _) ->
    [{[party], read}];
get_operation_access('GetPaymentsErrorDistribution'  , _) ->
    [{[party], read}];
get_operation_access('GetPaymentsSplitAmount'  , _) ->
    [{[party], read}];
get_operation_access('GetPaymentsSplitCount'  , _) ->
    [{[party], read}];
get_operation_access('GetRefundsAmount'  , _) ->
    [{[party], read}];
get_operation_access('GetCurrentBalances'  , _) ->
    [{[party], read}];
get_operation_access('GetPaymentsSubErrorDistribution'  , _) ->
    [{[party], read}];
get_operation_access('GetCurrentBalancesGroupByShop'  , _) ->
    [{[party], read}].

-spec get_consumer(uac:claims()) ->
    consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client"  >> -> client;
        <<"provider">> -> provider
    end.

-spec get_access_config() -> map().

get_access_config() ->
    #{
        domain_name => <<"common-api">>,
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
