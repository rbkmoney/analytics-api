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

-export([get_subject_id/1]).
-export([get_claims/1]).
-export([get_claim/2]).
-export([get_claim/3]).

-export([get_operation_access/2]).
-export([get_consumer/1]).

-type consumer() :: client | merchant | provider.

-spec get_subject_id(uac:context()) -> binary().

get_subject_id({_Id, {SubjectID, _ACL}, _Claims}) ->
    SubjectID.

-spec get_claims(uac:context()) -> uac:claims().

get_claims({_Id, _Subject, Claims}) ->
    Claims.

-spec get_claim(binary(), uac:context()) -> term().

get_claim(ClaimName, {_Id, _Subject, Claims}) ->
    maps:get(ClaimName, Claims).

-spec get_claim(binary(), uac:context(), term()) -> term().

get_claim(ClaimName, {_Id, _Subject, Claims}, Default) ->
    maps:get(ClaimName, Claims, Default).

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
get_operation_access('GetReport'     , _) ->
    [{[party], read}];
get_operation_access('CreateReport'  , _) ->
    [{[party], write}];
get_operation_access('DownloadFile'  , _) ->
    [{[party], read}].

-spec get_consumer(uac:claims()) ->
    consumer().
get_consumer(Claims) ->
    case maps:get(<<"cons">>, Claims, <<"merchant">>) of
        <<"merchant">> -> merchant;
        <<"client"  >> -> client;
        <<"provider">> -> provider
    end.
