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

-module(anapi_handler_decoder_invoicing).

-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-export([decode_invoice_cart/1]).
-export([decode_payment_operation_failure/2]).
-export([decode_recurrent_parent/1]).
-export([decode_make_recurrent/1]).

-type processing_context() :: anapi_handler:processing_context().

%%
-spec decode_payment_operation_failure({atom(), _}, processing_context()) ->
    anapi_handler_decoder_utils:decode_data().

decode_payment_operation_failure({operation_timeout, _}, _) ->
    payment_error(<<"timeout">>);
decode_payment_operation_failure({failure, Failure}, Context) ->
    case anapi_auth:get_consumer(anapi_auth:get_claims(anapi_handler_utils:get_auth_context(Context))) of
        merchant ->
            % чтобы не городить ещё один обход дерева как в payproc_errors проще отформатировать в текст,
            % а потом уже в json
            decode_payment_operation_failure_(
                binary:split(erlang:list_to_binary(payproc_errors:format_raw(Failure)), <<":">>, [global])
            )
    end.

decode_payment_operation_failure_([H|T]) ->
    R = payment_error(H),
    case T of
        [] -> R;
        _  -> R#{<<"subError">> => decode_payment_operation_failure_(T)}
    end.

-spec decode_make_recurrent(undefined | boolean()) ->
    boolean().

decode_make_recurrent(undefined) ->
    false;
decode_make_recurrent(Value) when is_boolean(Value) ->
    Value.

-spec decode_recurrent_parent(anapi_handler_encoder:encode_data()) ->
    anapi_handler_decoder_utils:decode_data().

decode_recurrent_parent(#domain_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    };
decode_recurrent_parent(#merchstat_RecurrentParentPayment{invoice_id = InvoiceID, payment_id = PaymentID}) ->
    #{
        <<"invoiceID">> => InvoiceID,
        <<"paymentID">> => PaymentID
    }.

payment_error(Code) ->
    #{<<"code">> => Code}.

-spec decode_invoice_cart(anapi_handler_encoder:encode_data() | undefined) ->
    anapi_handler_decoder_utils:decode_data() | undefined.

decode_invoice_cart(#domain_InvoiceCart{lines = Lines}) ->
    [decode_invoice_line(L) || L <- Lines];
decode_invoice_cart(undefined) ->
    undefined.

decode_invoice_line(InvoiceLine = #domain_InvoiceLine{quantity = Quantity, price = #domain_Cash{amount = Price}}) ->
    genlib_map:compact(#{
        <<"product" >> => InvoiceLine#domain_InvoiceLine.product,
        <<"quantity">> => Quantity,
        <<"price"   >> => Price,
        <<"cost"    >> => Price * Quantity,
        <<"taxMode" >> => decode_invoice_line_tax_mode(InvoiceLine#domain_InvoiceLine.metadata)
    }).

-spec decode_invoice_line_tax_mode(map()) ->
    anapi_handler_decoder_utils:decode_data() | undefined.

decode_invoice_line_tax_mode(#{<<"TaxMode">> := {str, TM}}) ->
    %% for more info about taxMode look here:
    %% https://github.com/rbkmoney/starrys/blob/master/docs/settings.md
    #{
       <<"type">> => <<"InvoiceLineTaxVAT">>,
       <<"rate">> => TM
    };
decode_invoice_line_tax_mode(_) ->
    undefined.
