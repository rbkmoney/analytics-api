-module(anapi_handler_decoder_utils).

-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-export([decode_last_digits/1]).
-export([decode_masked_pan/2]).
-export([decode_operation_failure/2]).
-export([decode_context/1]).

-export_type([decode_data/0]).

-type decode_data() :: #{binary() => term()}.

-define(PAN_LENGTH, 16).

-spec decode_masked_pan(binary(), binary()) ->
    binary().

decode_masked_pan(Bin, LastDigits) ->
    Mask = binary:copy(<<"*">>, ?PAN_LENGTH - byte_size(Bin) - byte_size(LastDigits)),
    <<Bin/binary, Mask/binary, LastDigits/binary>>.

-define(MASKED_PAN_MAX_LENGTH, 4).

-spec decode_last_digits(binary()) ->
    binary().

decode_last_digits(MaskedPan) when byte_size(MaskedPan) > ?MASKED_PAN_MAX_LENGTH ->
    binary:part(MaskedPan, {byte_size(MaskedPan), -?MASKED_PAN_MAX_LENGTH});
decode_last_digits(MaskedPan) ->
    MaskedPan.

-spec decode_operation_failure(_, _) ->
    decode_data().

decode_operation_failure({operation_timeout, _}, _) ->
    logic_error(timeout, <<"timeout">>);
decode_operation_failure({failure, #domain_Failure{code = Code, reason = Reason}}, _) ->
    logic_error(Code, Reason).

logic_error(Code, Message) ->
    #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

-spec decode_context(anapi_handler_encoder:encode_data()) ->
    decode_data() | undefined.

decode_context(#'Content'{type = <<"application/json">>, data = InvoiceContext}) ->
    % @TODO deal with non json contexts
    jsx:decode(InvoiceContext,  [return_maps]);
decode_context(undefined) ->
    undefined.
