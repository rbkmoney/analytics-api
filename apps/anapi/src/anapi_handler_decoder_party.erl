-module(anapi_handler_decoder_party).

-export([decode_residence/1]).

%%

-spec decode_residence(atom() | undefined) ->
    binary().

decode_residence(undefined) ->
    undefined;
decode_residence(Residence) when is_atom(Residence) ->
    list_to_binary(string:to_upper(atom_to_list(Residence))).
