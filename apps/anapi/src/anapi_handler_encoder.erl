-module(anapi_handler_encoder).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_merch_stat_thrift.hrl").

-export([encode_stat_request/2]).

-export_type([encode_data/0]).

-type encode_data()  :: tuple().

-spec encode_stat_request(map() | binary(), binary() | undefined) ->
    encode_data().

encode_stat_request(Dsl, ContinuationToken) when is_map(Dsl) ->
    encode_stat_request(jsx:encode(Dsl), ContinuationToken);

encode_stat_request(Dsl, ContinuationToken) when is_binary(Dsl) ->
    #merchstat_StatRequest{
        dsl = Dsl,
        continuation_token = ContinuationToken
    }.
