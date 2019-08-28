-module(capi_msgp_marshalling).

-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

%% API
-export([marshal/1]).
-export([unmarshal/1]).

%%

-type value() :: term().

-spec marshal(value()) ->
    dmsl_msgpack_thrift:'Value'() | no_return().

marshal(undefined) ->
    {nl, #msgpack_Nil{}};
marshal(Boolean) when is_boolean(Boolean) ->
    {b, Boolean};
marshal(Integer) when is_integer(Integer) ->
    {i, Integer};
marshal(Float) when is_float(Float) ->
    {flt, Float};
marshal(String) when is_binary(String) ->
    {str, String};
marshal({bin, Binary}) ->
    {bin, Binary};
marshal(Object) when is_map(Object) ->
    {obj, maps:fold(
        fun(K, V, Acc) ->
            maps:put(marshal(K), marshal(V), Acc)
        end,
        #{},
        Object
    )};
marshal(Array) when is_list(Array) ->
    {arr, lists:map(fun marshal/1, Array)}.

-spec unmarshal(dmsl_msgpack_thrift:'Value'()) ->
    value().

unmarshal({nl, #msgpack_Nil{}}) ->
    undefined;
unmarshal({b, Boolean}) ->
    Boolean;
unmarshal({i, Integer}) ->
    Integer;
unmarshal({flt, Float}) ->
    Float;
unmarshal({str, String}) ->
    String;
unmarshal({bin, Binary}) ->
    {bin, Binary};
unmarshal({obj, Object}) ->
    maps:fold(fun(K, V, Acc) -> maps:put(unmarshal(K), unmarshal(V), Acc) end, #{}, Object);
unmarshal({arr, Array}) ->
    lists:map(fun unmarshal/1, Array).

