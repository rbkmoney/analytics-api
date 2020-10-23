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

-module(anapi_utils).

-export([map_to_base64url/1]).

-export([parse_deadline/1]).

-export([to_universal_time/1]).
-export([get_process_metadata/0]).

-export([unwrap/1]).

% 5 min
-define(MAX_REQUEST_DEADLINE_TIME, timer:minutes(5)).

-spec map_to_base64url(map()) -> binary() | no_return().
map_to_base64url(Map) when is_map(Map) ->
    base64url:encode(jsx:encode(Map)).

-spec to_universal_time(Timestamp :: binary()) -> TimestampUTC :: binary().
to_universal_time(Timestamp) ->
    Microseconds = genlib_rfc3339:parse(Timestamp, microsecond),
    genlib_rfc3339:format_relaxed(Microseconds, microsecond).

-spec unwrap(ok | {ok, Value} | {error, _Error}) -> Value | no_return().
unwrap(ok) ->
    ok;
unwrap({ok, Value}) ->
    Value;
unwrap({error, Error}) ->
    erlang:error({unwrap_error, Error}).

-spec parse_deadline
    (binary()) -> {ok, woody:deadline()} | {error, bad_deadline};
    (undefined) -> {ok, undefined}.
parse_deadline(undefined) ->
    {ok, undefined};
parse_deadline(DeadlineStr) ->
    Parsers = [
        fun try_parse_woody_default/1,
        fun try_parse_relative/1
    ],
    try_parse_deadline(DeadlineStr, Parsers).

%%
%% Internals
%%
try_parse_deadline(_DeadlineStr, []) ->
    {error, bad_deadline};
try_parse_deadline(DeadlineStr, [P | Parsers]) ->
    case P(DeadlineStr) of
        {ok, _Deadline} = Result ->
            Result;
        {error, bad_deadline} ->
            try_parse_deadline(DeadlineStr, Parsers)
    end.

try_parse_woody_default(DeadlineStr) ->
    try
        Deadline = woody_deadline:from_binary(to_universal_time(DeadlineStr)),
        NewDeadline = clamp_max_deadline(woody_deadline:to_timeout(Deadline)),
        {ok, woody_deadline:from_timeout(NewDeadline)}
    catch
        error:{bad_deadline, _Reason} ->
            {error, bad_deadline};
        error:{badmatch, _} ->
            %% Catch calendar:rfc3339_to_system_time/2 error here
            {error, bad_deadline};
        error:deadline_reached ->
            {error, bad_deadline}
    end.

try_parse_relative(DeadlineStr) ->
    %% deadline string like '1ms', '30m', '2.6h' etc
    case re:split(DeadlineStr, <<"^(\\d+\\.\\d+|\\d+)([a-z]+)$">>) of
        [<<>>, NumberStr, Unit, <<>>] ->
            Number = genlib:to_float(NumberStr),
            try_parse_relative(Number, Unit);
        _Other ->
            {error, bad_deadline}
    end.

try_parse_relative(Number, Unit) ->
    case unit_factor(Unit) of
        {ok, Factor} ->
            Timeout = erlang:round(Number * Factor),
            {ok, woody_deadline:from_timeout(clamp_max_deadline(Timeout))};
        {error, _Reason} ->
            {error, bad_deadline}
    end.

unit_factor(<<"ms">>) ->
    {ok, 1};
unit_factor(<<"s">>) ->
    {ok, 1000};
unit_factor(<<"m">>) ->
    {ok, 1000 * 60};
unit_factor(_Other) ->
    {error, unknown_unit}.

clamp_max_deadline(Value) when is_integer(Value) ->
    MaxDeadline = genlib_app:env(anapi, max_request_deadline, ?MAX_REQUEST_DEADLINE_TIME),
    case Value > MaxDeadline of
        true ->
            MaxDeadline;
        false ->
            Value
    end.

-spec get_process_metadata() -> logger:metadata().
get_process_metadata() ->
    case logger:get_process_metadata() of
        undefined -> #{};
        Metadata -> Metadata
    end.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_universal_time_test() -> _.

to_universal_time_test() ->
    ?assertEqual(<<"2017-04-19T13:56:07Z">>, to_universal_time(<<"2017-04-19T13:56:07Z">>)),
    ?assertEqual(<<"2017-04-19T13:56:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53Z">>)),
    ?assertEqual(<<"2017-04-19T10:36:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53+03:20">>)),
    ?assertEqual(<<"2017-04-19T17:16:07.530Z">>, to_universal_time(<<"2017-04-19T13:56:07.53-03:20">>)).

-spec parse_deadline_test() -> _.
parse_deadline_test() ->
    Deadline = woody_deadline:from_timeout(3000),
    BinDeadline = woody_deadline:to_binary(Deadline),
    {ok, {_, _}} = parse_deadline(BinDeadline),
    ?assertEqual({error, bad_deadline}, parse_deadline(<<"2017-04-19T13:56:07.53Z">>)),
    {ok, {_, _}} = parse_deadline(<<"15s">>),
    {ok, {_, _}} = parse_deadline(<<"15m">>),
    {error, bad_deadline} = parse_deadline(<<"15h">>).

-endif.
