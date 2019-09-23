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

-module(anapi_cors_policy).
-behaviour(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allowed_headers/2]).
-export([allowed_methods/2]).

-spec policy_init(cowboy_req:req()) -> {ok, cowboy_req:req(), any()}.

policy_init(Req) ->
    {ok, Req, undefined_state}.

-spec allowed_origins(cowboy_req:req(), any()) -> {'*', any()}.

allowed_origins(_, State) ->
    {'*', State}.

-spec allowed_headers(cowboy_req:req(), any()) -> {[binary()], any()}.

allowed_headers(_, State) ->
    {[
        <<"access-control-allow-headers">>,
        <<"x-requested-with">>,
        <<"content-type">>,
        <<"accept">>,
        <<"authorization">>,
        <<"x-request-id">>,
        <<"x-request-deadline">>
    ], State}.

-spec allowed_methods(cowboy_req:req(), any()) -> {[binary()], any()}.

allowed_methods(_, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], State}.
