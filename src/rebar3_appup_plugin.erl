%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Luis Rascão.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(rebar3_appup_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
%% @spec init(rebar_state:t()) -> {'ok',rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_appup_generate:init(State0),
    {ok, State2} = rebar3_appup_compile:init(State1),
    {ok, State3} = rebar3_appup_clean:init(State2),
    {ok, State4} = rebar3_appup_tar:init(State3),
    {ok, State4}.
