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
-module(rebar3_appup_clean).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
%% @spec init(rebar_state:t()) -> {'ok',rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, appup},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {opts, [                      % list of options understood by the plugin
            ]},
            {example, "rebar3 appup clean"},
            {short_desc, "Cleans all .appup files in the target dir"},
            {desc, "Appup clean"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
%% @spec do(rebar_state:t()) -> {'ok',rebar_state:t()} | {'error',string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
           end,
    lists:foreach(fun(AppInfo) ->
        Opts = rebar_app_info:opts(AppInfo),
        TargetDir = rebar_app_info:ebin_dir(AppInfo),
        rebar_base_compiler:run(Opts, [],
                                TargetDir, ".appup",
                                "", "",
                                fun(Source, _Target, _Config) ->
                                    rebar_api:debug("deleting ~s",
                                        [filename:basename(Source)]),
                                    rebar_file_utils:delete_each([Source])
                                end)
    end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
%% @spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
