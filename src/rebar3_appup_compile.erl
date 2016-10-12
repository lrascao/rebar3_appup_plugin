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
-module(rebar3_appup_compile).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, appup},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {opts, []},                   % list of options understood by the plugin
            {example, "rebar3 appup compile"},
            {short_desc, "Compile and validate all .appup.src files"},
            {desc, "Appup compile"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
           end,
    lists:foreach(fun(AppInfo) ->
                    Source0 = appup_file_src(AppInfo),
                    case filelib:is_file(Source0) of
                        true ->
                            rebar_api:info("Compiling ~s",
                                [filename:basename(Source0)]),
                            Target = appup_file_target(AppInfo),
                            case template(Source0, AppInfo) of
                                {ok, AppUpBin} ->
                                    %% allocate a temporary file and write the templated
                                    %% contents to it
                                    AppUpFile = rebar3_appup_utils:tmp_filename(),
                                    ok = file:write_file(AppUpFile, AppUpBin),
                                    case evaluate(AppUpFile) of
                                        {ok, AppupTerm} ->
                                            compile(AppupTerm, Target);
                                        {error, Reason} ->
                                            rebar_api:abort("failed to evaluate ~s (template ~p): ~p",
                                                [AppUpFile, Source0, Reason])
                                    end,
                                    %% leave no trash behind
                                    ok = file:delete(AppUpFile);
                                {error, Reason} ->
                                    rebar_api:abort("unable to render template due to ~p",
                                        [Reason])
                            end;
                        false -> ok
                    end
                  end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
-type bs_vars() :: [{term(), term()}].
-spec bs(bs_vars()) -> bs_vars().
bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

evaluate(Source) ->
    file:script(Source, bs([])).

template(Source, AppInfo) ->
    Context = [{"vsn", rebar_app_info:original_vsn(AppInfo)}],
    {ok, Template} = file:read_file(Source),
    case catch bbmustache:render(Template, Context) of
        B when is_binary(B) -> {ok, B};
        Error -> {error, Error}
    end.

compile(AppupTerm, Target) ->
    %% Perform basic validation on the appup term
    %% i.e. if basic appup structure exists.
    case AppupTerm of
        %% The .appup syntax is described in
        %% http://erlang.org/doc/man/appup.html.
        {_Vsn, UpFromVsn, DownToVsn}
          when is_list(UpFromVsn), is_list(DownToVsn) ->
            case file:write_file(
                   Target,
                   lists:flatten(io_lib:format("~p.", [AppupTerm]))) of
                {error, Reason} ->
                    rebar_api:abort("Failed writing to target file ~s due to ~s",
                           [Target, Reason]);
                ok ->
                    ok
            end;
        _ ->
            rebar_api:abort("Failed to compile not an appup:\n~p~n",
                [AppupTerm])
    end.

appup_file_src(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join([Dir, "src", ec_cnv:to_list(Name) ++ ".appup.src"]).

appup_file_target(AppInfo) ->
    OutDir = rebar_app_info:ebin_dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join(OutDir, ec_cnv:to_list(Name) ++ ".appup").
