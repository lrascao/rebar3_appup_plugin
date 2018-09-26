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
%% @spec init(rebar_state:t()) -> {'ok',rebar_state:t()}.
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
%% @spec do(rebar_state:t()) -> {'ok',rebar_state:t()} | {'error',string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
           end,
    %% find all .appup.src files in all project applications
    lists:foreach(fun(AppInfo) ->
                    process_app(AppInfo, State)
                  end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
%% @spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

process_app(AppInfo, State) ->
    Vsn = rebar_app_info:original_vsn(AppInfo),
    Sources = find_appup_src_files(AppInfo),
    lists:foreach(fun(Source) ->
                    case filelib:is_file(Source) of
                        true ->
                            %% this appup.src might pertain to some other
                            %% application dependency, check for that
                            Name = list_to_binary(filename:basename(Source, ".appup.src")),
                            SourceAppInfo = rebar3_appup_utils:find_app_info(Name, State),
                            process_appup_src(Source, SourceAppInfo, State, Vsn);
                        false -> ok
                    end
                  end, Sources).

process_appup_src(Source, AppInfo, State, Vsn) ->
    Target = appup_file_target(AppInfo),
    rebar_api:info("Compiling ~s to ~s",
        [filename:basename(Source), Target]),
    case template(Source, Vsn) of
        {ok, AppUpBin} ->
            %% allocate a temporary file and write the templated
            %% contents to it
            AppUpFile = rebar3_appup_utils:tmp_filename(),
            ok = file:write_file(AppUpFile, AppUpBin),
            case evaluate(AppUpFile, State) of
                {ok, AppupTerm} ->
                    compile(AppupTerm, Target);
                {error, Reason} ->
                    rebar_api:abort("failed to evaluate ~s (template ~p): ~p",
                        [AppUpFile, Source, Reason])
            end,
            %% leave no trash behind
            ok = file:delete(AppUpFile);
        {error, Reason} ->
            rebar_api:abort("unable to render template due to ~p",
                [Reason])
    end.

-type bs_vars() :: [{term(), term()}].
-spec bs(bs_vars()) -> bs_vars().
%% @spec bs(bs_vars()) -> bs_vars().
bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

%% @spec evaluate(binary() | string()) -> {'error',atom() | {integer(),atom() | tuple(),_}} | {'ok',_}.
evaluate(Source, State) ->
    file:script(Source, bs([{'STATE', State}])).

template(Source, Vsn) ->
    Context = [{"vsn", Vsn}],
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
            rebar_api:abort("Failed to compile not an appup:\n~p",
                [AppupTerm])
    end.

%% @spec find_appup_src_files(_) -> binary() | string().
find_appup_src_files(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    %% find all *.appup.src files in the application
    %% source directory
    filelib:wildcard(Dir ++ "/src/*.appup.src").

%% @spec appup_file_target(_) -> binary() | string().
appup_file_target(AppInfo) ->
    OutDir = rebar_app_info:ebin_dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join(OutDir, ec_cnv:to_list(Name) ++ ".appup").
