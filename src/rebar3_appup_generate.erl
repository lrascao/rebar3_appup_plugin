%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
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
-module(rebar3_appup_generate).

-export([init/1,
         do/1,
         format_error/1]).

%% exported for eunit
-export([matching_versions/2,
         merge_instructions/6]).

-define(PROVIDER, generate).
-define(DEPS, []).

-define(PRIV_DIR, "priv").
-define(APPUP_TEMPLATE, "templates/appup.tpl").
-define(APPUPFILEFORMAT, "%% appup generated for ~p by rebar3_appup_plugin (~p)~n"
        "{~p,\n\t[{~p, \n\t\t~p}], \n\t[{~p, \n\t\t~p}\n]}.~n").
-define(DEFAULT_RELEASE_DIR, "rel").
-define(DEFAULT_PRE_PURGE, brutal_purge).
-define(DEFAULT_POST_PURGE, brutal_purge).
-define(SUPPORTED_BEHAVIOURS, [gen_server,
                               gen_fsm,
                               gen_statem,
                               gen_event,
                               application,
                               supervisor]).

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
                {relname, $n, "relname", string, "Release name"},
                {previous, $p, "previous", string, "location of the previous release"},
                {previous_version, $p, "previous_version", string, "version of the previous release"},
                {current, $c, "current", string, "location of the current release"},
                {target_dir, $t, "target_dir", string, "target dir in which to generate the .appups to"},
                {purge, $g, "purge", string, "per-module semi-colon separated list purge type "
                                             "Module=PrePurge/PostPurge, reserved name default for "
                                             "modules that are unspecified:"
                                             "(eg. default=soft;m1=soft/brutal;m2=brutal)"
                                             "default is brutal"}
            ]},
            {example, "rebar3 appup generate"},
            {short_desc, "Compare two different releases and generate the .appup file"},
            {desc, "Appup generator"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
%% @spec do(rebar_state:t()) -> {'ok',rebar_state:t()} | {'error',string()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("opts: ~p", [Opts]),

    %% search for this plugin's appinfo in order to know
    %% where to look for the mustache templates
    Apps = rebar_state:all_plugin_deps(State),
    PluginInfo = rebar3_appup_utils:appup_plugin_appinfo(Apps),
    PluginDir = rebar_app_info:dir(PluginInfo),

    Name = rebar3_appup_utils:get_release_name(State),
    rebar_api:debug("release name: ~p", [Name]),

    %% check for overload of the current release
    CurrentRelPath = get_current_rel_path(State, Name),
    %% extract the current release data
    {CurrentName, CurrentVer} = rebar3_appup_rel_utils:get_rel_release_info(
                                            Name, CurrentRelPath),
    rebar_api:debug("current release, name: ~p, version: ~p",
        [CurrentName, CurrentVer]),

    %% if not specified the previous version is the current rel path
    PreviousRelPath = case proplists:get_value(previous, Opts, undefined) of
                        undefined -> CurrentRelPath;
                        P -> P
                      end,
    TargetDir = proplists:get_value(target_dir, Opts, undefined),
    rebar_api:debug("previous release: ~p", [PreviousRelPath]),
    rebar_api:debug("current release: ~p", [CurrentRelPath]),
    rebar_api:debug("target dir: ~p", [TargetDir]),

    %% deduce the previous version from the release path
    {PreviousName, _PreviousVer0} = rebar3_appup_rel_utils:get_rel_release_info(Name,
                                                                                PreviousRelPath),
    %% if a specific one was requested use that instead
    PreviousVer = case proplists:get_value(previous_version, Opts, undefined) of
                    undefined ->
                        deduce_previous_version(Name, CurrentVer,
                                                CurrentRelPath, PreviousRelPath);
                    V -> V
                  end,
    rebar_api:debug("previous release, name: ~p, version: ~p",
        [PreviousName, PreviousVer]),

    %% Run some simple checks
    true = rebar3_appup_utils:prop_check(CurrentVer =/= PreviousVer,
                      "current (~p) and previous (~p) release versions are the same",
                      [CurrentVer, PreviousVer]),
    true = rebar3_appup_utils:prop_check(CurrentName == PreviousName,
                      "current (~p) and previous (~p) release names are not the same",
                      [CurrentName, PreviousName]),

    %% Find all the apps that have been upgraded
    {AddApps0, UpgradeApps0, RemoveApps} = get_apps(Name,
                                                   PreviousRelPath, PreviousVer,
                                                   CurrentRelPath, CurrentVer,
                                                   State),

    %% Get a list of any appup files that exist in the current release
    CurrentAppUpFiles = rebar3_appup_utils:find_files_by_ext(
                            filename:join([CurrentRelPath, "lib"]),
                            ".appup"),
    %% Convert the list of appup files into app names
    CurrentAppUpApps = lists:usort([appup_info(File) || File <- CurrentAppUpFiles]),
    rebar_api:debug("apps that already have .appups: ~p",
        [CurrentAppUpApps]),

    %% Create a list of apps that don't already have appups
    UpgradeApps = gen_appup_which_apps(UpgradeApps0 ++ AddApps0, CurrentVer, CurrentAppUpApps),
    AddApps = gen_appup_which_apps(AddApps0, CurrentVer, CurrentAppUpApps),
    rebar_api:debug("generating .appup for apps: ~p",
        [AddApps ++ UpgradeApps ++ RemoveApps]),

    PurgeOpts0 = proplists:get_value(purge, Opts, []),
    PurgeOpts = parse_purge_opts(PurgeOpts0),

    AppupOpts = [{purge_opts, PurgeOpts},
                 {plugin_dir, PluginDir}],
    rebar_api:debug("appup opts: ~p", [AppupOpts]),

    %% Generate appup files for apps
    lists:foreach(fun(App) ->
                    generate_appup_files(TargetDir,
                                         CurrentRelPath, PreviousRelPath,
                                         App,
                                         AppupOpts, State)
                  end, AddApps ++ UpgradeApps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
%% @spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
%% @spec parse_purge_opts(maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | [])) -> [[] | {atom(),{_,_}}].
parse_purge_opts(Opts0) when is_list(Opts0) ->
    Opts1 = re:split(Opts0, ";"),
    lists:map(fun(Opt) ->
                case re:split(Opt, "=") of
                    [Module, PrePostPurge] ->
                        {PrePurge, PostPurge} =
                            case re:split(PrePostPurge, "/") of
                                [PrePurge0, PostPurge0] ->
                                    {PrePurge0, PostPurge0};
                                [PrePostPurge] ->
                                    {PrePostPurge, PrePostPurge}
                            end,
                        {list_to_atom(binary_to_list(Module)),
                          {purge_opt(PrePurge), purge_opt(PostPurge)}};
                    _ -> []
                end
              end, Opts1).

%% @spec purge_opt(<<_:32,_:_*16>>) -> 'brutal_purge' | 'soft_purge'.
purge_opt(<<"soft">>) -> soft_purge;
purge_opt(<<"brutal">>) -> brutal_purge.

%% @spec get_purge_opts(atom() | tuple(),[any()]) -> {_,_}.
get_purge_opts(Name, Opts) ->
    {DefaultPrePurge, DefaultPostPurge} = proplists:get_value(default, Opts,
                                                            {?DEFAULT_PRE_PURGE,
                                                             ?DEFAULT_POST_PURGE}),
    {PrePurge, PostPurge} = proplists:get_value(Name, Opts,
                                                {DefaultPrePurge, DefaultPostPurge}),
    {PrePurge, PostPurge}.

%% @spec deduce_previous_version(string(),_,atom() | binary() | [atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()]) -> any().
deduce_previous_version(Name, CurrentVersion, CurrentRelPath, PreviousRelPath) ->
    Versions = rebar3_appup_rel_utils:get_release_versions(Name, PreviousRelPath),
    case length(Versions) of
        N when N =:= 1 andalso CurrentRelPath =:= PreviousRelPath ->
            rebar_api:abort("only 1 version is present in ~p (~p) expecting at least 2",
                [PreviousRelPath, hd(Versions)]);
        %% the case below means the user requested the --previous option and there is exactly
        %% one release in that path, use that one
        N when N =:= 1 ->
            hd(Versions);
        %% there are two releases and the user didn't request an alternative previous
        %% release path, infer the old one
        N when N =:= 2 andalso CurrentRelPath =:= PreviousRelPath ->
            hd(Versions -- [CurrentVersion]);
        N when N >= 2 ->
            rebar_api:abort("more than 2 versions are present in ~p: (~.0p), please use the --previous_version "
                            "option to choose which version to upgrade from",
                            [PreviousRelPath, Versions])
    end.

%% @spec get_apps(string(),atom() | binary() | [atom() | [any()] | char()],[atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()],[atom() | [any()] | char()]) -> [any()].
get_apps(Name, OldVerPath, OldVer, NewVerPath, NewVer, State) ->
    OldApps0 = rebar3_appup_rel_utils:get_rel_apps(Name, OldVer, OldVerPath),
    OldApps = rebar3_appup_rel_utils:exclude_otp_apps(OldApps0, State),
    rebar_api:debug("previous version apps: ~p", [OldApps]),

    NewApps0 = rebar3_appup_rel_utils:get_rel_apps(Name, NewVer, NewVerPath),
    NewApps = rebar3_appup_rel_utils:exclude_otp_apps(NewApps0, State),
    rebar_api:debug("current version apps: ~p", [NewApps]),

    AddedApps = app_list_diff(NewApps, OldApps),
    Added = lists:map(fun(AppName) ->
                        AddedAppVer = proplists:get_value(AppName, NewApps),
                        {add, AppName, AddedAppVer}
                      end, AddedApps),
    rebar_api:debug("added: ~p", [Added]),

    Removed = lists:map(fun(AppName) ->
                            RemovedAppVer = proplists:get_value(AppName, OldApps),
                            {remove, AppName, RemovedAppVer}
                        end, app_list_diff(OldApps, NewApps)),
    rebar_api:debug("removed: ~p", [Removed]),

    Upgraded = lists:filtermap(fun(AppName) ->
                                OldAppVer = proplists:get_value(AppName, OldApps),
                                NewAppVer = proplists:get_value(AppName, NewApps),
                                case OldAppVer /= NewAppVer of
                                    true ->
                                        {true, {upgrade, AppName, {OldAppVer, NewAppVer}}};
                                    false -> false
                                end
                               end, proplists:get_keys(NewApps) -- AddedApps),
    rebar_api:debug("upgraded: ~p", [Upgraded]),
    {Added, Upgraded, Removed}.

%% @spec app_list_diff([any()],[any()]) -> [any()].
app_list_diff(List1, List2) ->
    List3 = lists:umerge(lists:sort(proplists:get_keys(List1)),
                         lists:sort(proplists:get_keys(List2))),
    List3 -- proplists:get_keys(List2).

%% @spec file_to_name(atom() | binary() | [atom() | [any()] | char()]) -> binary() | string().
file_to_name(File) ->
    filename:rootname(filename:basename(File)).

%% @spec appup_info(string()) -> {App :: atom(),
%%                                ToVersion :: string()}.
appup_info(File) ->
    {ok, [{ToVersion, _, _}]} = file:consult(File),
    {file_to_name(File), ToVersion}.

%% @spec gen_appup_which_apps([any()],[string()]) -> [any()].
gen_appup_which_apps(UpgradedApps0, CurrentVersion, [{App, Version} | Rest]) ->
    UpgradedApps = case CurrentVersion =:= Version of
                        true ->
                            lists:keydelete(list_to_atom(App), 2, UpgradedApps0);
                        false -> UpgradedApps0
                   end,
    gen_appup_which_apps(UpgradedApps, CurrentVersion, Rest);
gen_appup_which_apps(Apps, _CurrentVersion, []) ->
    Apps.

%% @spec generate_appup_files(_,atom() | binary() | [atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()],{'upgrade',_,{'undefined' | [any()],_}},[{'plugin_dir',_} | {'purge_opts',[any()]},...],_) -> 'ok'.
generate_appup_files(_, _, _, {upgrade, _App, {undefined, _}}, _, _) -> ok;
generate_appup_files(TargetDir,
                     _NewVerPath, _OldVerPath,
                     {add, App, Version},
                     Opts, State) ->

    UpgradeInstructions = [{add_application, App, permanent}],
    DowngradeInstructions = lists:reverse(lists:map(fun invert_instruction/1,
                                                    UpgradeInstructions)),
    ok = write_appup(App, ".*", Version, TargetDir,
                     UpgradeInstructions, DowngradeInstructions,
                     Opts, State),
    ok;
generate_appup_files(TargetDir,
                     NewVerPath, OldVerPath,
                     {upgrade, App, {OldVer, NewVer}},
                     Opts, State) ->
    OldRelEbinDir = filename:join([OldVerPath, "lib",
                                atom_to_list(App) ++ "-" ++ OldVer, "ebin"]),
    NewRelEbinDir = filename:join([NewVerPath, "lib",
                                atom_to_list(App) ++ "-" ++ NewVer, "ebin"]),

    {AddedFiles, DeletedFiles, ChangedFiles} = beam_lib:cmp_dirs(NewRelEbinDir,
                                                                 OldRelEbinDir),
    rebar_api:debug("beam files:", []),
    rebar_api:debug("   added: ~p", [AddedFiles]),
    rebar_api:debug("   deleted: ~p", [DeletedFiles]),
    rebar_api:debug("   changed: ~p", [ChangedFiles]),

    %% generate a module dependency tree
    ModDeps = module_dependencies(AddedFiles ++ ChangedFiles),
    rebar_api:debug("deps: ~p", [ModDeps]),

    Added = lists:map(fun(File) ->
                        generate_instruction(add_module, ModDeps, File, Opts)
                      end, AddedFiles),
    Deleted = lists:map(fun(File) ->
                            generate_instruction(delete_module, ModDeps, File, Opts)
                        end, DeletedFiles),
    Changed = lists:map(fun(File) ->
                            generate_instruction(upgrade, ModDeps, File, Opts)
                        end, ChangedFiles),

    UpgradeInstructions0 = lists:append([Added, Changed, Deleted]),
    %% check for updated supervisors, we'll need to check their child spec
    %% and see if any childs were added or removed
    UpgradeInstructions1 = apply_supervisor_child_updates(UpgradeInstructions0,
                                                          Added, Deleted,
                                                          OldRelEbinDir, NewRelEbinDir),
    UpgradeInstructions = lists:flatten(UpgradeInstructions1),
    rebar_api:debug("upgrade instructions: ~p", [UpgradeInstructions]),
    DowngradeInstructions0 = lists:reverse(lists:map(fun invert_instruction/1,
                                                     UpgradeInstructions1)),
    DowngradeInstructions = lists:flatten(DowngradeInstructions0),
    rebar_api:debug("downgrade instructions: ~p", [DowngradeInstructions]),

    ok = write_appup(App, OldVer, NewVer, TargetDir,
                     UpgradeInstructions, DowngradeInstructions,
                     Opts, State),
    ok.

%% @spec module_dependencies([string() | {[any()],[any()]}]) -> [{atom(),[any()]}].
module_dependencies(Files) ->
    %% build a unique list of directories holding the supplied files
    Dirs0 = lists:map(fun({File, _}) ->
                            filename:dirname(File);
                         (File) ->
                            filename:dirname(File)
                      end, Files),
    Dirs = lists:usort(Dirs0),
    %% start off xref
    {ok, _} = xref:start(xref),
    %% add each of the directories to the xref path

    lists:foreach(fun(Dir) ->
                    {ok, _} = xref:add_directory(xref, Dir)
                  end, Dirs),
    Mods = [list_to_atom(file_to_name(F)) || {F, _} <- Files],
    module_dependencies(Mods, Mods, []).

%% @spec module_dependencies([atom()],[atom()],[{atom(),[any()]}]) -> [{atom(),[any()]}].
module_dependencies([], _Mods, Acc) ->
    xref:stop(xref),
    Acc;
module_dependencies([Mod | Rest], Mods, Acc) ->
    {ok, Deps0} = xref:analyze(xref, {module_call, Mod}),
    %% remove self
    Deps1 = Deps0 -- [Mod],
    %% intersect with modules being changed
    Set0 = sets:from_list(Deps1),
    Set1 = sets:from_list(Mods),
    Deps = sets:to_list(sets:intersection(Set0, Set1)),
    module_dependencies(Rest, Mods, Acc ++ [{Mod, Deps}]).

%% @spec write_appup(atom(),_,_,atom() | binary() | [atom() | [any()] | char()],[any()],[{'add_module',_} | {'apply',{_,_,_}} | {'delete_module',_} | {'remove_application',_} | {'add_application',_,'permanent'} | {'update',_,'supervisor'} | {'load_module',_,_,_,_} | {'update',_,{_,_},_,_,_}],[{'plugin_dir',_} | {'purge_opts',[any()]},...],_) -> 'ok'.
write_appup(App, OldVer, NewVer, TargetDir,
            UpgradeInstructions0, DowngradeInstructions0,
            Opts, State) ->
    CurrentBaseDir = rebar_dir:base_dir(State),
    %% check for the app either in deps or lib
    rebar_api:info("current base dir: ~p", [CurrentBaseDir]),
    CheckoutsEbinDir = filename:join([rebar_dir:checkouts_dir(State),
                                      atom_to_list(App), "ebin"]),
    DepsEbinDir = filename:join([CurrentBaseDir, "deps",
                                atom_to_list(App), "ebin"]),
    LibEbinDir = filename:join([CurrentBaseDir, "lib",
                                atom_to_list(App), "ebin"]),
    AppEbinDir = case {filelib:is_dir(DepsEbinDir),
                       filelib:is_dir(LibEbinDir),
                       filelib:is_dir(CheckoutsEbinDir)} of
                    {true, _, _} -> DepsEbinDir;
                    {_, true, _} -> LibEbinDir;
                    {_, _, true} -> CheckoutsEbinDir;
                    {_, _, _} -> undefined
                 end,
    rebar_api:info("app ~p ebin dir: ~p",
        [App, AppEbinDir]),
    AppUpFiles = case TargetDir of
                    undefined ->
                        case AppEbinDir of
                            undefined ->
                                %% if we couldn't find any app ebin dir
                                %% then don't bother generating any app
                                rebar_api:warn("unable to generate appup for non-existing application ~p",
                                             [App]),
                                [];
                            _ ->
                                EbinAppup = filename:join([AppEbinDir,
                                                           atom_to_list(App) ++ ".appup"]),
                                [EbinAppup]
                        end;
                    _ ->
                        [filename:join([TargetDir, atom_to_list(App) ++ ".appup"])]
                 end,

    rebar_api:debug(
        "Upgrade instructions before merging with .appup.pre.src and "
        ".appup.post.src files: ~p",
        [UpgradeInstructions0]),
    rebar_api:debug(
        "Downgrade instructions before merging with .appup.pre.src and "
        ".appup.post.src files: ~p",
        [DowngradeInstructions0]),
    {UpgradeInstructions, DowngradeInstructions} =
        merge_instructions(AppUpFiles, UpgradeInstructions0, DowngradeInstructions0, OldVer, NewVer),
    rebar_api:debug(
        "Upgrade instructions after merging with .appup.pre.src and "
        ".appup.post.src files:\n~p\n",
        [UpgradeInstructions]),
    rebar_api:debug(
        "Downgrade instructions after merging with .appup.pre.src and "
        ".appup.post.src files:\n~p\n",
        [DowngradeInstructions]),

    {ok, AppupTemplate} = file:read_file(filename:join([proplists:get_value(plugin_dir, Opts),
                                                        ?PRIV_DIR, ?APPUP_TEMPLATE])),
    %% write each of the .appup files
    lists:foreach(fun(AppUpFile) ->
                    AppupCtx = [{"app", App},
                                {"now", rebar3_appup_utils:now_str()},
                                {"new_vsn", NewVer},
                                {"old_vsn", OldVer},
                                {"upgrade_instructions",
                                    io_lib:fwrite("~.9p", [UpgradeInstructions])},
                                {"downgrade_instructions",
                                    io_lib:fwrite("~.9p", [DowngradeInstructions])}],
                    EscFun = fun(X) -> X end,
                    AppUp = bbmustache:render(AppupTemplate, AppupCtx, [{escape_fun, EscFun}]),
                    rebar_api:info("Generated appup (~p <-> ~p) for ~p in ~p",
                        [OldVer, NewVer, App, AppUpFile]),
                    ok = file:write_file(AppUpFile, AppUp)
                  end, AppUpFiles),
    ok.

%% @spec generate_instruction('add_module' | 'delete_module' | 'upgrade',[{atom(),[any()]}],atom() | binary() | [atom() | [any()] | char()] | {atom() | binary() | string() | tuple(),_},[{'plugin_dir',_} | {'purge_opts',[any()]},...]) -> {'delete_module',atom()} | {'add_module',atom(),_} | {'update',atom() | tuple(),'supervisor'} | {'load_module',atom() | tuple(),_,_,_} | {'update',atom() | tuple(),{'advanced',[]},_,_,_}.
generate_instruction(add_module, ModDeps, File, _Opts) ->
    Name = list_to_atom(file_to_name(File)),
    Deps = proplists:get_value(Name, ModDeps, []),
    {add_module, Name, Deps};
generate_instruction(delete_module, ModDeps, File, _Opts) ->
    Name = list_to_atom(file_to_name(File)),
    _Deps = proplists:get_value(Name, ModDeps, []),
    % TODO: add dependencies to delete_module, fixed in OTP commit a4290bb3
    % {delete_module, Name, Deps};
    {delete_module, Name};
%generate_instruction(added_application, Application, _, _Opts) ->
%    {add_application, Application, permanent};
%generate_instruction(removed_application, Application, _, _Opts) ->
%    {remove_application, Application};
%generate_instruction(restarted_application, Application, _, _Opts) ->
%    {restart_application, Application};
generate_instruction(upgrade, ModDeps, {File, _}, Opts) ->
    {ok, {Name, List}} = beam_lib:chunks(File, [attributes, exports]),
    Behavior = get_behavior(List),
    CodeChange = is_code_change(List),
    Deps = proplists:get_value(Name, ModDeps, []),
    generate_instruction_advanced(Name, Behavior, CodeChange, Deps, Opts).

%% @spec generate_instruction_advanced(atom() | tuple(),_,'code_change' | 'undefined',_,[{'plugin_dir',_} | {'purge_opts',[any()]},...]) -> {'update',atom() | tuple(),'supervisor'} | {'load_module',atom() | tuple(),_,_,_} | {'update',atom() | tuple(),{'advanced',[]},_,_,_}.
generate_instruction_advanced(Name, undefined, undefined, Deps, Opts) ->
    PurgeOpts = proplists:get_value(purge_opts, Opts, []),
    {PrePurge, PostPurge} = get_purge_opts(Name, PurgeOpts),
    %% Not a behavior or code change, assume purely functional
    {load_module, Name, PrePurge, PostPurge, Deps};
generate_instruction_advanced(Name, supervisor, _, _, _Opts) ->
    %% Supervisor
    {update, Name, supervisor};
generate_instruction_advanced(Name, _, code_change, Deps, Opts) ->
    PurgeOpts = proplists:get_value(purge_opts, Opts, []),
    {PrePurge, PostPurge} = get_purge_opts(Name, PurgeOpts),
    %% Includes code_change export
    {update, Name, {advanced, []}, PrePurge, PostPurge, Deps};
generate_instruction_advanced(Name, _, _, Deps, Opts) ->
    PurgeOpts = proplists:get_value(purge_opts, Opts, []),
    {PrePurge, PostPurge} = get_purge_opts(Name, PurgeOpts),
    %% Anything else
    {load_module, Name, PrePurge, PostPurge, Deps}.

generate_supervisor_child_instruction(new, Mod, Worker) ->
    [{update, Mod, supervisor},
     {apply, {supervisor, restart_child, [Mod, Worker]}}];
generate_supervisor_child_instruction(remove, Mod, Worker) ->
    [{apply, {supervisor, terminate_child, [Mod, Worker]}},
     {apply, {supervisor, delete_child, [Mod, Worker]}},
     {update, Mod, supervisor}].

invert_instruction({load_module, Name, PrePurge, PostPurge, Deps}) ->
    {load_module, Name, PrePurge, PostPurge, Deps};
invert_instruction({add_module, Name, _Deps}) ->
    % TODO: add dependencies to delete_module, fixed in OTP commit a4290bb3
    % {delete_module, Name, Deps};
    {delete_module, Name};
invert_instruction({delete_module, Name}) ->
    % TODO: add dependencies to add_module, fixed in OTP commit a4290bb3
    % {add_module, Name, Deps};
    {add_module, Name};
invert_instruction({add_application, Application, permanent}) ->
    {remove_application, Application};
invert_instruction({remove_application, Application}) ->
    {add_application, Application, permanent};
invert_instruction({update, Name, supervisor}) ->
    {update, Name, supervisor};
invert_instruction({update, Name, {advanced, []}, PrePurge, PostPurge, Deps}) ->
    {update, Name, {advanced, []}, PrePurge, PostPurge, Deps};
invert_instruction([{update, Name, supervisor},
                    {apply, {supervisor, restart_child, [Sup, Worker]}}]) ->
    [{apply, {supervisor, terminate_child, [Sup, Worker]}},
     {apply, {supervisor, delete_child, [Sup, Worker]}},
     {update, Name, supervisor}];
invert_instruction([{apply, {supervisor, terminate_child, [Sup, Worker]}},
                    {apply, {supervisor, delete_child, [Sup, Worker]}},
                    {update, Name, supervisor}]) ->
    [{update, Name, supervisor},
     {apply, {supervisor, restart_child, [Sup, Worker]}}].


%% @spec get_behavior([{'abstract_code' | 'atoms' | 'attributes' | 'compile_info' | 'exports' | 'imports' | 'indexed_imports' | 'labeled_exports' | 'labeled_locals' | 'locals' | [any(),...],'no_abstract_code' | binary() | [any()] | {_,_}}]) -> any().
get_behavior(List) ->
    Attributes = proplists:get_value(attributes, List),
    case proplists:get_value(behavior, Attributes, []) ++
         proplists:get_value(behaviour, Attributes, []) of
        [] -> undefined;
        Bs -> select_behaviour(
                lists:sort(
                  drop_unknown_behaviours(Bs)))
    end.

drop_unknown_behaviours(Bs) ->
    drop_unknown_behaviours(Bs, []).

drop_unknown_behaviours([], Acc) -> Acc;
drop_unknown_behaviours([B|Rest], Acc0) ->
    Acc = case supported_behaviour(B) of
            true -> [B|Acc0];
            false -> Acc0
          end,
    drop_unknown_behaviours(Rest, Acc).

supported_behaviour(B) ->
    lists:member(B, ?SUPPORTED_BEHAVIOURS).

select_behaviour([]) -> undefined;
select_behaviour([B]) -> B;
%% apply the supervisor upgrade when a module is both it and application
select_behaviour([application, supervisor]) -> supervisor.

%% @spec is_code_change([{'abstract_code' | 'atoms' | 'attributes' | 'compile_info' | 'exports' | 'imports' | 'indexed_imports' | 'labeled_exports' | 'labeled_locals' | 'locals' | [any(),...],'no_abstract_code' | binary() | [any()] | {_,_}}]) -> 'code_change' | 'undefined'.
is_code_change(List) ->
    Exports = proplists:get_value(exports, List),
    case proplists:is_defined(code_change, Exports) orelse
        proplists:is_defined(system_code_change, Exports) of
        true ->
            code_change;
        false ->
            undefined
    end.

apply_supervisor_child_updates(Instructions, Added, Deleted,
                               OldRelEbinDir, NewRelEbinDir) ->
    apply_supervisor_child_updates(Instructions, Added, Deleted,
                                   OldRelEbinDir, NewRelEbinDir, []).

apply_supervisor_child_updates([], _, _, _, _, Acc) -> Acc;
apply_supervisor_child_updates([{update, Name, supervisor} | Rest],
                               Added, Deleted,
                               OldRelEbinDir, NewRelEbinDir, Acc) ->
    OldSupervisorSpec = get_supervisor_spec(Name, OldRelEbinDir),
    NewSupervisorSpec = get_supervisor_spec(Name, NewRelEbinDir),
    rebar_api:debug("old supervisor spec: ~p",
            [OldSupervisorSpec]),
    rebar_api:debug("new supervisor spec: ~p",
            [NewSupervisorSpec]),
    Diff = diff_supervisor_spec(OldSupervisorSpec,
                                NewSupervisorSpec),
    NewWorkers = proplists:get_value(new_workers, Diff),
    RemovedWorkers = proplists:get_value(removed_workers, Diff),
    rebar_api:debug("supervisor workers added: ~p",
            [NewWorkers]),
    rebar_api:debug("supervisor workers removed: ~p",
            [RemovedWorkers]),
    AddInstructions = [generate_supervisor_child_instruction(new, Name, N) ||
                        N <- NewWorkers],
    RemoveInstructions = [generate_supervisor_child_instruction(remove, Name, R) ||
                            R <- RemovedWorkers],
    Instructions = ensure_supervisor_update(Name, AddInstructions ++ RemoveInstructions),
    apply_supervisor_child_updates(Rest, Added, Deleted,
                                   OldRelEbinDir, NewRelEbinDir,
                                   Acc ++ Instructions);
apply_supervisor_child_updates([Else | Rest],
                               Added, Deleted,
                               OldRelEbinDir, NewRelEbinDir, Acc) ->
    apply_supervisor_child_updates(Rest, Added, Deleted,
                                   OldRelEbinDir, NewRelEbinDir, Acc ++ [Else]).

ensure_supervisor_update(Name, []) ->
    [{update, Name, supervisor}];
 ensure_supervisor_update(_, Instructions) ->
    Instructions.

get_supervisor_spec(Module, EbinDir) ->
    Beam = rebar3_appup_utils:beam_rel_path(EbinDir, atom_to_list(Module)),
    {module, Module} = rebar3_appup_utils:load_module_from_beam(Beam, Module),
    {ok, Arg} = guess_supervisor_init_arg(Module, Beam),
    rebar_api:debug("supervisor init arg: ~p", [Arg]),
    Spec = case catch Module:init(Arg) of
            {ok, S} -> S;
            _ ->
                rebar_api:info("could not obtain supervisor ~p spec, unable to generate "
                               "supervisor appup instructions", [Module]),
                undefined
           end,
    rebar3_appup_utils:unload_module_from_beam(Beam, Module),
    Spec.

diff_supervisor_spec({_, Spec1}, {_, Spec2}) ->
    Workers1 = supervisor_spec_workers(Spec1, []),
    Workers2 = supervisor_spec_workers(Spec2, []),
    [{new_workers, Workers2 -- Workers1},
     {removed_workers, Workers1 -- Workers2}];
diff_supervisor_spec(_, _) ->
    [{new_workers, []}, {removed_workers, []}].

supervisor_spec_workers([], Acc) -> Acc;
supervisor_spec_workers([{_, {Mod, _F, _A}, _, _, worker, _} | Rest], Acc) ->
    supervisor_spec_workers(Rest, Acc ++ [Mod]);
supervisor_spec_workers([#{start := {Mod, _F, _A}, type := worker} | Rest], Acc) ->
    supervisor_spec_workers(Rest, Acc ++ [Mod]);
supervisor_spec_workers([_ | Rest], Acc) ->
    supervisor_spec_workers(Rest, Acc).

guess_supervisor_init_arg(Module, Beam) ->
    %% obtain the abstract code and from that try and guess what
    %% are valid arguments for the supervisor init/1 method
    Forms =  case rebar3_appup_utils:get_abstract_code(Module, Beam) of
                no_abstract_code=E ->
                    {error, E};
                encrypted_abstract_code=E ->
                    {error, E};
                {raw_abstract_v1, Code} ->
                    epp:interpret_file_attribute(Code)
              end,
    {ok, AbsArg} = get_supervisor_init_arg_abstract(Forms),
    rebar_api:debug("supervisor abstract init arg: ~p", [AbsArg]),
    Arg = generate_supervisor_init_arg(AbsArg),
    {ok, Arg}.

get_supervisor_init_arg_abstract(Forms) ->
    [L] = lists:filtermap(fun({function, _, init, 1, [Clause]}) ->
                            %% currently not supporting more that one function clause
                            %% for the Mod:init/1 supervisor callback
                            %% extract the argument from the function clause
                            {clause, _, [Arg], _, _} = Clause,
                            {true, Arg};
                           (_) -> false
                        end, Forms),
    {ok, L}.

generate_supervisor_init_arg({nil, _}) -> [];
generate_supervisor_init_arg({var, _, _}) -> undefined;
generate_supervisor_init_arg({cons, _, Head, Rest}) ->
    [generate_supervisor_init_arg(Head) | generate_supervisor_init_arg(Rest)];
generate_supervisor_init_arg({integer, _, Value}) -> Value;
generate_supervisor_init_arg({string, _, Value}) -> Value;
generate_supervisor_init_arg({atom, _, Value}) -> Value;
generate_supervisor_init_arg({tuple, _, Elements}) ->
    L = [generate_supervisor_init_arg(Element) || Element <- Elements],
    Tuple0 = generate_tuple(length(L)),
    {Tuple, _} = lists:foldl(fun(E, {T0, Index}) ->
                                T1 = erlang:setelement(Index, T0, E),
                                {T1, Index + 1}
                             end, {Tuple0, 1}, L),
    Tuple;
generate_supervisor_init_arg(_) -> undefined.


generate_tuple(1) -> {undefined};
generate_tuple(2) -> {undefined, undefined};
generate_tuple(3) -> {undefined, undefined, undefined};
generate_tuple(4) -> {undefined, undefined, undefined, undefined};
generate_tuple(5) -> {undefined, undefined, undefined, undefined, undefined};
generate_tuple(6) -> {undefined, undefined, undefined, undefined,
                      undefined, undefined};
generate_tuple(7) -> {undefined, undefined, undefined, undefined,
                      undefined, undefined, undefined};
generate_tuple(8) -> {undefined, undefined, undefined, undefined,
                      undefined, undefined, undefined, undefined}.

-spec get_current_rel_path(State, Name) -> Res when
      State :: rebar_state:t(),
      Name :: string(),
      Res :: list().
get_current_rel_path(State, Name) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(current, Opts, undefined) of
        undefined ->
            filename:join([rebar_dir:base_dir(State),
                           ?DEFAULT_RELEASE_DIR,
                           Name]);
        Path -> Path
    end.

%%------------------------------------------------------------------------------
%%
%% Add pre and post instructions to the instuctions created by appup generate.
%% These instructions must be stored in the .appup.pre.src and .appup.post.src
%% files in the src folders of the given application.
%%
%% If one of these files are missing or the version patterns specified in
%% these files don't match the current old and new versions stored in the
%% .appup file the corresponding part will be empty.
%%
%% Example:
%%
%% Generated relapp.appup
%% %% appup generated for relapp by rebar3_appup_plugin (2018/01/10 14:35:19)
%% {"1.0.34",
%%   [{ "1.0.33",
%%     [{apply,{io,format,["Upgrading is in progress..."]}}]}],
%%   [{ "1.0.33",
%%     [{apply,{io,format,["Downgrading is in progress..."]}}]}],
%% }.
%%
%% relapp.appup.pre.src:
%%
%% {"1.0.34",
%%   [{"1.*",
%%      [{apply, {io, format, ["Upgrading started from 1.* to 1.0.34"]}}]},
%%    {"1.0.33",
%%      [{apply, {io, format, ["Upgrading started from 1.0.33 to 1.0.34"]}}]}],
%%   [{".*",
%%     [{apply, {io, format, ["Downgrading started from 1.0.34 to .*"]}}]},
%%    {"1.0.33",
%%     [{apply, {io, format, ["Downgrading started from 1.0.34 to 1.0.33"]}}]}]
%% }.
%%
%% relapp.appup.post.src:
%%
%% {"1.0.34",
%%   [{"1.*",
%%      [{apply, {io, format, ["Upgrading finished from 1.* to 1.0.34"]}}]},
%%    {"1.0.33",
%%      [{apply, {io, format, ["Upgrading finished from 1.0.33 to 1.0.34"]}}]}],
%%   [{".*",
%%      [{apply, {io, format, ["Downgrading finished from 1.0.034 to .*"]}}]},
%%    {"1.0.33",
%%      [{apply, {io, format, ["Downgrading finished from 1.0.34 to 1.0.33"]}}]}]
%% }.
%%
%% The final relapp.appup file after merging the pre and post contents:
%%
%% %% appup generated for relapp by rebar3_appup_plugin (2018/01/10 14:35:19)
%% {"1.0.34",
%%   [{"1.0.33",
%%     [{apply,{io,format,["Upgrading started from 1.* to 1.0.34"]}},
%%      {apply,{io,format,["Upgrading started from 1.0.33 to 1.0.34"]}},
%%      {apply,{io,format,["Upgrading is in progress..."]}},
%%      {apply,{io,format,["Upgrading finished from 1.* to 1.0.34"]}},
%%      {apply,{io,format,["Upgrading finished from 1.0.33 to 1.0.34"]}}] }],
%%   [{"1.0.33",
%%     [{apply,{io,format,["Downgrading started from 1.0.34 to .*"]}},
%%      {apply,{io,format,["Downgrading started from 1.0.34 to 1.0.33"]}},
%%      {apply,{io,format,["Downgrading is in progress..."]}},
%%      {apply,{io,format,["Downgrading finished from 1.0.34 to .*"]}},
%%      {apply,{io,format,["Downgrading finished from 1.0.34 to 1.0.33"]}}] }]
%% }.
%%
%%------------------------------------------------------------------------------
-spec merge_instructions(AppupFiles, UpgradeInstructions, DowngradeInstructions,
                         OldVer, NewVer) -> Res when
      AppupFiles :: [] | [string()],
      UpgradeInstructions :: list(tuple()),
      DowngradeInstructions :: list(tuple()),
      OldVer :: string(),
      NewVer :: string(),
      Res :: {list(tuple()), list(tuple())}.
merge_instructions([] = _AppupFiles, UpgradeInstructions, DowngradeInstructions,
                   _OldVer, _NewVer) ->
    {UpgradeInstructions, DowngradeInstructions};
merge_instructions([AppUpFile], UpgradeInstructions, DowngradeInstructions,
                   OldVer, NewVer) ->
    [_, _ | AppRootDir0] = lists:reverse(string_compat:tokens(AppUpFile, "/")),
    AppRootDir = filename:join(["/" | lists:reverse(AppRootDir0)]),
    AppupPrePath = find_file_by_ext(AppRootDir, ".appup.pre.src"),
    AppupPostPath = find_file_by_ext(AppRootDir, ".appup.post.src"),
    rebar_api:debug(".appup.pre.src path: ~p",
                    [AppupPrePath]),
    rebar_api:debug("appup.post.src path: ~p",
                    [AppupPostPath]),
    PreContents = read_pre_post_contents(AppupPrePath),
    PostContents = read_pre_post_contents(AppupPostPath),
    rebar_api:debug(".appup.pre.src contents: ~p",
                    [PreContents]),
    rebar_api:debug(".appup.post.src contents: ~p",
                    [PostContents]),
    merge_instructions(PreContents, PostContents, OldVer, NewVer,
                        UpgradeInstructions, DowngradeInstructions).

-spec merge_instructions(PreContents, PostContents, OldVer, NewVer,
                          UpgradeInstructions, DowngradeInstructions) -> Res when
      PreContents :: undefined | {string(), list(), list()},
      PostContents :: undefined | {string(), list(), list()},
      OldVer :: string(),
      NewVer :: string(),
      UpgradeInstructions :: list(tuple()),
      DowngradeInstructions :: list(tuple()),
      Res :: {list(tuple()), list(tuple())}.
merge_instructions(PreContents, PostContents, OldVer, NewVer,
                    UpgradeInstructions, DowngradeInstructions) ->
    {merge_pre_post_instructions(PreContents, PostContents, upgrade, OldVer,
                                 NewVer, UpgradeInstructions),
     merge_pre_post_instructions(PreContents, PostContents, downgrade, OldVer,
                                 NewVer, DowngradeInstructions)}.

-spec merge_pre_post_instructions(PreContents, PostContents, Direction, OldVer,
                                  NewVer, Instructions) -> Res when
      PreContents :: undefined | {string(), list(), list()},
      PostContents :: undefined | {string(), list(), list()},
      Direction :: upgrade | downgrade,
      OldVer :: string(),
      NewVer :: string(),
      Instructions :: list(tuple()),
      Res :: list(tuple()).
merge_pre_post_instructions(PreContents, PostContents, Direction, OldVer,
                            NewVer, Instructions) ->
    expand_instructions(PreContents, Direction, OldVer, NewVer) ++
    Instructions ++
    expand_instructions(PostContents, Direction, OldVer, NewVer).

-spec read_pre_post_contents(Path) -> Res when
      Path :: undefined | string(),
      Res :: undefined | tuple().
read_pre_post_contents(undefined) ->
    undefined;
read_pre_post_contents(Path) ->
    {ok, [Contents]} = file:consult(Path),
    Contents.

-spec expand_instructions(ExtFileContents, Direction, OldVer, NewVer) ->
    Res when
      ExtFileContents :: undefined | {string(), list(), list()},
      Direction :: upgrade | downgrade,
      OldVer :: string(),
      NewVer :: string(),
      Res :: list(tuple()).
expand_instructions(undefined, _Direction, _OldVer, _NewVer) ->
    [];
expand_instructions({VersionPattern, UpInsts, DownInsts}, Direction, OldVer,
                    NewVer) ->
    case matching_versions(VersionPattern, NewVer) of
        true ->
            Instructions = case Direction of
                               upgrade -> UpInsts;
                               downgrade -> DownInsts
                           end,
            expand_instructions(Instructions, OldVer, []);
        false ->
            []
    end.

%%------------------------------------------------------------------------------
%% Check if pattern in the first parameter matches the given version.
%% matching_versions("1.*", "1.0.34") -> true
%% matching_versions(".*", "1.0.34") -> true
%%------------------------------------------------------------------------------
-spec matching_versions(Pattern, Version) -> Res when
      Pattern :: string(),
      Version :: string(),
      Res :: boolean().
matching_versions(Pattern, Version) ->
    PatternParts = string_compat:tokens(Pattern, "."),
    PatternParts1 = expand_pattern_parts(PatternParts, []),
    VersionParts = string_compat:tokens(Version, "."),
    Res = is_matching_versions(PatternParts1, VersionParts),
    rebar_api:debug("Checking if pattern '~s' matches version '~s': ~p",
                    [Pattern, Version, Res]),
    Res.

is_matching_versions([], _) ->
    true;
is_matching_versions(["*" | PatternParts], [_ | VersionParts]) ->
    is_matching_versions(PatternParts, VersionParts);
is_matching_versions([PatternPart | PatternTail], [VersionPart | VersionTail])
  when PatternPart =:= VersionPart ->
    is_matching_versions(PatternTail, VersionTail);
is_matching_versions(_PatternParts, _VersionParts) ->
    false.

-spec expand_pattern_parts(Parts, Acc) -> Res when
      Parts :: list(string()),
      Acc :: list(string()),
      Res :: list(string()).
expand_pattern_parts([], Acc) ->
    lists:reverse(Acc);
expand_pattern_parts([P | T], Acc) when P =:= "*"; P =:= "" ->
    expand_pattern_parts(T, ["*" | Acc]);
expand_pattern_parts([P | T], Acc) ->
    expand_pattern_parts(T, [P | Acc]).

-spec expand_instructions(Instructions, Version, Acc) -> Res when
      Instructions :: list({string(), list(tuple())}),
      Version :: string(),
      Acc :: list(tuple()),
      Res :: list(tuple()).
expand_instructions([], _OldVersion, Acc) ->
    Acc;
expand_instructions([{Pattern, Insts} | T], OldVersion, Acc0) ->
    Acc = case matching_versions(Pattern, OldVersion) of
              true ->
                  Acc0 ++ Insts;
              false ->
                  Acc0
          end,
    expand_instructions(T, OldVersion, Acc).

find_file_by_ext(Dir, Ext) ->
    case rebar3_appup_utils:find_files_by_ext(Dir, Ext) of
        [] ->
            undefined;
        [Path] ->
            Path
    end.
