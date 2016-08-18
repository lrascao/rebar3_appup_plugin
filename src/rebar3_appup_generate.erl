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

-define(PROVIDER, generate).
-define(DEPS, []).

-define(PRIV_DIR, "priv").
-define(APPUP_TEMPLATE, "templates/appup.tpl").
-define(APPUPFILEFORMAT, "%% appup generated for ~p by rebar3_appup_plugin (~p)~n"
        "{~p,\n\t[{~p, \n\t\t~p}], \n\t[{~p, \n\t\t~p}\n]}.~n").
-define(DEFAULT_RELEASE_DIR, "rel").
-define(DEFAULT_PRE_PURGE, brutal_purge).
-define(DEFAULT_POST_PURGE, brutal_purge).

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
            {opts, [                      % list of options understood by the plugin
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
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    rebar_api:debug("opts: ~p~n", [Opts]),

    %% search for this plugin's appinfo in order to know
    %% where to look for the mustache templates
    Apps = rebar_state:all_plugin_deps(State),
    PluginInfo = rebar3_appup_utils:appup_plugin_appinfo(Apps),
    PluginDir = rebar_app_info:dir(PluginInfo),

    RelxConfig = rebar_state:get(State, relx, []),
    {release, {Name0, _Ver}, _} = lists:keyfind(release, 1, RelxConfig),
    Name = atom_to_list(Name0),
    rebar_api:debug("release name: ~p", [Name]),

    %% previous is a mandatory option
    CurrentRelPath = case proplists:get_value(current, Opts, undefined) of
                        undefined ->
                            filename:join([rebar_dir:base_dir(State),
                                           ?DEFAULT_RELEASE_DIR,
                                           Name]);
                        Path -> Path
                     end,
    %% if not specified the previous version if the current rel path
    PreviousRelPath = case proplists:get_value(previous, Opts, undefined) of
                        undefined -> CurrentRelPath;
                        P -> P
                      end,
    TargetDir = proplists:get_value(target_dir, Opts, undefined),
    rebar_api:debug("previous release: ~p~n", [PreviousRelPath]),
    rebar_api:debug("current release: ~p~n", [CurrentRelPath]),
    rebar_api:debug("target dir: ~p~n", [TargetDir]),

    {CurrentName, CurrentVer} = rebar3_appup_rel_utils:get_rel_release_info(
                                            Name, CurrentRelPath),
    rebar_api:debug("current release, name: ~p, version: ~p",
        [CurrentName, CurrentVer]),

    %% deduce the previous version from the release path
    {PreviousName, _PreviousVer0} = rebar3_appup_rel_utils:get_rel_release_info(Name,
                                                                               PreviousRelPath),
    %% if a specific one was requested use that instead
    PreviousVer = case proplists:get_value(previous_version, Opts, undefined) of
                    undefined ->
                        deduce_previous_version(Name, CurrentVer, PreviousRelPath);
                    V -> V
                  end,
    rebar_api:debug("previous release, name: ~p, version: ~p",
        [PreviousName, PreviousVer]),

    %% Run some simple checks
    true = rebar3_appup_utils:prop_check(CurrentVer =/= PreviousVer,
                      "current (~p) and previous (~p) .rel versions match",
                      [CurrentVer, PreviousVer]),
    true = rebar3_appup_utils:prop_check(CurrentName == PreviousName,
                      "current (~p) and previous (~p) release names do not match",
                      [CurrentName, PreviousName]),

    %% Find all the apps that have been upgraded
    Upgraded = get_apps(Name,
                        PreviousRelPath, PreviousVer,
                        CurrentRelPath, CurrentVer),

    %% Get a list of any appup files that exist in the current release
    CurrentAppUpFiles = rebar3_appup_utils:find_files_by_ext(
                            filename:join([CurrentRelPath, "lib"]),
                            ".appup"),
    %% Convert the list of appup files into app names
    CurrentAppUpApps = [file_to_name(File) || File <- CurrentAppUpFiles],
    rebar_api:debug("apps that already have .appups: ~p", [CurrentAppUpApps]),

    %% Create a list of apps that don't already have appups
    UpgradeApps = gen_appup_which_apps(Upgraded, CurrentAppUpApps),
    rebar_api:debug("generating .appup for apps: ~p", [UpgradeApps]),

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
                  end, UpgradeApps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
parse_purge_opts(Opts0) when is_list(Opts0) ->
    Opts1 = re:split(Opts0, ";"),
    lists:map(fun(Opt) ->
                case re:split(Opt, "=") of
                    [Module, PrePostPurge] ->
                        case re:split(PrePostPurge, "/") of
                            [PrePurge, PostPurge] -> ok;
                            [PrePostPurge] ->
                                PrePurge = PrePostPurge,
                                PostPurge = PrePostPurge
                        end,
                        {list_to_atom(binary_to_list(Module)),
                          {purge_opt(PrePurge), purge_opt(PostPurge)}};
                    _ -> []
                end
              end, Opts1).

purge_opt(<<"soft">>) -> soft_purge;
purge_opt(<<"brutal">>) -> brutal_purge.

get_purge_opts(Name, Opts) ->
    {DefaultPrePurge, DefaultPostPurge} = proplists:get_value(default, Opts,
                                                            {?DEFAULT_PRE_PURGE,
                                                             ?DEFAULT_POST_PURGE}),
    {PrePurge, PostPurge} = proplists:get_value(Name, Opts,
                                                {DefaultPrePurge, DefaultPostPurge}),
    {PrePurge, PostPurge}.

deduce_previous_version(Name, CurrentVersion, RelPath) ->
    Versions = rebar3_appup_rel_utils:get_release_versions(Name, RelPath),
    case length(Versions) of
        N when N =:= 1 ->
            rebar_api:abort("only 1 version is present in ~p (~p) expecting at least 2",
                [RelPath, Versions]);
        N when N =:= 2 ->
            hd(Versions -- [CurrentVersion]);
        N when N > 2 ->
            rebar_api:abort("more than 2 versions are present in ~p, please use the --previous_version "
                            "option to choose which version to upgrade from: ~p", [RelPath, Versions])
    end.

get_apps(Name, OldVerPath, OldVer, NewVerPath, NewVer) ->
    OldApps = rebar3_appup_rel_utils:get_rel_apps(Name, OldVer, OldVerPath),
    rebar_api:debug("previous version apps: ~p~n", [OldApps]),

    NewApps = rebar3_appup_rel_utils:get_rel_apps(Name, NewVer, NewVerPath),
    rebar_api:debug("current version apps: ~p~n", [NewApps]),

    AddedApps = app_list_diff(NewApps, OldApps),
    rebar_api:debug("added: ~p", [AddedApps]),
    % Added = lists:map(fun(AppName) ->
    %                     NewAppVer = proplists:get_value(AppName, NewApps),
    %                     {add, AppName, NewAppVer}
    %                   end, AddedApps),

    % Removed = lists:map(fun(AppName) ->
    %                         OldAppVer = proplists:get_value(AppName, OldApps),
    %                         {remove, AppName, OldAppVer}
    %                     end, app_list_diff(OldApps, NewApps)),
    rebar_api:debug("removed: ~p", [app_list_diff(OldApps, NewApps)]),

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
    Upgraded.

app_list_diff(List1, List2) ->
    List3 = lists:umerge(lists:sort(proplists:get_keys(List1)),
                         lists:sort(proplists:get_keys(List2))),
    List3 -- proplists:get_keys(List2).

file_to_name(File) ->
    filename:rootname(filename:basename(File)).

gen_appup_which_apps(UpgradedApps, [First|Rest]) ->
    List = proplists:delete(list_to_atom(First), UpgradedApps),
    gen_appup_which_apps(List, Rest);
gen_appup_which_apps(Apps, []) ->
    Apps.

generate_appup_files(_, _, _, {upgrade, _App, {undefined, _}}, _, _) -> ok;
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

    %% generate a module dependency tree
    ModDeps = module_dependencies(AddedFiles ++ DeletedFiles ++ ChangedFiles),
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

    UpgradeInstructions = lists:append([Added, Deleted, Changed]),
    DowngradeInstructions = lists:reverse(lists:map(fun invert_instruction/1,
                                                    UpgradeInstructions)),

    ok = write_appup(App, OldVer, NewVer, TargetDir,
                     UpgradeInstructions, DowngradeInstructions,
                     Opts, State),
    ok.

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

write_appup(App, OldVer, NewVer, TargetDir,
            UpgradeInstructions, DowngradeInstructions,
            Opts, State) ->
    CurrentBaseDir = rebar_dir:base_dir(State),
    %% check for the app either in deps or lib
    DepsEbinDir = filename:join([CurrentBaseDir, "deps",
                                atom_to_list(App), "ebin"]),
    LibEbinDir = filename:join([CurrentBaseDir, "lib",
                                atom_to_list(App), "ebin"]),
    AppEbinDir = case {filelib:is_dir(DepsEbinDir),
                       filelib:is_dir(LibEbinDir)} of
                    {true, _} -> DepsEbinDir;
                    {_, true} -> LibEbinDir;
                    {_, _} -> undefined
                 end,
    AppUpFiles = case TargetDir of
                    undefined ->
                        EbinAppup = filename:join([AppEbinDir,
                                                   atom_to_list(App) ++ ".appup"]),
                        [EbinAppup];
                    _ ->
                        [filename:join([TargetDir, atom_to_list(App) ++ ".appup"])]
                 end,

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
                    AppUp = bbmustache:render(AppupTemplate, AppupCtx),
                    ok = file:write_file(AppUpFile, AppUp),
                    rebar_api:info("Generated appup (~p <-> ~p) for ~p in ~p",
                        [OldVer, NewVer, App, AppUpFile])
                  end, AppUpFiles),
    ok.

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
generate_instruction(added_application, Application, _, _Opts) ->
    {add_application, Application, permanent};
generate_instruction(removed_application, Application, _, _Opts) ->
    {remove_application, Application};
generate_instruction(restarted_application, Application, _, _Opts) ->
    {restart_application, Application};
generate_instruction(upgrade, ModDeps, {File, _}, Opts) ->
    {ok, {Name, List}} = beam_lib:chunks(File, [attributes, exports]),
    Behavior = get_behavior(List),
    CodeChange = is_code_change(List),
    Deps = proplists:get_value(Name, ModDeps, []),
    generate_instruction_advanced(Name, Behavior, CodeChange, Deps, Opts).

generate_instruction_advanced(Name, undefined, undefined, Deps, Opts) ->
    PurgeOpts = proplists:get_value(purge_opts, Opts, []),
    {PrePurge, PostPurge} = get_purge_opts(Name, PurgeOpts),
    %% Not a behavior or code change, assume purely functional
    {load_module, Name, PrePurge, PostPurge, Deps};
generate_instruction_advanced(Name, [supervisor], _, _, _Opts) ->
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

invert_instruction({load_module, Name, PrePurge, PostPurge, Deps}) ->
    {load_module, Name, PrePurge, PostPurge, Deps};
invert_instruction({add_module, Name, _Deps}) ->
    % TODO: add dependencies to delete_module, fixed in OTP commit a4290bb3
    % {delete_module, Name, Deps};
    {delete_module, Name};
invert_instruction({delete_module, Name}) ->
    % TODO: add dependencies to delete_module, fixed in OTP commit a4290bb3
    % {add_module, Name, Deps};
    {add_module, Name};
invert_instruction({add_application, Application, permanent}) ->
    {remove_application, Application};
invert_instruction({remove_application, Application}) ->
    {add_application, Application, permanent};
invert_instruction({update, Name, supervisor}) ->
    {update, Name, supervisor};
invert_instruction({update, Name, {advanced, []}, PrePurge, PostPurge, Deps}) ->
    {update, Name, {advanced, []}, PrePurge, PostPurge, Deps}.

get_behavior(List) ->
    Attributes = proplists:get_value(attributes, List),
    case proplists:get_value(behavior, Attributes) of
        undefined -> proplists:get_value(behaviour, Attributes);
        Else -> Else
    end.

is_code_change(List) ->
    Exports = proplists:get_value(exports, List),
    case proplists:is_defined(code_change, Exports) orelse
        proplists:is_defined(system_code_change, Exports) of
        true ->
            code_change;
        false ->
            undefined
    end.
