-module(rebar3_appup_generate).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, generate).
-define(DEPS, []).

-define(APPUPFILEFORMAT, "%% appup generated for ~p by rebar3_appup_plugin (~p)~n"
        "{~p,\n\t[{~p, \n\t\t~p}], \n\t[{~p, \n\t\t~p}\n]}.~n").
-define(DEFAULT_RELEASE_DIR, "rel").

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
                {target_dir, $t, "target_dir", string, "target dir in which to generate the .appups to"}
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

    ModDeps = [],

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

    %% Generate appup files for apps
    lists:foreach(fun(App) ->
                    generate_appup_files(TargetDir,
                                         CurrentRelPath, PreviousRelPath,
                                         ModDeps, App,
                                         State)
                  end, UpgradeApps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
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

generate_appup_files(_, _, _, _, {upgrade, _App, {undefined, _}}, _) -> ok;
generate_appup_files(TargetDir,
                     NewVerPath, OldVerPath,
                     ModDeps, {upgrade, App, {OldVer, NewVer}},
                     State) ->
    OldRelEbinDir = filename:join([OldVerPath, "lib",
                                atom_to_list(App) ++ "-" ++ OldVer, "ebin"]),
    NewRelEbinDir = filename:join([NewVerPath, "lib",
                                atom_to_list(App) ++ "-" ++ NewVer, "ebin"]),

    {AddedFiles, DeletedFiles, ChangedFiles} = beam_lib:cmp_dirs(NewRelEbinDir,
                                                                 OldRelEbinDir),

    ChangedNames = [list_to_atom(file_to_name(F)) || {F, _} <- ChangedFiles],
    ModDeps1 = [{N, [M1 || M1 <- M, lists:member(M1, ChangedNames)]}
                || {N, M} <- ModDeps],

    Added = [generate_instruction(add_module, File) || File <- AddedFiles],
    Deleted = [generate_instruction(delete_module, File) || File <- DeletedFiles],
    Changed = [generate_instruction(upgrade, ModDeps1, File) || File <- ChangedFiles],

    UpgradeInstructions = lists:append([Added, Deleted, Changed]),
    DowngradeInstructions = lists:reverse(lists:map(fun invert_instruction/1,
                                                    UpgradeInstructions)),

    ok = write_appup(App, OldVer, NewVer, TargetDir,
                     UpgradeInstructions, DowngradeInstructions,
                     State),
    ok.

write_appup(App, OldVer, NewVer, TargetDir,
            UpgradeInstructions, DowngradeInstructions, State) ->
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

    %% write each of the .appup files
    lists:foreach(fun(AppUpFile) ->
                    ok = file:write_file(AppUpFile,
                                         io_lib:fwrite(?APPUPFILEFORMAT,
                                                       [App, rebar3_appup_utils:now_str(),
                                                        NewVer,
                                                        OldVer, UpgradeInstructions,
                                                        OldVer, DowngradeInstructions])),
                    rebar_api:info("Generated appup (~p <-> ~p) for ~p in ~p",
                        [OldVer, NewVer, App, AppUpFile])
                  end, AppUpFiles),
    ok.

generate_instruction(add_module, File) ->
    Name = list_to_atom(file_to_name(File)),
    {add_module, Name};
generate_instruction(delete_module, File) ->
    Name = list_to_atom(file_to_name(File)),
    {delete_module, Name};
generate_instruction(added_application, Application) ->
    {add_application, Application, permanent};
generate_instruction(removed_application, Application) ->
    {remove_application, Application};
generate_instruction(restarted_application, Application) ->
    {restart_application, Application}.

generate_instruction(upgrade, ModDeps, {File, _}) ->
    {ok, {Name, List}} = beam_lib:chunks(File, [attributes, exports]),
    Behavior = get_behavior(List),
    CodeChange = is_code_change(List),
    Deps = proplists:get_value(Name, ModDeps, []),
    generate_instruction_advanced(Name, Behavior, CodeChange, Deps).

generate_instruction_advanced(Name, undefined, undefined, Deps) ->
    %% Not a behavior or code change, assume purely functional
    {load_module, Name, Deps};
generate_instruction_advanced(Name, [supervisor], _, _) ->
    %% Supervisor
    {update, Name, supervisor};
generate_instruction_advanced(Name, _, code_change, Deps) ->
    %% Includes code_change export
    {update, Name, {advanced, []}, Deps};
generate_instruction_advanced(Name, _, _, Deps) ->
    %% Anything else
    {load_module, Name, Deps}.

invert_instruction({load_module, Name, Deps}) -> {load_module, Name, Deps};
invert_instruction({add_module, Name}) -> {delete_module, Name};
invert_instruction({delete_module, Name}) -> {add_module, Name};
invert_instruction({add_application, Application, permanent}) -> {remove_application, Application};
invert_instruction({remove_application, Application}) -> {add_application, Application, permanent};
invert_instruction({update, Name, supervisor}) -> {update, Name, supervisor};
invert_instruction({update, Name, {advanced, []}, Deps}) -> {update, Name, {advanced, []}, Deps}.

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
