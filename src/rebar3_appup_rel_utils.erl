-module(rebar3_appup_rel_utils).

-export([get_permanent_version/1,
         get_rel_file_path/2, get_rel_file_path/3,
         get_rel_release_info/1, get_rel_release_info/2,
         get_rel_apps/1, get_rel_apps/2, get_rel_apps/3]).

%% get permanent version from start_erl.data
get_permanent_version(Path) ->
    DataFile = filename:join([Path, "releases", "start_erl.data"]),
    case file:read_file(DataFile) of
        {ok, DataBin} ->
            [_, Version] = string:tokens(
                             string:strip(binary_to_list(DataBin), right, $\n),
                             " "),
            Version;
        {error, enoent} ->
            rebar_api:abort("~s is missing~n", [DataFile])
    end.

%% Get rel file path from name and path
get_rel_file_path(Name, Path) ->
    PVer = get_permanent_version(Path),
    get_rel_file_path(Name, Path, PVer).

get_rel_file_path(Name, Path, Version) ->
    Dir = filename:join([Path, "releases", Version]),
    Path1 = filename:join([Dir, Name ++ "_" ++ Version ++".rel"]),
    Path2 = filename:join([Dir, Name ++ ".rel"]),
    case {filelib:is_file(Path1), filelib:is_file(Path2)} of
        {true, _} -> Path1;
        {_, true} -> Path2;
        _ -> rebar_api:abort("can not find .rel file for version ~p~n", [Version])
    end.

%% Get release name and version from a name and a path
get_rel_release_info(Name, Path) ->
    RelPath = get_rel_file_path(Name, Path),
    get_rel_release_info(RelPath).

%% Get release name and version from a rel file
get_rel_release_info(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, {Name, Ver}, _, _}]} ->
            {Name, Ver};
        _ ->
            rebar_api:abort("Failed to parse ~s~n", [RelFile])
    end.

%% Get list of apps included in a release from a name, a path and a version
get_rel_apps(Name, Version, Path) ->
    RelPath = get_rel_file_path(Name, Path, Version),
    get_rel_apps(RelPath).

%% Get list of apps included in a release from a name and a path
get_rel_apps(Name, Path) ->
    RelPath = get_rel_file_path(Name, Path),
    get_rel_apps(RelPath).

%% Get list of apps included in a release from a rel file
get_rel_apps(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, _, _, Apps}]} ->
            rebar3_appup_utils:make_proplist(Apps, []);
        _ ->
            rebar_api:abort("Failed to parse ~s~n", [RelFile])
    end.
