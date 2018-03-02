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
-module(rebar3_appup_rel_utils).

-export([get_permanent_version/1,
         get_rel_file_path/2, get_rel_file_path/3,
         get_rel_release_info/1, get_rel_release_info/2,
         get_rel_apps/1, get_rel_apps/2, get_rel_apps/3,
         get_rel_releases_path/2,
         get_release_versions/2,
         exclude_otp_apps/2]).

%% get permanent version from start_erl.data
%% @spec get_permanent_version(atom() | binary() | [atom() | [any()] | char()]) -> any().
get_permanent_version(Path) ->
    DataFile = filename:join([Path, "releases", "start_erl.data"]),
    case file:read_file(DataFile) of
        {ok, DataBin} ->
            [_, Version] = string_compat:tokens(
                             string_compat:strip(binary_to_list(DataBin), right, $\n),
                             " "),
            Version;
        {error, enoent} ->
            rebar_api:abort("~s is missing~n", [DataFile])
    end.

%% Get rel file path from name and path
%% @spec get_rel_file_path([atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()]) -> any().
get_rel_file_path(Name, Path) ->
    PVer = get_permanent_version(Path),
    get_rel_file_path(Name, Path, PVer).

%% @spec get_rel_file_path([atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()],[atom() | [any()] | char()]) -> any().
get_rel_file_path(Name, Path, Version) ->
    Dir = filename:join([get_rel_releases_path(Name, Path), Version]),
    Path1 = filename:join([Dir, Name ++ "_" ++ Version ++".rel"]),
    Path2 = filename:join([Dir, Name ++ ".rel"]),
    case {filelib:is_file(Path1), filelib:is_file(Path2)} of
        {true, _} -> Path1;
        {_, true} -> Path2;
        _ -> rebar_api:abort("can not find .rel file for version ~p~n", [Version])
    end.

%% @spec get_rel_releases_path(_,atom() | binary() | [atom() | [any()] | char()]) -> binary() | string().
get_rel_releases_path(_Name, Path) ->
    filename:join([Path, "releases"]).

%% Get release name and version from a name and a path
%% @spec get_rel_release_info([atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()]) -> any().
get_rel_release_info(Name, Path) ->
    RelPath = get_rel_file_path(Name, Path),
    get_rel_release_info(RelPath).

%% Get release name and version from a rel file
%% @spec get_rel_release_info(atom() | binary() | [atom() | [any()] | char()]) -> any().
get_rel_release_info(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, {Name, Ver}, _, _}]} ->
            {Name, Ver};
        _ ->
            rebar_api:abort("Failed to parse ~s~n", [RelFile])
    end.

%% @spec get_release_versions(_,atom() | binary() | [atom() | [any()] | char()]) -> [binary() | string()].
get_release_versions(Name, Path) ->
    RelPath = get_rel_releases_path(Name, Path),
    lists:map(fun(Dir) ->
                filename:basename(Dir)
              end, rebar3_appup_utils:get_sub_dirs(RelPath)).

%% Get list of apps included in a release from a name, a path and a version
%% @spec get_rel_apps([atom() | [any()] | char()],[atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()]) -> any().
get_rel_apps(Name, Version, Path) ->
    RelPath = get_rel_file_path(Name, Path, Version),
    get_rel_apps(RelPath).

%% Get list of apps included in a release from a name and a path
%% @spec get_rel_apps([atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()]) -> any().
get_rel_apps(Name, Path) ->
    RelPath = get_rel_file_path(Name, Path),
    get_rel_apps(RelPath).

%% Get list of apps included in a release from a rel file
%% @spec get_rel_apps(atom() | binary() | [atom() | [any()] | char()]) -> any().
get_rel_apps(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, _, _, Apps}]} ->
            rebar3_appup_utils:make_proplist(Apps, []);
        _ ->
            rebar_api:abort("Failed to parse ~s~n", [RelFile])
    end.

%% TODO: FIX THIS
exclude_otp_apps(Apps, State) ->
    lists:filter(fun({Name, _Version}) ->
                    NameBin = list_to_binary(atom_to_list(Name)),
                    _AppInfo = rebar3_appup_utils:find_app_info(NameBin, State),
                    true 
                 end, Apps).
