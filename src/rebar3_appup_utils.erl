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
-module(rebar3_appup_utils).

-export([prop_check/3,
         make_proplist/2,
         find_files/3,
         find_files_by_ext/2, find_files_by_ext/3,
         now_str/0,
         get_sub_dirs/1,
         appup_plugin_appinfo/1,
         tmp_filename/0,
         find_app_info/2]).

%% Helper function for checking values and aborting when needed
prop_check(true, _, _) -> true;
prop_check(false, Msg, Args) -> rebar_api:abort(Msg, Args).

make_proplist([{_,_}=H|T], Acc) ->
    make_proplist(T, [H|Acc]);
make_proplist([H|T], Acc) ->
    App = element(1, H),
    Ver = element(2, H),
    make_proplist(T, [{App,Ver}|Acc]);
make_proplist([], Acc) ->
    Acc.

find_files(Dir, Regex, Recursive) ->
    filelib:fold_files(Dir, Regex, Recursive,
                       fun(F, Acc) -> [F | Acc] end, []).

%% Find files by extension, for example ".erl", avoiding resource fork
%% files in OS X.  Such files are named for example src/._xyz.erl
%% Such files may also appear with network filesystems on OS X.
%%
%% The Ext is really a regexp, with any leading dot implicitly
%% escaped, and anchored at the end of the string.
%%
find_files_by_ext(Dir, Ext) ->
    find_files_by_ext(Dir, Ext, true).

find_files_by_ext(Dir, Ext, Recursive) ->
    %% Convert simple extension to proper regex
    EscapeDot = case Ext of
                    "." ++ _ ->
                        "\\";
                    _ ->
                        %% allow for other suffixes, such as _pb.erl
                        ""
                end,
    ExtRe = "^[^._].*" ++ EscapeDot ++ Ext ++ [$$],
    find_files(Dir, ExtRe, Recursive).

now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b",
                                [Year, Month, Day, Hour, Minute, Second])).

get_sub_dirs(Dir) ->
    lists:filtermap(fun(SubDir) ->
                        case filelib:is_dir(SubDir) of
                            true -> {true, SubDir};
                            false -> false
                        end
                    end, filelib:wildcard(filename:join(Dir, "*"))).

appup_plugin_appinfo(Apps) ->
    appup_plugin_appinfo(Apps, undefined).

appup_plugin_appinfo([], AppInfo) -> AppInfo;
appup_plugin_appinfo([AppInfo | Rest], _) ->
    case rebar_app_info:name(AppInfo) of
        <<"rebar3_appup_plugin">> ->
            appup_plugin_appinfo([], AppInfo);
        _ ->
            appup_plugin_appinfo(Rest, undefined)
    end.

tmp_filename() ->
     lists:flatten(io_lib:format("tmp.appup.~p", [erlang:phash2(make_ref())])).

find_app_info(Name, State) ->
    Apps = rebar_state:project_apps(State),
    find_app_info1(Name, Apps).

find_app_info1(_Name, []) -> undefined;
find_app_info1(Name, [App | Apps]) ->
    case rebar_app_info:name(App) =:= Name of
        true -> App;
        false -> find_app_info1(Name, Apps)
    end.
