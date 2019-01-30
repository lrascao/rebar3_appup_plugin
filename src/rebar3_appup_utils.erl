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

-export([get_release_name/1,
         prop_check/3,
         make_proplist/2,
         find_files/3,
         find_files_by_ext/2, find_files_by_ext/3,
         now_str/0,
         get_sub_dirs/1,
         appup_plugin_appinfo/1,
         find_app_info/2,
         load_module_from_beam/2,
         unload_module_from_beam/2,
         beam_rel_path/2, beam_rel_path/4,
         get_abstract_code/2,
         tmp_filename/0,
         find_app_by_name/2,
         vsn/1]).

-spec get_release_name(State) -> Res when
      State :: rebar_state:t(),
      Res :: string().
get_release_name(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(relname, Opts, undefined) of
        undefined ->
            RelxConfig = rebar_state:get(State, relx, []),
            case lists:keyfind(release, 1, RelxConfig) of
                {release, {Name0, _Ver}, _} ->
                    atom_to_list(Name0);
                {release, {Name0, _Ver}, _, _} ->
                    atom_to_list(Name0)
            end;
        Name ->
            Name
    end.

%% Helper function for checking values and aborting when needed
%% @spec prop_check(boolean(),_,_) -> any().
prop_check(true, _, _) -> true;
prop_check(false, Msg, Args) -> rebar_api:abort(Msg, Args).

%% @spec make_proplist([tuple()],_) -> any().
make_proplist([{_,_}=H|T], Acc) ->
    make_proplist(T, [H|Acc]);
make_proplist([H|T], Acc) ->
    App = element(1, H),
    Ver = element(2, H),
    make_proplist(T, [{App,Ver}|Acc]);
make_proplist([], Acc) ->
    Acc.

%% @spec find_files(atom() | [atom() | [any()] | char()],string(),boolean()) -> any().
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
%% @spec find_files_by_ext(atom() | [atom() | [any()] | char()],string()) -> any().
find_files_by_ext(Dir, Ext) ->
    find_files_by_ext(Dir, Ext, true).

%% @spec find_files_by_ext(atom() | [atom() | [any()] | char()],string(),boolean()) -> any().
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

%% @spec now_str() -> [any()].
now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b",
                                [Year, Month, Day, Hour, Minute, Second])).

%% @spec get_sub_dirs(atom() | binary() | [atom() | [any()] | char()]) -> [any()].
get_sub_dirs(Dir) ->
    lists:filtermap(fun(SubDir) ->
                        case filelib:is_dir(SubDir) of
                            true -> {true, SubDir};
                            false -> false
                        end
                    end, filelib:wildcard(filename:join(Dir, "*"))).

%% @spec appup_plugin_appinfo(maybe_improper_list()) -> any().
appup_plugin_appinfo(Apps) ->
    appup_plugin_appinfo(Apps, undefined).

%% @spec appup_plugin_appinfo(maybe_improper_list(),_) -> any().
appup_plugin_appinfo([], AppInfo) -> AppInfo;
appup_plugin_appinfo([AppInfo | Rest], _) ->
    case rebar_app_info:name(AppInfo) of
        <<"rebar3_appup_plugin">> ->
            appup_plugin_appinfo([], AppInfo);
        _ ->
            appup_plugin_appinfo(Rest, undefined)
    end.

%% @spec get_abstract_code(atom(),binary() | string()) -> 'encrypted_abstract_code' | 'no_abstract_code' | binary() | [{atom() | integer(),_} | {atom(),atom() | byte(),integer()} | {non_neg_integer(),atom() | tuple(),atom(),byte()}] | {atom(),[any()] | {atom() | tuple(),[any()]}} | {'error','beam_lib',{'not_a_beam_file',string()} | {'file_error',string(),atom() | nonempty_string() | non_neg_integer()} | {'invalid_beam_file',string(),atom() | nonempty_string() | non_neg_integer()} |
%% {'invalid_chunk',string(),atom() | nonempty_string() | non_neg_integer()} | {'missing_chunk',string(),atom() | nonempty_string() | non_neg_integer()} | {'unknown_chunk',string(),atom() | nonempty_string() | non_neg_integer()} | {'chunk_too_big',string(),nonempty_string(),non_neg_integer(),non_neg_integer()}}.
get_abstract_code(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Module, [{abstract_code, AbstractCode}]}} ->
            AbstractCode;
        {error, beam_lib, {key_missing_or_invalid, _, _}} ->
            encrypted_abstract_code;
        Error -> Error
    end.

tmp_filename() ->
     lists:flatten(io_lib:format("tmp.appup.~p", [erlang:phash2(make_ref())])).

%% @spec find_app_info(_,_) -> any().
find_app_info(Name, State) ->
    Apps = rebar_state:project_apps(State),
    Deps = rebar_state:all_deps(State),
    find_app_info1(Name, Apps ++ Deps).

%% @spec find_app_info1(_,maybe_improper_list()) -> any().
find_app_info1(_Name, []) -> undefined;
find_app_info1(Name, [App | Apps]) ->
    case rebar_app_info:name(App) =:= Name of
        true -> App;
        false -> find_app_info1(Name, Apps)
    end.

%% @spec load_module_from_beam(atom() | binary() | [atom() | [any()] | char()],atom()) -> {'module',atom()}.
load_module_from_beam(Beam, Module) ->
    true = code:add_path(filename:dirname(Beam)),
    {module, Module} = code:load_file(Module).

%% @spec unload_module_from_beam(atom() | binary() | [atom() | [any()] | char()],atom()) -> 'ok'.
unload_module_from_beam(Beam, Module) ->
    code:del_path(filename:dirname(Beam)),
    code:delete(Module),
    code:purge(Module),
    ok.

%% @spec beam_rel_path([atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()],[atom() | [any()] | char()],[atom() | [any()] | char()]) -> binary() | string().
beam_rel_path(App, RelDir, Version, Module) ->
    filename:join([RelDir, App, "lib",
                   App ++ "-" ++ Version,
                   "ebin",
                   Module ++ ".beam"]).

%% @spec beam_rel_path(atom() | binary() | [atom() | [any()] | char()],[atom() | [any()] | char()]) -> binary() | string().
beam_rel_path(EbinDir, Module) ->
    filename:join([EbinDir,
                   Module ++ ".beam"]).

find_app_by_name(Name, Apps) ->
    ec_lists:find(fun(App) ->
        rebar_app_info:name(App) =:= Name
     end, Apps).

-spec vsn(AppInfo :: term()) -> string().
vsn(AppInfo) ->
    case rebar_app_info:original_vsn(AppInfo) of
        Vsn when is_binary(Vsn) -> binary_to_list(Vsn);
        Vsn when is_list(Vsn) -> Vsn
    end.

