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
-module(rebar3_appup_tar).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, tar).
-define(DEPS, [{default, tar}]).

-define(DEFAULT_RELEASE_DIR, "rel").
-define(PRIV_DIR, "priv").
-define(CONVERT_TEMPLATE, "templates/convert.tpl").
-define(CONVERT_CALL_TEMPLATE, "templates/convert_call.tpl").

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
            {example, "rebar3 appup tar"},
            {short_desc, "Instruments necessary to provide automatic state record migration "
                         "for gen_servers"},
            {desc, "Appup tar"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    {release, {Name0, _Ver}, _} = lists:keyfind(release, 1, RelxConfig),
    Name = atom_to_list(Name0),
    rebar_api:debug("release name: ~p", [Name]),

    RelDir = filename:join([rebar_dir:base_dir(State),
                            ?DEFAULT_RELEASE_DIR]),
    CurrentRelPath = filename:join([RelDir, Name]),

    CurrentBaseDir = rebar_dir:base_dir(State),
    LibDir = filename:join([CurrentBaseDir, "lib"]),

    {CurrentName, Version} = rebar3_appup_rel_utils:get_rel_release_info(
                                            Name, CurrentRelPath),
    rebar_api:debug("current release, path: ~p, name: ~p, version: ~p",
        [CurrentRelPath, CurrentName, Version]),

    %% search for this plugin's appinfo in order to know
    %% where to look for the mustache templates
    Apps = rebar_state:all_plugin_deps(State),
    PluginInfo = rebar3_appup_utils:appup_plugin_appinfo(Apps),
    PluginDir = rebar_app_info:dir(PluginInfo),

    %% first consult the appup to look for gen_servers that need to be upgraded
    AppupFile = filename:join([LibDir, Name, "ebin", Name ++ ".appup"]),
    case file:consult(AppupFile) of
        {ok, Appup} ->
            case lists:keysearch(Version, 1, Appup) of
              {value, {Version, Upgrade, _Downgrade}} ->
                Opts = [{app, Name},
                        {version, Version},
                        {rel_dir, RelDir},
                        {lib_dir, LibDir},
                        {plugin_dir, PluginDir}],
                handle_upgrade(Upgrade, Opts);
              false ->
                rebar_api:abort("unable to find version ~p in appup ~p",
                  [Version, Appup])
            end;
        _ -> ok
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

handle_upgrade([], _Opts) -> ok;
handle_upgrade([{UpFromVersion, Instructions} | Rest], Opts) ->
    handle_upgrade_instruction(UpFromVersion, Instructions, Opts),
    handle_upgrade(Rest, Opts).

handle_upgrade_instruction(_, [], _) -> ok;
handle_upgrade_instruction(UpFromVersion,
                           [{update, Module, _Change, _PrePurge, _PostPurge, _DepMods} | Rest],
                           Opts) ->
    Version = proplists:get_value(version, Opts),
    App = proplists:get_value(app, Opts),
    RelDir = proplists:get_value(rel_dir, Opts),

    FromData = get_gen_server_data(App, RelDir,
                                   UpFromVersion,
                                   atom_to_list(Module)),
    ToData = get_gen_server_data(App, RelDir,
                                 Version,
                                 atom_to_list(Module)),

    case proplists:get_value(state_record_name, ToData, undefined) of
        undefined -> ok;
        StateRecordName ->
            %% now check if there are any change in the state record structure
            %% from the previous to this version
            OldRecordFields = proplists:get_value(state_record_fields, FromData),
            NewRecordFields = proplists:get_value(state_record_fields, ToData),
            case NewRecordFields =/= OldRecordFields of
              true ->
                rebar_api:debug("gen_server ~p state record ~p upgrade from ~p to ~p",
                    [Module, StateRecordName, UpFromVersion, Version]),
                rebar_api:debug("\told record fields: ~p",
                    [OldRecordFields]),
                rebar_api:debug("\tnew record fields: ~p",
                    [NewRecordFields]),
                do_state_record_migration(Module,
                                          UpFromVersion, FromData,
                                          Version, ToData, Opts);
              false -> ok
            end
    end,
    handle_upgrade_instruction(UpFromVersion, Rest, Opts);
handle_upgrade_instruction(UpFromVersion, [_ | Rest], Opts) ->
    handle_upgrade_instruction(UpFromVersion, Rest, Opts).

get_gen_server_data(App, RelDir, Version, ModuleStr) ->
    Beam = beam_rel_path(App, RelDir, Version, ModuleStr),
    {module, Module} = load_module_from_beam(Beam, list_to_atom(ModuleStr)),
    Attributes = Module:module_info(attributes),
    Exports = Module:module_info(exports),
    Forms =  case get_abstract_code(Module, Beam) of
                no_abstract_code=E ->
                    {error, E};
                encrypted_abstract_code=E ->
                    {error, E};
                {raw_abstract_v1, Code} ->
                    epp:interpret_file_attribute(Code)
              end,
    StateData = case proplists:get_value(state_record, Attributes, undefined) of
                    undefined -> [{state_record_name, undefined}];
                    [StateRecordName] ->
                        get_state_data(Module, StateRecordName, Forms)
                end,
    CompileInfo = get_compile_info(Module, Beam),
    ok = unload_module_from_beam(Beam, Module),
    [{version, Version},
     {beam, Beam},
     {module, Module},
     {attributes, Attributes},
     {exports, Exports},
     {abstract_code, Forms},
     {compile_info, CompileInfo}] ++ StateData.

get_state_data(Module, StateRecordName, Forms) ->
    %% extract state record info, a proplists of key position pairs
    Info = state_record_info(Module, StateRecordName, Forms),
    %% also get the abstract code of the record
    StateRecordAbst = extract_record(StateRecordName, Forms),
    [{state_record_name, StateRecordName},
     {state_record_fields, Info},
     {state_record_abstract_code, StateRecordAbst}].

abst_atom_name({atom, _L, Name}) -> Name.

state_record_info(_Module, RecordName, Forms) ->
    {attribute, _L0, record, {RecordName, Fields}} = extract_record(RecordName, Forms),
    {L, _} =
        lists:mapfoldl(fun({record_field, _L1, RecordFieldName}, Acc) ->
                            {{abst_atom_name(RecordFieldName), Acc}, Acc + 1};
                          ({record_field, _L1, RecordFieldName, _}, Acc) ->
                            {{abst_atom_name(RecordFieldName), Acc}, Acc + 1};
                          ({typed_record_field, {record_field, _L1, RecordFieldName}, _}, Acc) ->
                            {{abst_atom_name(RecordFieldName), Acc}, Acc + 1};
                          ({typed_record_field, {record_field, _L1, RecordFieldName, _}, _}, Acc) ->
                            {{abst_atom_name(RecordFieldName), Acc}, Acc + 1}
                       end, 2, Fields),
    L.

get_abstract_code(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Module, [{abstract_code, AbstractCode}]}} ->
            AbstractCode;
        {error, beam_lib, {key_missing_or_invalid, _, _}} ->
            encrypted_abstract_code;
        Error -> Error
    end.

get_compile_info(Module, Beam) ->
    case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {Module, [{compile_info, Compile}]}} ->
            Compile;
        _ -> []
    end.

load_module_from_beam(Beam, Module) ->
    true = code:add_path(filename:dirname(Beam)),
    {module, Module} = code:load_file(Module).

unload_module_from_beam(Beam, Module) ->
    code:del_path(filename:dirname(Beam)),
    code:delete(Module),
    code:purge(Module),
    ok.

beam_rel_path(App, RelDir, Version, Module) ->
    filename:join([RelDir, App, "lib",
                   App ++ "-" ++ Version,
                   "ebin",
                   Module ++ ".beam"]).

do_state_record_migration(Module,
                          FromVersion, FromData,
                          _ToVersion, ToData, Opts) ->
    %% get the necessary data for the from version
    FromVersion = proplists:get_value(version, FromData),
    %% get the state record name, assume that it hasn't changed from one version
    %% to the other
    StateRecordName = proplists:get_value(state_record_name, ToData),
    %% get the abstract to inject code into
    Forms0 = proplists:get_value(abstract_code, ToData),
    % inject the method that converts from one record to the other
    %% build the required arguments for the mustache template
    {ok, ConvertTemplate} = file:read_file(filename:join([proplists:get_value(plugin_dir, Opts),
                                                          ?PRIV_DIR, ?CONVERT_TEMPLATE])),
    FromRecordFields = proplists:get_value(state_record_fields, FromData),
    ToRecordFields = proplists:get_value(state_record_fields, ToData),
    Module = proplists:get_value(module, ToData),
    ConvertCtx = [{"module", atom_to_list(Module)},
                  {"old_state_record_name",
                      atom_to_list(apply_version_record_name(StateRecordName, FromVersion))},
                  {"state_record_name", atom_to_list(StateRecordName)},
                  {"vsn_data", io_lib:format("~p",
                      [[{old, [{state_record_fields, FromRecordFields}]},
                               {new, [{state_record_fields, ToRecordFields}]}]])}
                 ],
    Convert = bbmustache:render(ConvertTemplate, ConvertCtx),
    ConvertForm = to_abstract(binary_to_list(Convert)),

    %% inject the abstract code for the old record
    OldStateRecordAbst = proplists:get_value(state_record_abstract_code, FromData),
    OldStateRecordName = apply_version_record_name(StateRecordName,
                                                   FromVersion),
    %% inject the old record version in the new beam code
    Forms1 = inject_record(OldStateRecordName, OldStateRecordAbst, Forms0),
    %% inject the method that performs the record conversion
    Forms2 = inject_method(ConvertForm, Forms1),
    %% inject an invocation at the top of the code_change method
    %% so that the record is converted
    Forms = inject_code_change_convert_call(Forms2, Opts),

    %% now recompile the injected abstract code and rewrite the
    %% current version .beam file
    CompileInfo = proplists:get_value(compile_info, ToData),
    {ok, Module, Binary, _Warnings} = compile:forms(Forms,
                                                    CompileInfo ++
                                                    [binary, debug_info, return]),
    ToBeam = proplists:get_value(beam, ToData),
    ok = file:write_file(ToBeam, Binary),
    ok.

extract_record(RecordName, Forms) ->
    [RecordAbst] =
      lists:filter(fun({attribute, _L, record, {R, _}})
                          when R =:= RecordName -> true;
                      (_) -> false
                end, Forms),
    RecordAbst.

apply_version_record_name(Name, Version) ->
    %% follow the exprecs format (eg. <record>__<version>)
    list_to_atom(atom_to_list(Name) ++ "__" ++ Version).

apply_record_line(L, {attribute, _, record, RecordDef}) ->
    {attribute, L, record, RecordDef}.

apply_record_name(Name, {attribute, L, record, {_, Fields}}) ->
    {attribute, L, record, {Name, Fields}}.

inject_record(RecordName, RecordAbst, Forms0) ->
    {value, {eof, L0}} = lists:keysearch(eof, 1, Forms0),
    Forms =
      lists:map(fun({eof, L}) ->
                      apply_record_line(L,
                          apply_record_name(RecordName, RecordAbst));
                   (Form) -> Form
                end, Forms0),
    Forms ++ [{eof, L0 + 1}].

apply_method_line(L, {function, _, Method, Arity, Clauses}) ->
    {function, L, Method, Arity, Clauses}.

inject_method(Form, Forms0) ->
    {value, {eof, L}} = lists:keysearch(eof, 1, Forms0),
    Forms1 =
      lists:map(fun({eof, _L}) ->
                      apply_method_line(L, Form);
                    (F) -> F
                end, Forms0),
    Forms1 ++ [{eof, L + 1}].

inject_code_change_convert_call(Forms, Opts) ->
    %% search for the code_change function
    lists:map(fun({function, L, code_change, 3, FunctionClauses0}) ->
                    FunctionClauses = lists:map(fun(FunctionClause) ->
                                                  inject_code_change_clause(FunctionClause, Opts)
                                                end, FunctionClauses0),
                    {function, L, code_change, 3, FunctionClauses};
                 (Form) -> Form
              end, Forms).

inject_code_change_clause({clause, L, Args0, [], Body0}, Opts) ->
    %% modify the args, rename the second argument which is the state to be migrated
    %% and the third one which is the extra options
    [{var, L0, OldVsnVar}, {var, L1, StateVar}, {var, L2, ExtraVar}] = Args0,
    Args = [{var, L0, '__OldVsn__'},
            {var, L1, '__State0__'},
            {var, L2, '__Extra0__'}],
    %% now inject the beginning of the body with a call to our convert method
    {ok, ConvertCallTemplate} = file:read_file(filename:join([proplists:get_value(plugin_dir, Opts),
                                                              ?PRIV_DIR,
                                                              ?CONVERT_CALL_TEMPLATE])),
    ConvertCallCtx = [{"old_vsn_var", atom_to_list(OldVsnVar)},
                      {"state_var", atom_to_list(StateVar)},
                      {"extra_var", atom_to_list(ExtraVar)}],
    ConvertCall = bbmustache:render(ConvertCallTemplate, ConvertCallCtx),
    AbstConvertCall = to_abstract(binary_to_list(ConvertCall)),
    {clause, L, Args, [], AbstConvertCall ++ Body0}.

-spec to_abstract(string()) -> erl_parse:abstract_form().
to_abstract(String) ->
    {ok, Tokens, _EndLocation} =
        erl_scan:string(String),
    {ok, AbsForm} =
        try
            {ok, _} = erl_parse:parse_form(Tokens)
        catch
            _:_ ->
                {ok, _} = erl_parse:parse_exprs(Tokens)
        end,
    AbsForm.
