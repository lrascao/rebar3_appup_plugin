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
-define(CONVERT_AUX_TEMPLATE, "templates/convert_aux.tpl").
-define(CONVERT_CALL_TEMPLATE, "templates/convert_call.tpl").

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
            {example, "rebar3 appup tar"},
            {short_desc, "Instruments necessary to provide automatic state record migration "
                         "for gen_servers"},
            {desc, "Appup tar"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
%% @spec do(rebar_state:t()) -> {'ok',rebar_state:t()} | {'error',string()}.
do(State) ->
    Name = rebar3_appup_utils:get_release_name(State),
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
                rebar_api:debug("unable to find version ~p in appup ~p",
                  [Version, Appup])
            end;
        _ -> ok
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
%% @spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

%% @spec handle_upgrade([{_,[any()]}],[{'app',[any()]} | {'lib_dir',binary() | [any()]} | {'plugin_dir',_} | {'rel_dir',binary() | [any()]} | {'version',_},...]) -> 'ok'.
handle_upgrade([], _Opts) -> ok;
handle_upgrade([{UpFromVersion, Instructions} | Rest], Opts) ->
    handle_upgrade_instruction(UpFromVersion, Instructions, Opts),
    handle_upgrade(Rest, Opts).

%% @spec handle_upgrade_instruction(_,[any()],[{'app',[any()]} | {'lib_dir',binary() | [any()]} | {'plugin_dir',_} | {'rel_dir',binary() | [any()]} | {'version',_},...]) -> 'ok'.
handle_upgrade_instruction(_, [], _) -> ok;
handle_upgrade_instruction(UpFromVersion,
                           [{update, Module, _Change, _PrePurge, _PostPurge, _DepMods} | Rest],
                           Opts) ->
    Version = proplists:get_value(version, Opts),
    App = proplists:get_value(app, Opts),
    RelDir = proplists:get_value(rel_dir, Opts),

    ToData = get_gen_server_data(App, RelDir,
                                 Version,
                                 atom_to_list(Module),
                                 undefined),
    CurrentStateRecordName = proplists:get_value(state_record_name, ToData,
                                                 undefined),
    %% the previous might not have had the omniescience
    %% of declaring the state record name, for that case
    %% we pass the state record name of the current version
    %% for it to be used instead
    FromData = get_gen_server_data(App, RelDir,
                                   UpFromVersion,
                                   atom_to_list(Module),
                                   CurrentStateRecordName),

    case proplists:get_value(state_record_name, ToData, undefined) of
        undefined -> ok;
        _ ->
            %% now check if there are any change in the state record structure
            %% from the previous to this version
            OldRecordFields = proplists:get_value(state_record_fields, FromData),
            NewRecordFields = proplists:get_value(state_record_fields, ToData),
            case OldRecordFields =/= undefined andalso
                 NewRecordFields =/= OldRecordFields of
              true ->
                do_state_record_migration(Module,
                                          UpFromVersion, FromData,
                                          Version, ToData, Opts),
                rebar_api:info("Injected code for state record migration of process ~p from ~p to ~p",
                  [Module, UpFromVersion, Version]);
              false -> ok
            end
    end,
    handle_upgrade_instruction(UpFromVersion, Rest, Opts);
handle_upgrade_instruction(UpFromVersion, [_ | Rest], Opts) ->
    handle_upgrade_instruction(UpFromVersion, Rest, Opts).

%% @spec get_gen_server_data([atom() | [any()] | char()],atom() | binary() | [atom() | [any()] | char()],[atom() | [any()] | char()],string(),_) -> [{'abstract_code' | 'attributes' | 'beam' | 'compile_info' | 'exports' | 'module' | 'state_record_abstract_code' | 'state_record_fields' | 'state_record_name' | 'version',_}].
get_gen_server_data(App, RelDir, Version, ModuleStr, StateRecordName) ->
    Beam = rebar3_appup_utils:beam_rel_path(App, RelDir, Version, ModuleStr),
    case filelib:is_file(Beam) of
      true ->
        get_gen_server_data(Beam, Version, ModuleStr, StateRecordName);
      false -> []
    end.

%% @spec get_gen_server_data(binary() | string(),[atom() | [any()] | char()],string(),_) -> [{'abstract_code' | 'attributes' | 'beam' | 'compile_info' | 'exports' | 'module' | 'state_record_abstract_code' | 'state_record_fields' | 'state_record_name' | 'version',_},...].
get_gen_server_data(Beam, Version, ModuleStr, StateRecordName0) ->
    {module, Module} = rebar3_appup_utils:load_module_from_beam(Beam, list_to_atom(ModuleStr)),
    Attributes = Module:module_info(attributes),
    Exports = Module:module_info(exports),
    Forms =  case rebar3_appup_utils:get_abstract_code(Module, Beam) of
                no_abstract_code=E ->
                    {error, E};
                encrypted_abstract_code=E ->
                    {error, E};
                {raw_abstract_v1, Code} ->
                    epp:interpret_file_attribute(Code)
              end,
    StateRecordName = proplists:get_value(state_record, Attributes,
                                          StateRecordName0),
    StateData = case StateRecordName of
                    undefined -> [{state_record_name, undefined}];
                    %% we have something in the attributes
                    %% it comes inside a list
                    [S] ->
                        get_state_data(Module, S, Forms);
                    %% we use what we were passed
                    _ ->
                        get_state_data(Module, StateRecordName, Forms)
                end,
    CompileInfo = get_compile_info(Module, Beam),
    ok = rebar3_appup_utils:unload_module_from_beam(Beam, Module),
    [{version, Version},
     {beam, Beam},
     {module, Module},
     {attributes, Attributes},
     {exports, Exports},
     {abstract_code, Forms},
     {compile_info, CompileInfo}] ++ StateData.

%% @spec get_state_data(atom(),_,[any()]) -> [{'state_record_abstract_code',_} | {'state_record_fields',[{_,_}]} | {'state_record_name',_},...].
get_state_data(Module, StateRecordName, Forms) ->
    %% extract state record info, a proplists of key position pairs
    Info = state_record_info(Module, StateRecordName, Forms),
    %% also get the abstract code of the record
    StateRecordAbst = extract_record(StateRecordName, Forms),
    [{state_record_name, StateRecordName},
     {state_record_fields, Info},
     {state_record_abstract_code, StateRecordAbst}].

%% @spec abst_atom_name({'atom',_,_}) -> any().
abst_atom_name({atom, _L, Name}) -> Name.

%% @spec state_record_info(atom(),_,[any()]) -> [{_,number()}].
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

%% @spec get_compile_info(atom(),binary() | string()) -> 'no_abstract_code' | binary() | [{atom() | integer(),_} | {atom(),atom() | byte(),integer()} | {non_neg_integer(),atom() | tuple(),atom(),byte()}] | {atom(),[any()]}.
get_compile_info(Module, Beam) ->
    case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {Module, [{compile_info, Compile}]}} ->
            Compile;
        _ -> []
    end.

%% @spec do_state_record_migration(atom(),string(),[{'abstract_code' | 'attributes' | 'beam' | 'compile_info' | 'exports' | 'module' | 'state_record_abstract_code' | 'state_record_fields' | 'state_record_name' | 'version',_}],[atom() | [any()] | char()],[{'abstract_code' | 'attributes' | 'beam' | 'compile_info' | 'exports' | 'module' | 'state_record_abstract_code' | 'state_record_fields' | 'state_record_name' | 'version',_}],[{'app',[any()]} | {'lib_dir',binary() | [any()]} | {'plugin_dir',_} | {'rel_dir',binary() | [any()]} | {'version',_},...]) -> any().
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
    FromRecordFields = proplists:get_value(state_record_fields, FromData),
    ToRecordFields = proplists:get_value(state_record_fields, ToData),
    Module = proplists:get_value(module, ToData),
    % inject the method that converts from one record to the other
    %% build the required arguments for the mustache template
    {ok, ConvertTemplate} = file:read_file(filename:join([proplists:get_value(plugin_dir, Opts),
                                                          ?PRIV_DIR, ?CONVERT_TEMPLATE])),
    {ok, ConvertAuxTemplate} = file:read_file(filename:join([proplists:get_value(plugin_dir, Opts),
                                                            ?PRIV_DIR, ?CONVERT_AUX_TEMPLATE])),
    ConvertCtx = [{"module", atom_to_list(Module)},
                  {"old_state_record_name",
                      atom_to_list(apply_version_record_name(StateRecordName, FromVersion))},
                  {"state_record_name", atom_to_list(StateRecordName)},
                  {"vsn_data", io_lib:format("~p",
                      [[{old, [{state_record_fields, FromRecordFields}]},
                               {new, [{state_record_fields, ToRecordFields}]}]])}
                 ],
    ConvertAuxCtx = [{"module", atom_to_list(Module)},
                     {"old_state_record_name",
                        atom_to_list(apply_version_record_name(StateRecordName, FromVersion))},
                     {"state_record_name", atom_to_list(StateRecordName)},
                     {"vsn_data", io_lib:format("~p",
                        [[{old, [{state_record_fields, FromRecordFields}]},
                                 {new, [{state_record_fields, ToRecordFields}]}]])}
                    ],
    Convert = bbmustache:render(ConvertTemplate, ConvertCtx),
    ConvertAux = bbmustache:render(ConvertAuxTemplate, ConvertAuxCtx),
    ConvertForm = to_abstract(binary_to_list(Convert)),
    ConvertAuxForm = to_abstract(binary_to_list(ConvertAux)),

    %% inject the abstract code for the old record
    OldStateRecordAbst = proplists:get_value(state_record_abstract_code, FromData),
    OldStateRecordName = apply_version_record_name(StateRecordName,
                                                   FromVersion),
    %% inject the old record version in the new beam code
    Forms1 = inject_record(OldStateRecordName, OldStateRecordAbst, Forms0),
    %% inject the auxilliary method for record conversion
    Forms2 = inject_method(ConvertAuxForm, Forms1),
    %% inject the method that performs the record conversion
    Forms3 = inject_method(ConvertForm, Forms2),
    %% inject an invocation at the top of the code_change method
    %% so that the record is converted
    Forms = inject_code_change_convert_call(Forms3, Opts),

    %% now recompile the injected abstract code and rewrite the
    %% current version .beam file
    CompileInfo = proplists:get_value(compile_info, ToData),
    case compile:forms(Forms, CompileInfo ++ [binary, debug_info, return]) of
      {ok, Module, Binary, _Warnings} ->
        ToBeam = proplists:get_value(beam, ToData),
        ok = file:write_file(ToBeam, Binary);
      {error, Errors, Warnings} ->
        rebar_api:abort("code conversion injection failed due to ~p, warnings: ~p",
          [Errors, Warnings])
    end.

%% @spec extract_record(_,[any()]) -> any().
extract_record(RecordName, Forms) ->
    [RecordAbst] =
      lists:filter(fun({attribute, _L, record, {R, _}})
                          when R =:= RecordName -> true;
                      (_) -> false
                end, Forms),
    RecordAbst.

%% @spec apply_version_record_name(atom(),string()) -> atom().
apply_version_record_name(Name, Version) ->
    %% follow the exprecs format (eg. <record>__<version>)
    list_to_atom(atom_to_list(Name) ++ "__" ++ Version).

%% @spec apply_record_line(_,{'attribute',_,'record',{atom(),_}}) -> {'attribute',_,'record',{atom(),_}}.
apply_record_line(L, {attribute, _, record, RecordDef}) ->
    {attribute, L, record, RecordDef}.

%% @spec apply_record_name(atom(),{'attribute',_,'record',{_,_}}) -> {'attribute',_,'record',{atom(),_}}.
apply_record_name(Name, {attribute, L, record, {_, Fields}}) ->
    {attribute, L, record, {Name, Fields}}.

%% @spec inject_record(atom(),_,[any()]) -> [any(),...].
inject_record(RecordName, RecordAbst, Forms0) ->
    {value, {eof, L0}} = lists:keysearch(eof, 1, Forms0),
    Forms =
      lists:map(fun({eof, L}) ->
                      apply_record_line(L,
                          apply_record_name(RecordName, RecordAbst));
                   (Form) -> Form
                end, Forms0),
    Forms ++ [{eof, L0 + 1}].

%% @spec apply_method_line(_,{'function',_,_,_,_}) -> {'function',_,_,_,_}.
apply_method_line(L, {function, _, Method, Arity, Clauses}) ->
    {function, L, Method, Arity, Clauses}.

%% @spec inject_method(_,[any(),...]) -> [any(),...].
inject_method(Form, Forms0) ->
    {value, {eof, L}} = lists:keysearch(eof, 1, Forms0),
    Forms1 =
      lists:map(fun({eof, _L}) ->
                      apply_method_line(L, Form);
                    (F) -> F
                end, Forms0),
    Forms1 ++ [{eof, L + 1}].

%% @spec inject_code_change_convert_call([any(),...],[{'app',[any()]} | {'lib_dir',binary() | [any()]} | {'plugin_dir',_} | {'rel_dir',binary() | [any()]} | {'version',_},...]) -> [any(),...].
inject_code_change_convert_call(Forms, Opts) ->
    %% search for the code_change function
    lists:map(fun({function, L, code_change, 3, FunctionClauses0}) ->
                    FunctionClauses = lists:map(fun(FunctionClause) ->
                                                  inject_code_change_clause(FunctionClause, Opts)
                                                end, FunctionClauses0),
                    {function, L, code_change, 3, FunctionClauses};
                 (Form) -> Form
              end, Forms).

%% @spec quote(_) -> [any()].
quote(S) ->
  lists:flatten(io_lib:format("\"~s\"", [S])).

%% @spec format_literal('integer' | 'string',_) -> [any()].
format_literal(string, Literal) -> quote(Literal);
format_literal(integer, Literal) ->
    integer_to_list(Literal).

%% code_change(OldVsn, State, Extra) ->
%% @spec match_code_change_args([any(),...]) -> {'undefined' | [{'integer',_,_} | {'string',_,_} | {'tuple',_,[any(),...]} | {'var',_,'__Extra0__' | '__OldVsn__' | '__State0__'},...],[{'extra_ret',[any()]} | {'old_vsn_arg',[any()]} | {'old_vsn_ret',[any()]} | {'state_ret',[any()]}]}.
match_code_change_args([{var, L0, OldVsnVar}, {var, L1, StateVar}, {var, L2, ExtraVar}]) ->
    NewArgs = [{var, L0, '__OldVsn__'},
               {var, L1, '__State0__'},
               {var, L2, '__Extra0__'}],
    Vars = [{old_vsn_ret, atom_to_list(OldVsnVar)},
            {old_vsn_arg, "__OldVsn__"},
            {state_ret, atom_to_list(StateVar)},
            {extra_ret, atom_to_list(ExtraVar)}],
    {NewArgs, Vars};
% code_change({down, OldVsn}, State0, Extra) ->
match_code_change_args([{tuple, L0, [{atom, _, down}, {var, _, OldVsnVar}]},
                        {var, L1, StateVar}, {var, L2, ExtraVar}]) ->
    NewArgs = [{tuple, L0, [{atom, L0, down}, {var, L0, '__OldVsn__'}]},
               {var, L1, '__State0__'},
               {var, L2, '__Extra0__'}],
    Vars = [{old_vsn_ret, atom_to_list(OldVsnVar)},
            {old_vsn_arg, "{down, __OldVsn__}"},
            {state_ret, atom_to_list(StateVar)},
            {extra_ret, atom_to_list(ExtraVar)}],
    {NewArgs, Vars};
% code_change({down, "1.0.9" | 298571625185962019378328193930557187913}, State, Extra) ->
match_code_change_args([{tuple, L0, [{atom, _, down}, {LiteralType, _, OldVsnLiteral}]},
                        {var, L1, StateVar}, {var, L2, ExtraVar}]) ->
    NewArgs = [{tuple, L0, [{atom, L0, down}, {LiteralType, L0, OldVsnLiteral}]},
               {var, L1, '__State0__'},
               {var, L2, '__Extra0__'}],
    Vars = [{old_vsn_ret, "_"},
            {old_vsn_arg, io_lib:format("{down, ~s}",
                                        [format_literal(LiteralType, OldVsnLiteral)])},
            {state_ret, atom_to_list(StateVar)},
            {extra_ret, atom_to_list(ExtraVar)}],
    {NewArgs, Vars};
% code_change("1.0.9" | 298571625185962019378328193930557187913, State, Extra) ->
match_code_change_args([{LiteralType, L0, OldVsnLiteral},
                        {var, L1, StateVar}, {var, L2, ExtraVar}]) ->
    NewArgs = [{LiteralType, L0, OldVsnLiteral},
               {var, L1, '__State0__'},
               {var, L2, '__Extra0__'}],
    Vars = [{old_vsn_ret, "_"},
            {old_vsn_arg, format_literal(LiteralType, OldVsnLiteral)},
            {state_ret, atom_to_list(StateVar)},
            {extra_ret, atom_to_list(ExtraVar)}],
    {NewArgs, Vars};
% any other code change clause is rejected
match_code_change_args([_, _, _]) ->
    {undefined, []}.

% TODO - upply the code change line to the call
-spec apply_code_change_line(_, erl_parse:abstract_form()) -> erl_parse:abstract_form() | [erl_parse:abstract_expr()].
%% @spec apply_code_change_line(_,_) -> any().
apply_code_change_line(_L, AbstConvertCall) -> AbstConvertCall.

%% @spec inject_code_change_clause({'clause',_,[any(),...],[],_},[{'app',[any()]} | {'lib_dir',binary() | [any()]} | {'plugin_dir',_} | {'rel_dir',binary() | [any()]} | {'version',_},...]) -> {'clause',_,[any(),...],[],_}.
inject_code_change_clause({clause, L, Args, [], Body0} = Clause, Opts) ->
    %% modify the args, rename the second argument which is the state to be migrated
    %% and the third one which is the extra options
    %% take into account that the first argument can either be a tuple or an atom
    %% http://erlang.org/doc/man/gen_server.html#Module:code_change-3
    {NewArgs, Vars} = match_code_change_args(Args),
    OldVsnArgStr = proplists:get_value(old_vsn_arg, Vars),
    OldVsnRetStr = proplists:get_value(old_vsn_ret, Vars),
    StateRetStr = proplists:get_value(state_ret, Vars),
    ExtraRetStr = proplists:get_value(extra_ret, Vars),

    case NewArgs of
      undefined ->
        rebar_api:info("unable to inject code change in unrecognized function clause with args (~p)",
          [Args]),
        Clause;
      _ ->
        %% now inject the beginning of the body with a call to our convert method
        {ok, ConvertCallTemplate} = file:read_file(
                                      filename:join([proplists:get_value(plugin_dir, Opts),
                                                     ?PRIV_DIR,
                                                     ?CONVERT_CALL_TEMPLATE])),
        ConvertCallCtx = [{"old_vsn_arg", OldVsnArgStr},
                          {"old_vsn_ret", OldVsnRetStr},
                          {"state_ret", StateRetStr},
                          {"extra_ret", ExtraRetStr}],
        ConvertCall = bbmustache:render(ConvertCallTemplate, ConvertCallCtx),
        AbstConvertCall = apply_code_change_line(L, to_abstract(binary_to_list(ConvertCall))),
        case is_list(AbstConvertCall) of
            true ->
                {clause, L, NewArgs, [], AbstConvertCall ++ Body0};
            false ->
                {clause, L, NewArgs, [], [AbstConvertCall | Body0]}
        end
    end.

-spec to_abstract(string()) -> erl_parse:abstract_form() | [erl_parse:abstract_expr()].
%% @spec to_abstract(string()) -> erl_parse:abstract_form().
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
