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
-module(rebar3_appup_plugin_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

%% -------------------------------------------------------------
%% Callback Functions
%% -------------------------------------------------------------

all() ->
    [{group, generate}].

groups() ->
    [{generate, [],
        [empty_appup, supervisor_appup,
         new_gen_server_appup,
         new_gen_server_state_appup,
         add_fields_gen_server_state_appup,
         add_field_middle_gen_server_state_appup,
         replace_field_middle_gen_server_state_appup,
         new_dependency_appup,
         remove_dependency_appup,
         restore_dependency_appup,
         new_auto_gen_server_appup,
         add_fields_auto_gen_server_state_appup,
         new_simple_module, simple_module_use,
         brutal_purge_test, soft_purge_test]
     }].

init_per_suite(Config) ->
    DataDir = lookup_config(data_dir, Config),
    SrcDir = filename:join(lists:takewhile(fun("_build") -> false;
                                              (_) -> true
                                           end, filename:split(DataDir))),
    SuiteConfig = ct:get_config(config),
    % log("suite config: ~p\n", [SuiteConfig]),
    % log("src dir: ~p\n", [SrcDir]),
    Apps = proplists:get_value(apps, SuiteConfig),
    lists:foreach(fun({App, GitUrl}) ->
                    git_clone(GitUrl, App, DataDir),
                    {ok, _} = sh(io_lib:format("rsync -a --exclude='_build' ~s .",
                                               [SrcDir]),
                                 [], DataDir),
                    RelDir = filename:join([DataDir, App]),
                    {ok, _} = sh(io_lib:format("cp -f rebar3 ~s",
                                               [RelDir]),
                                 [], SrcDir),
                    {ok, _} = sh("mkdir -p _checkouts", [], RelDir),
                    CheckoutsDir = filename:join([RelDir, "_checkouts"]),
                    {ok, _} = sh(io_lib:format("ln -s ~s/rebar3_appup_plugin rebar3_appup_plugin",
                                               [DataDir]),
                                 [], CheckoutsDir)
                  end, Apps),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(_GroupName, Config) ->
    % ct:print(default, 50, "starting test group: ~p", [GroupName]),
    Config.

end_per_group(_GroupName, Config) ->
    % ct:print(default, 50, "ending test group: ~p", [GroupName]),
    Config.

%% included for test server compatibility
%% assume that all test cases only takes Config as sole argument
init_per_testcase(_Func, Config) ->
    global:register_name(rebar3_appup_plugin_global_logger, group_leader()),
    Config.

end_per_testcase(_Func, Config) ->
    DataDir = lookup_config(data_dir, Config),
    PrivDir = lookup_config(priv_dir, Config),
    SuiteConfig = ct:get_config(config),
    Apps = proplists:get_value(apps, SuiteConfig),
    lists:foreach(fun({App, _}) ->
                    Dir = filename:join(DataDir, App),
                    {ok, _} = sh("rm -rf _build/default/rel", [], Dir),
                    {ok, _} = sh("rm -rf _build/default/lib/relapp/ebin/relapp.appup", [], Dir)
                  end, Apps),
    {ok, _} = sh("rm -rf " ++ PrivDir, [], DataDir),
    global:unregister_name(rebar3_appup_plugin_global_logger),
    Config.

%% -------------------------------------------------------------
%% Test Cases
%% -------------------------------------------------------------

empty_appup(doc) -> ["Generate an empty appup"];
empty_appup(suite) -> [];
empty_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.0", "1.0.1",
                           {[], []},
                           Config),
    ok.

supervisor_appup(doc) -> ["Generate an appup for a supervisor upgrade"];
supervisor_appup(suite) -> [];
supervisor_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.1", "1.0.2",
                           {[{update, relapp_sup, supervisor}],
                            [{update, relapp_sup, supervisor}]},
                           Config),
    ok.

new_gen_server_appup(doc) -> ["Generate an appup for a gen_server upgrade"];
new_gen_server_appup(suite) -> [];
new_gen_server_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.2", "1.0.3",
                           {[{add_module, relapp_srv, []},
                             {update, relapp_sup, supervisor}],
                            [{update,relapp_sup,supervisor},
                             {delete_module,relapp_srv}]},
                           Config),
    ok.

new_gen_server_state_appup(doc) -> ["Generate an appup for a gen_server upgrade that involves "
                                    "replacing it's state with an empty record"];
new_gen_server_state_appup(suite) -> [];
new_gen_server_state_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.3", "1.0.4",
                           {[{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}],
                            [{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}]},
                           Config),
    ok.

add_fields_gen_server_state_appup(doc) -> ["Generate an appup for a gen_server upgrade that involves "
                                           "adding two new fields to the state record"];
add_fields_gen_server_state_appup(suite) -> [];
add_fields_gen_server_state_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.4", "1.0.5",
                           {[{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}],
                            [{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}]},
                           Config),
    ok.

add_field_middle_gen_server_state_appup(doc) -> ["Generate an appup for a gen_server upgrade that involves "
                                                 "adding a new field to the middle of the record state"];
add_field_middle_gen_server_state_appup(suite) -> [];
add_field_middle_gen_server_state_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.5", "1.0.6",
                           {[{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}],
                            [{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}]},
                           Config),
    ok.

replace_field_middle_gen_server_state_appup(doc) -> ["Generate an appup for a gen_server upgrade that involves "
                                                     "replacing a field to the middle of the record state "
                                                     "with a new one"];
replace_field_middle_gen_server_state_appup(suite) -> [];
replace_field_middle_gen_server_state_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.6", "1.0.7",
                           {[{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}],
                            [{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, []}]},
                           Config),
    ok.

new_dependency_appup(doc) -> ["Generate an appup for an application that involves "
                              "adding a new dependency"];
new_dependency_appup(suite) -> [];
new_dependency_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.7", "1.0.8",
                           {[], []},
                           Config),
    ok.

remove_dependency_appup(doc) -> ["Generate an appup for an application that involves "
                                 "removings an existing dependency"];
remove_dependency_appup(suite) -> [];
remove_dependency_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.8", "1.0.9",
                           {[], []},
                           Config),
    ok.

restore_dependency_appup(doc) -> ["Generate an appup for an application that involves "
                                  "restores back a dependency that was removed"];
restore_dependency_appup(suite) -> [];
restore_dependency_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.9", "1.0.10",
                           {[], []},
                           Config),
    ok.

new_auto_gen_server_appup(doc) -> ["Generate an appup for a gen_server upgrade "
                                   "that is prepared to support automatic state "
                                   "migration"];
new_auto_gen_server_appup(suite) -> [];
new_auto_gen_server_appup(Config) when is_list(Config) ->
    ok = upgrade_downgrade("relapp1", "1.0.10", "1.0.11",
                           {[{add_module, relapp_srv2, []},
                             {update, relapp_sup, supervisor}],
                            [{update,relapp_sup,supervisor},
                             {delete_module,relapp_srv2}]},
                           Config),
    ok.

add_fields_auto_gen_server_state_appup(doc) ->
    ["Generate an appup for a gen_server upgrade that involves "
     "adding two new fields to the state record and automatically "
     "migrating the state of the old record to the new one"];
add_fields_auto_gen_server_state_appup(suite) -> [];
add_fields_auto_gen_server_state_appup(Config) when is_list(Config) ->
    BeforeUpgradeFun = fun(DeployDir, State) ->
                            {ok, Srv2State0} = sh("./bin/relapp eval "
                                              "\"sys:get_state(relapp_srv2).\"",
                                              [], DeployDir),
                            true = (Srv2State0 =:= "{state,0}"),
                            sh("./bin/relapp eval "
                               "\"relapp_srv2:set_state({state, 42}).\"",
                               [], DeployDir),
                            {ok, Srv2State1} = sh("./bin/relapp eval "
                                              "\"sys:get_state(relapp_srv2).\"",
                                              [], DeployDir),
                            true = (Srv2State1 =:= "{state,42}"),
                            State
                       end,
    AfterUpgradeFun = fun(DeployDir, State) ->
                            {ok, Srv2State} = sh("./bin/relapp eval "
                                             "\"sys:get_state(relapp_srv2).\"",
                                             [], DeployDir),
                            log("state: ~p\n", [Srv2State]),
                            true = (Srv2State =:= "{state,42,<<>>,<<>>}"),
                            State
                      end,
    AfterDowngradeFun = fun(DeployDir, State) ->
                            {ok, Srv2State} = sh("./bin/relapp eval "
                                             "\"sys:get_state(relapp_srv2).\"",
                                             [], DeployDir),
                            log("state: ~p\n", [Srv2State]),
                            true = (Srv2State =:= "{state,42}"),
                            State
                        end,
    ok = upgrade_downgrade("relapp1", "1.0.11", "1.0.12",
                           [{before_upgrade, BeforeUpgradeFun},
                            {after_upgrade, AfterUpgradeFun},
                            {after_downgrade, AfterDowngradeFun}],
                           {[{update, relapp_srv2, {advanced,[]}, brutal_purge, brutal_purge, []}],
                            [{update, relapp_srv2, {advanced,[]}, brutal_purge, brutal_purge, []}]},
                           [], Config),
    ok.

new_simple_module(doc) -> ["Generate an appup for an upgrade "
                           "that involves loading a new module"];
new_simple_module(suite) -> [];
new_simple_module(Config) when is_list(Config) ->
    AfterUpgradeFun = fun(DeployDir, State) ->
                            {ok, "ok"} = sh("./bin/relapp eval "
                                            "\"relapp_m1:test().\"",
                                            [], DeployDir),
                            State
                      end,
    AfterDowngradeFun = fun(DeployDir, State) ->
                            {ok, Ret} = sh("./bin/relapp eval "
                                             "\"erlang:module_loaded(relapp_m1).\"",
                                             [], DeployDir),
                            true = (Ret =:= "false"),
                            State
                        end,
    ok = upgrade_downgrade("relapp1", "1.0.12", "1.0.13",
                           [{after_upgrade, AfterUpgradeFun},
                            {after_downgrade, AfterDowngradeFun}],
                           {[{add_module, relapp_m1, []}],
                            [{delete_module,relapp_m1}]},
                           [], Config),
    ok.

simple_module_use(doc) -> ["Generate an appup for an upgrade "
                           "an updated module and a gen server that "
                           "will make use of that module"];
simple_module_use(suite) -> [];
simple_module_use(Config) when is_list(Config) ->
    AfterUpgradeFun = fun(DeployDir, State) ->
                            {ok, "{ok,arg}"} = sh("./bin/relapp eval "
                                            "\"relapp_m1:test(arg).\"",
                                            [], DeployDir),
                            State
                      end,
    AfterDowngradeFun = fun(DeployDir, State) ->
                            {ok, "ok"} = sh("./bin/relapp eval "
                                             "\"relapp_m1:test().\"",
                                             [], DeployDir),
                            State
                        end,
    ok = upgrade_downgrade("relapp1", "1.0.13", "1.0.14",
                           [{after_upgrade, AfterUpgradeFun},
                            {after_downgrade, AfterDowngradeFun}],
                           {[{load_module, relapp_m1, brutal_purge, brutal_purge, []},
                             {update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, [relapp_m1]}],
                            [{update, relapp_srv, {advanced,[]}, brutal_purge, brutal_purge, [relapp_m1]},
                             {load_module, relapp_m1, brutal_purge, brutal_purge, []}]},
                           [], Config),
    ok.

brutal_purge_test(doc) -> ["Generate an appup for an upgrade "
                           "that involves loading a module that had sent "
                           "one of it's methods to a gen_server's state"];
brutal_purge_test(suite) -> [];
brutal_purge_test(Config) when is_list(Config) ->
    BeforeUpgradeFun = fun(DeployDir, State) ->
                            {ok, "ok"} = sh("./bin/relapp eval "
                                            "\"relapp_m1:store_fun().\"",
                                            [], DeployDir),
                            State
                      end,
    CheckUpgradeFun = fun(DeployDir, State) ->
                        %% the upgrade will succeed, however will be killed due to it
                        {ok, _} = proplists:get_value(upgrade_result, State),
                        %% since this is a brutal purge, the gen server will get killed
                        %% and cause the application to stop due to reached maximum supervisor
                        %% intensity
                        %% wait a bit
                        timer:sleep(4000),
                        %% then ensure it actually died
                        true = ({ok, "pong"} =/= sh("./bin/relapp ping ", [], DeployDir)),
                        %% restart it
                        {ok, _} = sh("./bin/relapp start ", [], DeployDir),
                        %% wait for it to start
                        ok = wait_for_node_start(DeployDir),
                        State
                      end,
    ok = upgrade_downgrade("relapp1", "1.0.15", "1.0.16",
                           [{before_upgrade, BeforeUpgradeFun},
                            {check_upgrade, CheckUpgradeFun}],
                           {[{load_module, relapp_m1, brutal_purge, brutal_purge, []}],
                            [{load_module, relapp_m1, brutal_purge, brutal_purge, []}]},
                           [{generate_opts, "--purge \"relapp_m1=brutal\""}],
                           Config),
    ok.

soft_purge_test(doc) -> ["Generate an appup for an upgrade "
                         "that involves loading a module that had sent "
                         "one of it's methods to a gen_server's state"];
soft_purge_test(suite) -> [];
soft_purge_test(Config) when is_list(Config) ->
    BeforeUpgradeFun = fun(DeployDir, State) ->
                            {ok, "ok"} = sh("./bin/relapp eval "
                                            "\"relapp_m1:store_fun().\"",
                                            [], DeployDir),
                            {ok, Srv2Pid} = sh("./bin/relapp eval "
                                               "\"whereis(relapp_srv2).\"",
                                            [], DeployDir),
                            State ++ [{srv2_pid, Srv2Pid}]
                      end,
    CheckUpgradeFun = fun(_DeployDir, State) ->
                        {ok, _} = proplists:get_value(upgrade_result, State),
                        State
                      end,
    AfterUpgradeFun = fun(DeployDir, State) ->
                            {ok, NewSrv2Pid} = sh("./bin/relapp eval "
                                                  "\"whereis(relapp_srv2).\"",
                                                  [], DeployDir),
                            true = (NewSrv2Pid =:= proplists:get_value(srv2_pid, State)),
                            {ok, "ok"} =
                                  sh("./bin/relapp eval "
                                     "\"relapp_srv2:call_stored_fun().\"",
                                     [], DeployDir),
                            State
                      end,
    CheckDowngradeFun = fun(_DeployDir, State) ->
                        DowngradeResult = proplists:get_value(downgrade_result, State),
                        %% since this is a soft purge we expect an error from the release
                        %% handler
                        % escript: exception error: no case clause matching {error,{old_processes,relapp_m1}}
                        true = ({error,127} =:= DowngradeResult),
                        State
                      end,
    ok = upgrade_downgrade("relapp1", "1.0.15", "1.0.16",
                           [{before_upgrade, BeforeUpgradeFun},
                            {check_upgrade, CheckUpgradeFun},
                            {after_upgrade, AfterUpgradeFun},
                            {check_downgrade, CheckDowngradeFun}],
                           {[{load_module, relapp_m1, soft_purge, soft_purge, []}],
                            [{load_module, relapp_m1, soft_purge, soft_purge, []}]},
                           [{generate_opts, "--purge \"relapp_m1=soft\""}],
                           Config),
    ok.

%% -------------------------------------------------------------
%% Private methods
%% -------------------------------------------------------------

upgrade_downgrade(App, FromVersion, ToVersion,
                  ExpectedAppup, Config) ->
    upgrade_downgrade(App, FromVersion, ToVersion,
                      [],
                      ExpectedAppup,
                      [], Config).

upgrade_downgrade(App, FromVersion, ToVersion,
                  Hooks,
                  ExpectedAppup,
                  Opts, Config) ->
    DataDir = lookup_config(data_dir, Config),
    RelAppDir = filename:join(DataDir, App),
    %% check out the from version
    {ok, _} = git_checkout(RelAppDir, FromVersion),
    {ok, _} = rebar3_command(RelAppDir, "tar"),
    %% check out the to version
    {ok, _} = git_checkout(RelAppDir, ToVersion),
    {ok, _} = rebar3_command(RelAppDir, "release"),
    %% now generate the appup
    GenerateOpts = proplists:get_value(generate_opts, Opts, ""),
    {ok, _} = rebar3_command(RelAppDir, "appup generate " ++  GenerateOpts),
    %% ensure that we have an expected appup file
    ExpectedAppup = check_appup(RelAppDir, "relapp", FromVersion, ToVersion),
    %% now generate the relup
    {ok, _} = rebar3_command(RelAppDir, "relup"),
    {ok, _} = rebar3_command(RelAppDir, "tar"),
    %% and now actually perform it on a running release
    ok = release_upgrade_downgrade(RelAppDir, "relapp",
                                   FromVersion, ToVersion,
                                   Hooks,
                                   Config),
    ok.

release_upgrade_downgrade(RelDir, AppName,
                          FromVersion, ToVersion,
                          Hooks,
                          Config) ->
    PrivDir = lookup_config(priv_dir, Config),
    %% deploy the from version
    DeployDir = filename:join(PrivDir, FromVersion),
    ok = filelib:ensure_dir(filename:join(DeployDir, "dummy")),
    Src0 = filename:join([RelDir, "_build/default/rel",
                          AppName,
                          AppName ++ "-" ++ FromVersion ++ ".tar.gz"]),
    Dst0 = filename:join(DeployDir, "app.tar.gz"),
    {ok, _} = file:copy(Src0, Dst0),
    log("copied from version ~p to ~p\n", [Src0, Dst0]),
    {ok, _} = sh("tar xzf app.tar.gz", [], DeployDir),
    {ok, _} = sh("rm -rf app.tar.gz", [], DeployDir),
    %% start the app
    {ok, _} = sh("./bin/relapp start ", [], DeployDir),
    %% wait for it to start
    ok = wait_for_node_start(DeployDir),
    {ok, FromVersion} = get_running_version(DeployDir),
    % %% prepare the upgrade to the to version
    {ok, _} = sh("mkdir releases/" ++ ToVersion, [], DeployDir),
    Src1 = filename:join([RelDir, "_build/default/rel",
                          AppName,
                          AppName ++ "-" ++ ToVersion ++ ".tar.gz"]),
    Dst1 = filename:join([DeployDir,
                          "releases", ToVersion,
                          AppName ++ ".tar.gz"]),
    {ok, _} = file:copy(Src1, Dst1),
    log("copied to version ~p to ~p\n", [Src1, Dst1]),
    State0 = maybe_run_hook(proplists:get_value(before_upgrade, Hooks),
                            DeployDir, []),
    %% and upgrade it
    UpgradeResult = sh("./bin/relapp upgrade " ++ ToVersion, [], DeployDir),
    State1 = maybe_run_hook(proplists:get_value(check_upgrade, Hooks,
                                                fun default_check_upgrade/2),
                            DeployDir, State0 ++ [{upgrade_result, UpgradeResult}]),
    %% ensure that the upgrade really went through
    {ok, ToVersion} = get_running_version(DeployDir),
    State2 = maybe_run_hook(proplists:get_value(after_upgrade, Hooks),
                            DeployDir, State1),
    %% now downgrade it
    Dst2 = filename:join([DeployDir,
                          "releases", FromVersion,
                          AppName ++ ".tar.gz"]),
    {ok, _} = file:copy(Src0, Dst2),
    State3 = maybe_run_hook(proplists:get_value(before_downgrade, Hooks),
                            DeployDir, State2),
    DowngradeResult = sh("./bin/relapp downgrade " ++ FromVersion, [], DeployDir),
    State4 = maybe_run_hook(proplists:get_value(check_downgrade, Hooks,
                                                fun default_check_downgrade/2),
                            DeployDir, State3 ++ [{downgrade_result, DowngradeResult}]),
    %% ensure that the downgrade really went through
    {ok, FromVersion} = get_running_version(DeployDir),
    maybe_run_hook(proplists:get_value(after_downgrade, Hooks),
                   DeployDir, State4),
    %% stop the app
    {ok, _} = sh("./bin/relapp stop ", [], DeployDir),
    %% wait for it to stop
    ok = wait_for_node_stop(DeployDir),
    ok.

default_check_upgrade(_DeployDir, State) ->
    UpgradeResult = proplists:get_value(upgrade_result, State),
    {ok, _} = UpgradeResult,
    State.

default_check_downgrade(_DeployDir, State) ->
    DowngradeResult = proplists:get_value(downgrade_result, State),
    {ok, _} = DowngradeResult,
    State.

maybe_run_hook(undefined, _DeployDir, State) -> State;
maybe_run_hook(Hook, DeployDir, State) ->
    Hook(DeployDir, State).

wait_for_node_start(DeployDir) ->
    case sh("./bin/relapp ping ", [], DeployDir) of
        {ok, "pong"} -> ok;
        _ ->
            timer:sleep(1000),
            wait_for_node_start(DeployDir)
    end.

wait_for_node_stop(DeployDir) ->
    case sh("./bin/relapp ping ", [], DeployDir) of
        {ok, "pong"} ->
            timer:sleep(1000),
            wait_for_node_stop(DeployDir);
        _ -> ok
    end.

get_running_version(DeployDir) ->
    case sh("./bin/relapp eval "
            "\"{relapp, _, VersionStr} = "
            "lists:keyfind(relapp, 1, application:which_applications()),"
            "VersionStr.\"", [], DeployDir) of
        {ok, Version} ->
            %% unescape the string quotes
            {ok, re:replace(Version, "\"", "", [global, {return, list}])};
        _ -> {error, failed}
    end.

check_appup(RelDir, AppName, FromVersion, ToVersion) ->
    %% the .appup is generated to two locations:
    %% _build/default/lib/relapp/ebin/relapp.appup
    %% _build/default/rel/relapp/lib/relapp-1.0.1/ebin/relapp.appup
    log("checking appup on ~p from ~p to ~p\n",
        [RelDir, FromVersion, ToVersion]),
    {ok, EbinAppup} = file:consult(filename:join([RelDir,
                                            "_build/default/lib",
                                            AppName, "ebin",
                                            AppName ++ ".appup"])),
    % log("appup: ~p\n", [EbinAppup]),
    case lists:keysearch(ToVersion, 1, EbinAppup) of
        {value, {ToVersion, UpFromVsn, DownToVsn}} ->
            [{FromVersion, UpgradeInstructions}] = UpFromVsn,
            [{FromVersion, DowngradeInstructions}] = DownToVsn,
            {UpgradeInstructions, DowngradeInstructions};
        _ -> {undefined, undefined}
    end.

log(Format, Args) ->
    case global:whereis_name(rebar3_appup_plugin_global_logger) of
        undefined ->
            io:format(user, Format, Args);
        Pid ->
            io:format(Pid, Format, Args)
    end.

lookup_config(Key,Config) ->
    case lists:keysearch(Key,1,Config) of
    {value,{Key,Val}} ->
        Val;
    _ ->
        []
    end.

rebar3_command(Dir, Command) ->
    rebar3_command(Dir, Command, []).

rebar3_command(Dir, Command, []) ->
    sh("./rebar3 " ++ Command, [], Dir);
rebar3_command(Dir, Command, [debug]) ->
    sh("./rebar3 " ++ Command, [{"DEBUG", "1"}], Dir).

git_checkout(Dir, Tag) ->
    sh("git checkout " ++ Tag, [], Dir).

git_clone(Url, Name, Dir) ->
    sh("git clone " ++ Url ++ " " ++ Name, [], Dir).

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

sh(Command, Env) ->
    sh(Command, Env, get_cwd()).

sh(Command, Env, Dir) ->
    log("sh: ~s [~p]\n~p\n", [Command, Dir, Env]),
    Port = open_port({spawn, lists:flatten(Command)},
                     [{cd, Dir},
                      {env, Env},
                      exit_status,
                      {line, 16384},
                      use_stdio, stderr_to_stdout]),
    case sh_loop(Port) of
        {ok, Ret} ->
            {ok, Ret};
        {error, Rc} ->
            log("~s failed with error: ~w\n", [Command, Rc]),
            {error, Rc}
    end.

sh_loop(Port) ->
    sh_loop(Port, "").

sh_loop(Port, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            log("~s\n", [Line]),
            sh_loop(Port, Acc ++ Line);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.
