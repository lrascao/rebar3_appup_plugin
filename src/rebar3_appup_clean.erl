-module(rebar3_appup_clean).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

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
            ]},
            {example, "rebar3 appup clean"},
            {short_desc, "Cleans all .appup files in the target dir"},
            {desc, "Appup clean"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            AppInfo ->
                [AppInfo]
           end,
    lists:foreach(fun(AppInfo) ->
        Opts = rebar_app_info:opts(AppInfo),
        TargetDir = rebar_app_info:ebin_dir(AppInfo),
        rebar_base_compiler:run(Opts, [],
                                TargetDir, ".appup",
                                "", "",
                                fun(Source, _Target, _Config) ->
                                    rebar_file_utils:delete_each([Source])
                                end)
    end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
