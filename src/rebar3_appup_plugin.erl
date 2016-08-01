-module(rebar3_appup_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_appup_generate:init(State0),
    {ok, State2} = rebar3_appup_compile:init(State1),
    {ok, State3} = rebar3_appup_clean:init(State2),
    {ok, State4} = rebar3_appup_tar:init(State3),
    {ok, State4}.
