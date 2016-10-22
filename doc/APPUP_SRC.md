# Using an .appup.src file

The plugin will search for any files ending in `.appup.src`, evaluate them and write the result to the `.appup` file. The last term for the expression in this file should a properly formatted `.appup` term.
Here is a simple example in the [relapp test release](https://github.com/lrascao/relapp1/commit/bd301cbc7a93e7187da074557fa7d3c3faf14b63):

```
Version = "1.0.17",
{ok, V} = relapp_m1:test("1.0.16"),
UpFrom = DownTo = V,
{Version,
    [{UpFrom,
        [{load_module,relapp_m1,brutal_purge,brutal_purge,
                              [relapp_srv2]},
                 {update,relapp_srv,
                         {advanced,[]},
                         brutal_purge,brutal_purge,
                         [relapp_m1]},
                 {update,relapp_srv2,
                         {advanced,[]},
                         brutal_purge,brutal_purge,[]}]}],
    [{DownTo,
        [{update,relapp_srv2,
                         {advanced,[]},
                         brutal_purge,brutal_purge,[]},
                 {update,relapp_srv,
                         {advanced,[]},
                         brutal_purge,brutal_purge,
                         [relapp_m1]},
                 {load_module,relapp_m1,brutal_purge,brutal_purge,
                              [relapp_srv2]}]}
]}.
```

Note that we are able to access methods in our codebase (ie. the `relapp_m1` module).

## Templated .appup.src

Before being evaluated there is a template variable substitution phase using [mustache templates](https://github.com/soranoba/bbmustache), the following variables are available:
   * `vsn` - the current release version

Here is an example of this in practice in the [relapp test release](https://github.com/lrascao/relapp1/commit/423c284b):

```
{"{{vsn}}",
    [
     {<<".*">>, [{restart_application, relapp}]}
    ],
    [
     {<<".*">>, [{restart_application, relapp}]}
    ]
}.
```

## Built-in variables

The following built-in variables are exposed and made available when evaluating the `.appup.src`:
   * STATE - `rebar3`'s internal state

## Helper methods

The plugin exposes these methods in order to ease extraction of useful
information from the state:
   * `rebar3_appup_utils:find_app_info(Name :: binary(), State :: #state_t{}) -> #app_info_t{}`. Given an app name obtains it's app info record from rebar's state, `rebar_app_info` methods can then be used to extract all kinds of information.

Here is another example from the [relapp test release](https://github.com/lrascao/relapp1/commit/ee2faf07):

```
%% find our app info in rebar's STATE
AppInfo = rebar3_appup_utils:find_app_info(<<"relapp">>, STATE),
"{{vsn}}" = rebar_app_info:original_vsn(AppInfo),
{"{{vsn}}",
    [
     {<<".*">>, [{restart_application, relapp}]}
    ],
    [
     {<<".*">>, [{restart_application, relapp}]}
    ]
}.
```
