rebar3_appup_plugin
=====

A rebar3 plugin for handling .appup files. Contains the instructions necessary to upgrade from one release to another.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [rebar3_appup_plugin]}.

and the provider hooks:

```
  {provider_hooks, [
      {pre, [{compile, {appup, compile}}]},
      {pre, [{clean, {appup, clean}}]}
  ]}.
```

```
    $ rebar3 help appup
    appup <task>:
        clean           Cleans all .appup files
        compile         Compile and validate all .appup.src files
        generate        Compare two different releases and generate the .appup file
```

Generate
---

```
    Appup generator
    Usage: rebar3 appup generate [-p <previous>] [-c <current>]
                                 [-t <target_dir>]

      -p, --previous    location of the previous release
      -c, --current     location of the current release
      -t, --target_dir  target dir in which to generate the .appups to
```

    rebar3 appup generate --previous /path/to/previous/release [--current /path/to/current/release] [--target_dir /path/to/target/dir]

  * previous and current options should be full path names to the release (ie. the folder that contains lib/, releases/)
  * current is optional and defaults to _build/`<profile>`/rel
  * target_dir is optional and defaults to `_build/`<profile>`/rel/`<app>`/lib/`<app>`/ebin`

Compile
---

    rebar3 appup compile

Clean
---

    rebar3 appup clean
