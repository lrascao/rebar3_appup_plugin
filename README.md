rebar3 appup plugin
=====

[![Build Status](https://travis-ci.org/lrascao/rebar3_appup_plugin.svg?branch=develop)](https://travis-ci.org/lrascao/rebar3_appup_plugin)
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_appup_plugin.svg)](https://hex.pm/packages/rebar3_appup_plugin)
[![gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/rebar3-appup-plugin/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

A rebar3 plugin for handling release upgrades. Supports the following features:
   * Automatic generation of the `.appup` file containing instructions necessary to upgrade and downgrade from one release to the other. [More info](doc/UPGRADE_DOWNGRADE.md)
   * Validation of any `.appup.src` files that might be present, these are scripts that can contain Erlang code. They are templated, evaluated and their results and written to an `.appup` file that is then moved to the target dir. [More info](doc/APPUP_SRC.md)
   * Handles any dependency `.appup.src` files maintained by your application. [More info](doc/CUSTOM_APPUP.md)
   * Automatic code injection for `gen_server` state record conversion between versions. [More info](doc/STATE_RECORD_CONVERSION.md)
   * Automatically generated module dependencies. [More info](doc/MODULE_DEPENDENCIES.md)

Demo
-----
### Generating a release upgrade

![gif](http://i.imgur.com/zdfAg6o.gif)

### Upgrading a release

![gif](http://i.imgur.com/khWaee2.gif)

Build
-----

    $ rebar3 compile

Configure
---

Add the plugin to your project's rebar.config:

    {plugins, [rebar3_appup_plugin]}.

and the provider hooks:

```
  {provider_hooks, [
      {pre, [{tar, {appup, tar}}]},
      {post, [{compile, {appup, compile}},
              {clean, {appup, clean}}]}
  ]}.
```

Application upgrade generation
---

By generating two releases, the one that is live right now and the one that we want to end up in. We can invoke the plugin and have it generate a special `.appup` file that OTP knows how to process in order to generate a release upgrade file (`relup`). This file contains low level Erlang VM instructions that will transition the Erlang application from one version to another without any downtime. [More info](doc/UPGRADE_DOWNGRADE.md).

Using an .appup.src file
---

You can generate the `.appup` file every time you pack a release with the `rebar3 appup generate` call. However when there is the need to add custom data or instructions to the `.appup` it's useful to have it in source control alongside your `.app.src` file. The `.appup.src` file can contain Erlang code and it's result should be a [valid appup Erlang term](http://erlang.org/doc/man/appup.html). The plugin will look for any files ending in `.appup.src`, perform template variables substitution, evaluate them and have their results written to an `.appup` file that will then be used to generate the relup. [More info](doc/APPUP_SRC.md).

Code Injection
---

When doing relups an issue that comes up often is the problem of ugprading the state record of your `gen_server` code. Most likely you have made some change that needs a new record structure alongside the code that handles it. OTP offers you the `gen_server:code_change/3` call that enables state migration, this implies however that the new code must somehow know about the old record structure to migrate from. The plugin will automatically, by specifying a `-state_record` directive, inject the necessary code into the gen_server's `.beam` file that will take care of the migration. [More info](doc/STATE_RECORD_CONVERSION.md).

Automatically generated module dependencies
---

OTP allows the developer to manually specify the dependencies for each module in the `.appup` file, that is, which modules should be loaded before that one. The plugin takes care of this for you, by using `xref` it discovers which modules are being used and adds them to the relevant `.appup` entries. [More info](doc/MODULE_DEPENDENCIES.md).

Command line options
---

```
    Appup generator
    Usage: rebar3 appup generate

      -p, --previous    location of the previous release
      -c, --current     location of the current release
      -t, --target_dir  target dir in which to generate the .appups to
      -t, --target_dir  target dir in which to generate the .appups to
      -g, --purge       per module purge instructions, format is
                          [default | Module]=Purge | PrePurge/PostPurge
                        where Purge, PrePurge, PostPurge is either soft or brutal. Module is the name of a module in the .appup file, default is a reserved name to apply purge options
                        to unspecified modules.
```

    rebar3 appup generate [--previous /path/to/previous/release] 
    [--previous_version version] [--current /path/to/current/release] 
    [--target_dir /path/to/target/dir] [--purge ]

  * previous and current options should be full path names to the release (ie. the folder that contains lib/, releases/)
  * current is optional and defaults to _build/`<profile>`/rel
  * target_dir is optional and defaults to `_build/`<profile>`/rel/`<app>`/lib/`<app>`/ebin`

Compile
---

    rebar3 appup compile

Clean
---

    rebar3 appup clean

## Copyright and License

Copyright (c) 2016 Luis Rascão

**rebar3_appup_plugin** source code is licensed under [Apache 2.0](LICENSE).
