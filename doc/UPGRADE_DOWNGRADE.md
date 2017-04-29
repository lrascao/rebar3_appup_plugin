# Upgrade / Downgrade between release versions

Using rebar3 the most simple flow necessary to generate the `.appup` files is:

   * Checkout the original version (the one that is running in production right now)
   * `rebar3 release` in order to generate that release
   * Checkout the version you wish to upgrade to
   * `rebar3 release` again in order to generate the destination version release
   * We now have both version releases (source and destination), run the generate command: `rebar3 appup generate` (since there are only two versions there is no ambiguity), an `.appup` file is generated containing the necessary instructions to upgrade/downgrade between versions.
   * Now that we are in the possession of the .appup we can ask rebar3 to generate the relup: `rebar3 relup`
   * Finally pack the resulting release into a tarball that is ready to be deployed to production: `rebar3 tar`

Using the example [relapp1 release](https://github.com/lrascao/relapp1) (this app is also used for the plugin's regression tests):

    $ git clone https://github.com/lrascao/relapp1.git relapp1
    # checkout the original version that it's running somewhere live
    $ git checkout 1.0.11
    $ rebar3 release
    # checkout the version with the hotfix we need to deploy
    $ git checkout 1.0.12
    $ rebar3 release
    # generate the .appup that contains the necessary instructions for the upgrade from 1.0.11 to 1.0.12
    # and the downgrade from 1.0.12 to 1.0.11
    $ rebar3 appup generate
    $ ===> Generated appup ("1.0.11" <-> "1.0.12") for relapp in
    $ "_build/default/lib/relapp/ebin/relapp.appup"
    # generate the relup which essentially takes the appup instructions and
    # translates them to lower level VM instructions
    $ rebar3 relup
    # finally generate a tarball with the 1.0.12 upgrade that is ready for deployment
    $ rebar3 tar

## Deploying the upgrade

Deploying the new version is also straightforward, it involves (in the live machine):
   * Copying the new tarball to the releases folder (ie. releases/relapp-1.0.12.tar.gz)
   * Issue the upgrade/downgrade command

In the case of the example release upgrading from 1.0.11 to 1.0.12:

    $ cd <live_release_dir>
    $ cp relapp-1.0.12.tar.gz releases
    $ bin/relapp upgrade 1.0.12

## Soft vs Brutal purge

In the `update` and `load_module` `appup` directives there is a parameter that specifies the type of purge that the Erlang VM will apply when loading code. There are two phases: pre and post purge and two types of purge: soft and brutal, according to the [doc](http://erlang.org/doc/man/appup.html):

```
PrePurge
  Defaults to brutal_purge. It controls what action to take with processes
  executing old code before loading the new module version. If the value is
   brutal_purge, the processes are killed. If the value is soft_purge,
    release_handler:install_release/1 returns {error,{old_processes,Mod}}.
```

```
PostPurge
  Defaults to brutal_purge. It controls what action to take with processes
  that are executing old code when the new module version has been loaded. If
  the value is brutal_purge, the code is purged when the release is made
  permanent and the processes are killed. If the value is soft_purge, the
  release handler purges the old code when no remaining processes execute the
  code.
```

A good example of this behaviour in practice is in [a test app commit](https://github.com/lrascao/relapp1/commit/563c4ac642bf912949ca00db5553da57a7303c5c), in this example `relapp_m1` (a simple library module) sends an anonymous function that it declared to the `relapp_srv2` `gen_server` that will store in it's state. If you look into it's state you will see something like this:
```
{state,0,<<"name">>,<<"description">>,
       #Fun<relapp_m1.0.72101784>}
```

Now what happens if we load a new version of `relapp_m1`? This is where the purge option comes into play, a `brutal_purge` will kill the gen server process, that might not be the ideal method since the supervisor's restart intensity could be reached, perhaps in this case a `soft_purge` should be chosen instead and then manually restart the gen server or supervisor tree. As a general rule it is best not to send functions across processes if this is to be avoided.

As of ERTS version 9.0 this behaviour has been changed, according to the [erlang:check_process_code/3 doc](http://erlang.org/doc/man/erlang.html#check_process_code-3):
```
As of ERTS version 9.0, the check process code operation only checks for direct references to the code. Indirect references via funs will be ignored. If such funs exist and are used after a purge of the old code, an exception will be raised upon usage (same as the case when the fun is received by the process after the purge). 
```
This means that the release upgrade will go through with no error and only when the purged method is invoked will the process that referenced it crashes.

