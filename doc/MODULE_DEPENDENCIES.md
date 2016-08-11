# Module dependencies

OTP allows you to define a list of module dependencies that each upgraded module is dependent on, as described in [the upgrade instructions doc](http://erlang.org/doc/man/appup.html#Release%20Upgrade%20Instructions):

```
    Defaults to [] and defines other modules that Mod is dependent on. In the
    relup file, instructions for suspending processes using Mod come before
    instructions for suspending processes using modules in DepMods when
    upgrading, and conversely when downgrading. In case of circular
    dependencies, the order of the instructions in the appup file is kept.
```

The appup plugin generates these dependencies list for you by using the `xref` tool, for each generated module in the `.appup` the plugin performs an `xref:analyze/2` that yields a list of modules that it makes calls to, this list is then intersected with the modules being upgraded and added to the relevant entries in the `.appup`.
