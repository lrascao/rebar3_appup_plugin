# Providing a custom dependency .appup.src file

There is sometimes the need to provide `.appup` files for external dependencies.

A lot of the times it's not possible to timely integrate the `.appup` pull requests for them, maybe you're not bumping to the latest version and the maintainer is not willing to create a support branch just for you.

The plugin offers a way out of this by searching not only the application's `appup.src` files but others as well, it then processes them as usual and moves to the dependency directory which goes through he usual release upgrade procedure.

Your application will be the one maintaining the `.appup.src` files of dependencies that it's interested in generating release upgrades from, you just keep them alonsing your own `.appup.src`, the name of the file must match the name of the dependency.

You can find an example of this in the [relapp test release](https://github.com/lrascao/relapp1/commit/488b9501d88166ee9792e14ee676469f790384f1), in this case `relapp1` is maintaining a `statsderl.appup.src` file that will be processed and copied to the output of the `statsderl` application:

```
{ "0.5.2",
    [{ <<".*">>,
        [{restart_application, statsderl}]
    }],
    [{ <<".*">>,
        [{restart_application, statsderl}]
    }]
}.
```

Here we're saying that to move from any version to `0.5.2` of the `statsderl` dependency (either upgrading or downgrading) we just restart the application.
