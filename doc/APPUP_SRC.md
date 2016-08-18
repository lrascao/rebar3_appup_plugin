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
