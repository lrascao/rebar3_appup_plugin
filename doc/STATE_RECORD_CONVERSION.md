# State record conversion

Use of records to contain gen_server's state is widespread in Erlang. Due to the fact that records are in fact just tuples a problem arises when performing relups between release versions that involve state changes. Several solutions are available to overcome this:

## Manual conversion

The most simple way is just taking the old record that arrives through `code_change` and by making several `erlang:setelement/2` calls, convert the old tuple to the new one. This is a bit of a hackish solution, but it works.

## Record conversion using exprecs

Ulf Wiger's excellent project [parse_trans](https://github.com/uwiger/parse_trans) contains a parse_transform called [exprecs](https://github.com/uwiger/parse_trans/blob/master/doc/exprecs.md) that generates methods to manipulate your exported records, one interesting method is `'#convert'` which (as the name implies) converts one record to another. It needs the following to work:

  * Requesting the parse_transform to process your `gen_server` file. (`-compile({parse_transform, exprecs}).`)
  * Declaring a record with the same definition as your previous version gen_server state, so assuming your gen_server's state record is called `state` you would need to declare a `'state__VSN'` record where VSN is the gen_server's version as specified through the `-vsn`directive (obtained through the `Module:module_info(attributes)` call).
  * Inform exprecs that we want to generate methods for the `state` record
  * Invoking the expecs convert method in `code_change` callback to migrate from the old version of the record to the new one (ie. `{NewState, _} = Module:'#convert-'(VSN, OldState)`).

Here is a simple example snippet with the relevant entries:

```
-module(m).
%% this is the current gen_server version, the one we want to upgrade from
%% is 1.0.0
-vsn("1.0.1").

%% ask exprecs to apply the parse transform to this file
-compile({parse_transform, exprecs}).

%% below is the previous state record definition with a new name containing
%% it's version, it only has one field
%% do note that in the 1.0.0 version the record is still named `state`, the
%% renaming to 'state__1.0.0' is just so that exprecs is able to distinguish
%% between versions
-record('state__1.0.0', {
    id = 0 :: non_neg_integer(),
  }).

%% this is our new state record definition
%% as you can see, we've added a new name field to it
-record(state, {
    id = 0 :: non_neg_integer(),
    name = <<>> :: binary()
  }).

%% ask the exprecs parse transform to generate it's methods
%% for our state record on compilation
-export_records([state]).

%% pattern match on a requested code_change from module version 1.0.0 to 1.0.1
code_change("1.0.0", OldState, Extra) ->
    lager:debug("code change from 1.0.0 version has been requested"),
    lager:debug("   extra: ~p", [Extra]),
    {NewState, _} = m:'#convert-'("1.0.0", OldState),
    {ok, NewState};
```

## Plugin code injection

This plugin offers the choice of injecting the necessary code in `gen_server:code_change/3` that takes care of record migration from the old version to the new. The plugin will act when the new version tarball is being generated (hence the `tar` provider hook in `rebar.config`), it will:

  * Scan the `.appup` file for `gen_server`'s' that need upgrade
  * Check for the existence of the `state_record` directive. This informs the plugin of two things: the user wants it to perform the code injection and the name of the record that represents the state. An example can be found [here](https://github.com/lrascao/relapp1/commit/0489c78af736f462f388e03953f84277bfa984ed#diff-319563e2e8eca2f29eab68597c7feba8R13).
  * Extract `gen_server` information from the source and destination releases
      - Abstract code
      - State record definition abstract code
      - State record fields structure
  * It will then inject into the `gen_server`'s destination beam file:
      - The old state record abstract code
      - A method that is in charge of performing the conversion of the state record between the two versions
      - An invocation of the above method at the beginning of `gen_server:code_change/3`. This call will be the very first instruction of `code_change`, so from the user's point of view the state record is already converted when the code change method is called, the old state is kept in the `Extra` variable as the tuple `{old_state, OldState}`.

#### Conversion method

The Erlang code that performs the conversion can be found in the `priv/` folder, based on a proplist of named record fields for the old and new versions it uses `erlang:setelement/3` and `erlang:element/2` to populate a new version record, new record fields are filled out with record defaults, update fields are copied, renaming fields is not supported.

#### gen_server:code_change hook injection

In the `priv/` folder you can also see the call that is injected into the beginning of the `code_change` method, it simply is a call to the conversion method making sure that the original variable names are kept and setting them to their new values.
