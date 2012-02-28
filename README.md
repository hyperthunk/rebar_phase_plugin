# Support for *build phases* in rebar

This plugin provides a mechanism for introducing the concept of a build 
lifecycle to rebar projects, allowing for a clearly defined process where
phases can depend on one another.

Using the plugin, you can set up your build lifecycle once, in your global 
rebar config (i.e., `$HOME/.rebar/config`), distribute custom lifecycle config
on a per-project basis, or have your phases loaded from a specific dependency.
The latter case allows you to distribute your lifecycle configuration as a
separate library (containing configuration only!) and users can install this
into `$ERL_LIBS` (recommended) or `code:lib_dir` (not so much), or you can have
it pulled down via *deps* in all your projects and reference it that way.

## Simple example

```erlang
{phases, [
    % phase              depends-on       executes
    {build,              [],              ['get-deps', filter, compile]},
    {test,               build,           [eunit, qc]},
    {package,            build,           [generate, dist]},
    {'integration-test', [package, test], [ct, tsung]}
]}.
```

## More examples

See the `examples` folder for use-cases.

## License

BSD-like, do pretty much what you like license.
