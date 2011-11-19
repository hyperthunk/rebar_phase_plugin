# Example: integration-test

This example configures the phase plugin to understand a new `integration-test`
phase, which runs `ct` and any other integration test commands (such as `retest`,
`load-test` for running the `rebar_tsung_plugin` and so on). This new *phase* is
dependent on a `test` phase, which runs `eunit qc` and is in turn, dependent on
the rebar `compile` command having run.

## Running the example

    $ rebar get-deps compile
    $ rebar skip_deps=true integration-test

