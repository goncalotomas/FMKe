# FMK Application

FMK-- is a prototype application that serves as a use case for the [Antidote key-value store][3], which was built for the [SyncFree project][2].

## Dependencies

FMK-- uses rebar3 to build releases and tests, and a local installation is provided in order to make the application start straight away. It also assumes that there is available access to a local Antidote node.

## Building FMK

The rebar3 script included in the root directory of the project contains all required information to build FMK. To build a release, run the following command:

```$ rebar3 release```

If the build is successful, you will be able to run FMK by running:

```_build/default/rel/fmk/bin/fmk console```

This will open an erlang VM and from this point on you can use any function in the FMK API.

[1]: https://www.rebar3.org/docs/getting-started
[2]: https://syncfree.lip6.fr/
[3]: https://github.com/SyncFree/antidote