# FMK Application [![Build Status](https://travis-ci.org/goncalotomas/FMKe.svg?branch=master)](https://travis-ci.org/goncalotomas/FMKe)

FMK-- is a prototype application that serves as a use case for the [Antidote key-value store][3], which was built for the [SyncFree project][2].

## Dependencies

FMK-- uses rebar3 to build releases and tests, and a local installation is provided in order to make the application start straight away. It also assumes that there is available access to a local Antidote node.

## Building FMK

The rebar3 script included in the root directory of the project contains all required information to build FMK. To build a release, run the following command:

```make all```

If the build is successful, you will be able to run FMK by running:

```make console```

This will open an erlang VM and from this point on you can use any function in the FMK API.

## Running the Antidote benchmark

To generate graphs for the benchmark [R](https://www.r-project.org/) must be installed (e.g. `sudo apt-get install r-base` on Ubuntu).

### Using runBenchLocal.sh

The script `runBenchLocal.sh` can be used to run the benchmark locally.
Antidote can either be pulled from Github, run as a docker image, run from a local folder, or an already started Antidote can be used.
Choose one of the following commands:

    # Use already running Antidote (default)
    ./runBenchLocal.sh

    # Use a local folder with Antidote
    ./runBenchLocal.sh /path/to/antidote/

    # Use an Antidote docker image
    ./runBenchLocal.sh docker

    # Clone Antidote from GitHub, compile and run it:
    ./runBenchLocal github


### Manually

1. Start Antidote.

2. Compile FMK.

        make all

3. Start FMK.

        make console

4. *(must be executed once)* Fill the database with testdata used by the benchmark.

        ./scripts/fmk_setup_script.erl

5. *(optional)* Adjust Benchmark parameters in `test/fmkclient.config`

6. Run the benchmark.

        make bench

7. The results can be found in the `tests` folder.







[1]: https://www.rebar3.org/docs/getting-started
[2]: https://syncfree.lip6.fr/
[3]: https://github.com/SyncFree/antidote
