# FMK Application

This is the repository for the FMK-- Application Prototype from the [SyncFree Consortium][2].  
To adhere to modern standarts of Erlang build tools, I have decided to use rebar3 and will provide small support in using rebar3 in this file.

## Build Tool

The FMK-- prototype application uses rebar3 as its official build tool. For anything other than the instructions listed below, I will refer you to the [rebar3 official documentation][1] for installation support.

### Quick instalation guide

If you are on MacOS, you can use brew to quickly install rebar3:
```
	brew install rebar3
```

### Building the project

Building a project using rebar3 is as easy as changing directory to the project root and executing the following command:
```
	rebar3 compile
```

### Running unit tests

Rebar3 uses EUnit to perform unit testing. There are multiple tests already on the repository. To check that everything is ok, run the following command:
```
	rebar3 eunit
```

**Important note:** FMK needs a running Antidote instance on localhost for the unit tests to succeed. You can start an Antidote instance by cloning the [Antidote repository][3] and executing the following command in the repository root:

```
  rel/antidote/bin/antidote start
  rel/antidote/bin/antidote attach
```

Once you see a large amount of log messages, it means Antidote has finished its setup and is ready to take requests.

[1]: https://www.rebar3.org/docs/getting-started
[2]: https://syncfree.lip6.fr/
[3]: https://github.com/SyncFree/antidote