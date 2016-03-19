# FMK Application

This is the repository for the FMK Application from the [SyncFree Consortium][2].  
To adere to modern standarts of Erlang build tools, I have decided to use rebar3 and will provide small support in using rebar3 in this file.

## Rebar3

[Antidote][3] used rebar internally as the official build tool, but newer projects are shipping with rebar3 because it is the follow up version which attends to many of rebar2's shortcomings. As such, I will refer you to the [rebar3 official documentation][1] for installation support.

### Quick instalation guide

If you are on MacOS, you can use brew to quickly install rebar3:
```
	brew install --devel homebrew/devel-only/rebar3
```
Notice that rebar3, as of the time of writing this document, does not have a stable release, hence using a development version in the command above.

### Building the project

Building a project using rebar3 is as easy as executing the following command:
```
	rebar3 compile
```


[1]: https://www.rebar3.org/docs/getting-started
[2]: https://syncfree.lip6.fr/
[3]: https://github.com/SyncFree/antidote