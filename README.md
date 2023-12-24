# squasher

Automatic type annotator tool based on dynamic tracking for Erlang programs.

# Gather live data

## Using the Erlang/OTP tracer

Compile collect.erl
```bash
erlc -I erlang/include/ erlang/src/collect.erl
```
Make the .beam file available on the same node your project is running on. To start collecting data:
```erlang
collect:start_erlang_trace(name_of_your_module).
```
After you have run the testcases/run your program, use the following function to dump the data to file:
```erlang
collect:stop_erlang_trace("<a_file_name>").
```

## Using parse_tranform

Add collect.erl and squasher_trans.erl to your project's erlang files, squasher.hrl to the include files.

Ad the following lines to the module you want to trace:
```erlang
-compile({parse_transform, squasher_trans}).
%%% If you want alias naming based on function parameters:
-save_to("<some_file_name>").
```

If you are using rebar3 and EUnit, add this to your rebar.config:
```erlang
{eunit_tests, [{setup, fun collect:prepare_trace/0, fun collect:save_traces_/1, {dir, "<your_project_dir>/_build/test/lib/<your_project_name>//test"}}]}.
```
When rebar3 eunit is called, the data will be collected and dumped into a default.bin file.

With Common Test or some other strategy, the following functions should be used:
```erlang
%%% Call before the tests are run:
collect:prepare_trace().
%%% Call after they have finished:
collect:save_traces().
```
This produces a default.bin file. save_traces/1 accepts a filepath if you want to specify the output file instead.

# Generate annotations

There are many options available to the annotator tool, to see them call:
```bash
stack run squasher-exe -- -h
```

With --out you can specify the name of the erlang file which will be generated. The prettyprinted aliases and specs will be written here. Last positional argument is the path to the collected dumps.
