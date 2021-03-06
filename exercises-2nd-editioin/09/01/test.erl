%%1. Write some very small modules that export a single function. Write type specifications for the exported functions. In the functions make some type errors; then run the dialyzer on these programs and try to understand the error messages. Sometimes you’ll make an error but the dialyzer will not find the error; stare hard at the program to try to work out why you did not get the error you expected.
%%
-module(test).

-export([start/1]).

-spec start(L) -> R when
    L :: list(),
    R :: list().
start(L) ->
    erlang:md5(L).

