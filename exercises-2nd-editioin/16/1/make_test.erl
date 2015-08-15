-module(make_test).

-include_lib("eunit/include/eunit.hrl").
-import(make, [
               need_recompile/1
              ]).

need_recompile1_test() ->
    os:cmd("touch fib.erl"),
    os:cmd("touch fib.beam"),
    Result = need_recompile(fib),
    ?assert(Result =:= false).

need_recompile2_test() ->
    os:cmd("touch fib.beam"),
    timer:sleep(1000),
    os:cmd("touch fib.erl"),
    Result = need_recompile(fib),
    ?assert(Result =:= true).
