#!/usr/bin/env escript

main([File|T]) ->
    case lists:member(File, filelib:wildcard(File)) of
        false ->
            io:format("~p : No such file or direcory~n", [File]);
        true ->
            {ok, B} = file:read_file(File),
            Digest = erlang:md5(B),
            L = binary_to_list(Digest),
            L1 = [erlang:integer_to_list(X, 16)||X <- L],
            L2 = lists:append(L1),
            io:format("~p ~p~n", [File, L2])
    end,
    main(T);
main([]) ->
    ok.
