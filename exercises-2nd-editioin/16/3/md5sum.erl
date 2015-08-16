#!/usr/bin/env escript
%% 3. Repeat the previous exercise for a large file (say a few hundred megabytes). This time read the file in small chunks, using erlang:md5_init, erlang:md5_update, and erlang:md5_final to compute the MD5 sum of the file.
-define(BIG_SIZE, 100*1024*1024).
-define(STEP, 10*1024*1024).
main([File|T]) ->
    case lists:member(File, filelib:wildcard(File)) of
        false ->
            io:format("~p : No such file or direcory~n", [File]);
        true ->
            case filelib:file_size(File)  of
                Size when Size > ?BIG_SIZE ->
                    main_big(File, Size);
                _ ->
                    main_small(File)
            end
    end,
    main(T);
main([]) ->
    ok.

main_big(File, Size) ->
    statistics(runtime),
    statistics(wall_clock),
    {ok, S} = file:open(File, [read, binary, raw]),
    Context = do_md5(S, Size, 0, erlang:md5_init()),
    Digest = erlang:md5_final(Context),
    Md5 = digest2hex(Digest),
    {_, U1} = statistics(runtime),
    {_, U2} = statistics(wall_clock),
    io:format("~p ~p ~pms[~pms]~n", [File, Md5, U1, U2]).

main_small(File) ->
    statistics(runtime),
    statistics(wall_clock),
    {ok, B} = file:read_file(File),
    Digest = erlang:md5(B),
    Md5 = digest2hex(Digest),
    {_, U1} = statistics(runtime),
    {_, U2} = statistics(wall_clock),
    io:format("~p ~p ~pms(~pms)~n", [File, Md5, U1, U2]).

do_md5(S, Size, Start, Context) when Start + ?STEP =< Size ->
    {ok, B} = file:pread(S, Start, ?STEP),
    NewContext = erlang:md5_update(Context, B),
    do_md5(S, Size, Start+?STEP, NewContext);
do_md5(S, Size, Start, Context) ->
    {ok, B} = file:pread(S, Start, Size - Start + 1),
    erlang:md5_update(Context, B).

digest2hex(Digest) ->
    L = binary_to_list(Digest),
    L1 = [erlang:integer_to_list(X, 16)||X <- L],
    lists:append(L1).




