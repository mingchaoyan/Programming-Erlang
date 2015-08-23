-module(socket_examples).
-export([nano_get_url/0]).

nano_get_url() ->
    ResultBin = nano_get_url("www.google.com.hk"),
    ResultList = string:tokens(ResultBin, "\r\n"), 
    StatusLine = string:tokens(hd(ResultList), " "),
    io:format("StatusLine:~p~n", [StatusLine]),
    case lists:member("302", StatusLine) of
        true ->
            LocationLine = lists:nth(2, ResultList),
            io:format("LocationLine:~p~n", [LocationLine]),
            ["Location: http:", Location | _] = string:tokens(LocationLine, "/"),
            io:format("Location:~p~n", [Location]),
            nano_get_url(Location);
        false ->
            io:format("ResultList:~p~n",[ResultList])
    end.

nano_get_url(Host) ->
    {ok,Socket} = gen_tcp:connect(Host,80,[binary, {packet, 0}]), %% (1)
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),  %% (2)
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp,Socket,Bin} ->    %% (3)
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed,Socket} -> %% (4)
            binary_to_list(list_to_binary(lists:reverse(SoFar))) %% (5)
    end.
