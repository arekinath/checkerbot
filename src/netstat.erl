-module(netstat).
-export([candidates/0]).

candidates() ->
    Cmd = "netstat.sh",
    run(Cmd, 5000).

run(Cmd, Timeout) ->
    Port = erlang:open_port({spawn, Cmd},[exit_status, binary, {line, 1024}]),
    loop(Port, [], Timeout).

loop(Port, Data, Timeout) ->
    receive
        {Port, {data, {_Flag, Line}}} ->
            Num = case (catch list_to_integer(binary:bin_to_list(Line))) of
                {'EXIT', _} -> 0;
                N -> N
            end,
            if (Num > 1024) and (Num < 40000) ->
                loop(Port, [Num | Data], Timeout);
            true ->
                loop(Port, Data, Timeout)
            end;
        {Port, {exit_status, 0}} -> Data;
        {Port, {exit_status, S}} -> throw({commandfailed, S})
    after Timeout ->
        throw(timeout)
    end.
