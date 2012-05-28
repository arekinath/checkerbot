-module(listener).
-export([start_link/1, accept_loop/2, send_table/1]).

start_link(Port) ->
    case gen_tcp:listen(Port, [{packet_size, 32000}, {recbuf, 32000}, {active, true}, {packet, line}, binary, {reuseaddr, true}]) of
        {error, Term} ->
            {error, Term};
        {ok, Socket} ->
            {ok, spawn_link(?MODULE, accept_loop, [Socket, Port])}
    end.

accept_loop(ListenSocket, Port) ->
    error_logger:info_report([{event, listener_started}, {port, Port}]),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {error, _} ->
            accept_loop(ListenSocket);
        {ok, AcceptSocket} ->
            spawn(?MODULE, send_table, [AcceptSocket]),
            accept_loop(ListenSocket)
    end.

send_table(Socket) ->
    Heads = [port, status, last_up, players, hello],
    HeadFormat = "~-6w  ~-10w  ~-25w  ~-30w  ~-30w~n",
    OkFormat =   "~-6w  ~-10w  ~-25w  ~-30s  ~-30s~n",
    BadFormat =  "~-6w  ~-10w  ~-25w (~w)~n",
    gen_tcp:send(Socket, io_lib:format(HeadFormat, Heads)),
    gen_tcp:send(Socket, bin_concat(lists:duplicate(101, <<"-">>))),
    gen_tcp:send(Socket, <<"\n">>),

    Ports = db:get_list(),
    lists:foreach(fun({Port, {server, LastSeen, Players, Hello, FailReason, FailCount}}) ->
        if FailCount > 1 ->
            gen_tcp:send(Socket, io_lib:format(BadFormat, [Port, down, LastSeen, FailReason]));
        true ->
            gen_tcp:send(Socket, io_lib:format(OkFormat, [Port, up, LastSeen, bin_concat(Players, <<", ">>), Hello]))
        end
    end, Ports),

    gen_tcp:close(Socket).

%% @doc Concatenates a list of binaries together
bin_concat(List) ->
    bin_concat(List, <<"">>).

%% @doc Concatenates a list of binaries together separated by Sep
bin_concat([], _Sep) ->
    <<"">>;
bin_concat(List, Sep) ->
    Fun = fun(Bin, Acc) ->
        if Acc =:= first ->
            <<Bin/binary>>;
        true ->
            <<Acc/binary, Sep/binary, Bin/binary>>
        end
    end,
    lists:foldl(Fun, first, List).
