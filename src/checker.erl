-module(checker).
-behaviour(gen_server).
-export([give_job/2, start_link/0, connect/1, check_hello/1, check_players/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

give_job(Pid, Port) ->
    gen_server:cast(Pid, {job, Port}).

%% gen_server callbacks

init(_Args) ->
    checker_pool:checker_ready(),
    {ok, idle}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({job, Port}, State) ->

    case check_players(Port) of
        {error, Term} ->
            db:fail_server(Port, Term);
        {ok, Players} ->
            case check_hello(Port) of
                {error, Term} ->
                    db:fail_server(Port, Term);
                {ok, HelloLine} ->
                    db:saw_server(Port, Players, HelloLine)
            end
    end,

    checker_pool:checker_ready(),
    {noreply, State}.

handle_info(Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(Port) ->
    gen_tcp:connect("localhost", Port,
    [   binary,
        {active, true},
        {packet, line},
        {packet_size, 32000},
        {recbuf, 32000}
    ]).

check_players(Port) ->
    Result = connect(Port),
    case Result of
        {error, Reason} ->
            {error, Reason};
        {ok, Socket} ->
            gen_tcp:send(Socket, <<"scores\n">>),
            case receive_players(Socket) of
                {error, Term} ->
                    {error, Term};
                {ok, Players} ->
                    {ok, Players}
            end
    end.

receive_players(Socket) ->
    receive_players(Socket, []).
receive_players(Socket, Players) ->
    receive
        {tcp, Socket, Data} ->
            case binrev(Data) of
                <<"\n", LineRev/binary>> ->
                    Line = binrev(LineRev),
                    case binary:split(Line, <<" ">>) of
                        [<<"checkerbot">> | _Rest] ->
                            receive_players(Socket, Players);
                        [Name | _Rest] ->
                            receive_players(Socket, [Name | Players]);
                        _Other ->
                            gen_tcp:close(Socket),
                            {error, badline}
                    end;
                _Other ->
                    gen_tcp:close(Socket),
                    {error, badline}
            end;
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            {ok, Players}
    after 5000 ->
        gen_tcp:close(Socket),
        {error, timeout}
    end.

check_hello(Port) ->
    case connect(Port) of
        {error, Reason} ->
            {error, Reason};
        {ok, Socket} ->
            gen_tcp:send(Socket, <<"checkerbot\n">>),
            case receive_hello(Socket) of
                {error, Term} ->
                    {error, Term};
                {ok, HelloLine} ->
                    {ok, HelloLine}
            end
    end.

receive_hello(Socket) ->
    receive
        {tcp, Socket, Data} ->
            case binrev(Data) of
                <<"\n", LineRev/binary>> ->
                    Line = binrev(LineRev),
                    gen_tcp:close(Socket),
                    case Line of
                        <<"Hello ", _Rest/binary>> ->
                            {ok, Line};
                        <<"hello ", _Rest/binary>> ->
                            {ok, Line};
                        _Other ->
                            {error, badline}
                    end;
                _Other ->
                    gen_tcp:close(Socket),
                    {error, badline}
            end;
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            {error, closed}
    after 5000 ->
        gen_tcp:close(Socket),
        {error, timeout}
    end.

%% @doc Reverses a binary
binrev(B) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(B))).
