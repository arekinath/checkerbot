-module(db).
-behaviour(gen_server).
-export([start_link/0, get_list/0, saw_server/3, fail_server/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({global, dbserver}, ?MODULE, [], []).

get_list() ->
    gen_server:call({global, dbserver}, get_list).

saw_server(Port, Players, Hello) ->
    gen_server:cast({global, dbserver}, {saw_server, Port, erlang:localtime(), Players, Hello}).

fail_server(Port, Reason) ->
    gen_server:cast({global, dbserver}, {fail_server, Port, Reason}).

%% gen_server callbacks

-record(server, {lastseen, players, hello, failreason=none, failcount=0}).

init(_Args) ->
    {ok, dict:new()}.

handle_call(get_list, _From, State) ->
    {reply, dict:to_list(State), State}.

handle_cast({saw_server, Port, When, Players, Hello}, State) ->
    NewState = dict:store(Port, #server{lastseen=When, players=Players, hello=Hello}, State),
    {noreply, NewState};

handle_cast({fail_server, Port, Reason}, State) ->
    case dict:find(Port, State) of
        {ok, Server} ->
            #server{failcount = FC} = Server,
            if FC > 5 ->
                NewState = dict:erase(Port, State);
            true ->
                NewState = dict:store(Port, Server#server{failcount = FC + 1, failreason = Reason}, State)
            end,
            {noreply, NewState};
        _Other ->
            When = erlang:localtime(),
            NewState = dict:store(Port, #server{lastseen=When, players=[], hello= <<"">>, failcount = 1, failreason = Reason}, State),
            {noreply, NewState}
    end.

handle_info(Msg, State) ->
    io:format("db server got unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
