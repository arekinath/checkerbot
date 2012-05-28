-module(checker_pool).
-behaviour(gen_server).
-export([start_link/0, checker_ready/0, add_job/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({global, cpserver}, ?MODULE, [], []).

checker_ready() ->
    gen_server:cast({global, cpserver}, {checker_ready, self()}).

add_job(Port) ->
    gen_server:cast({global, cpserver}, {add_job, Port}).

%% gen_server callbacks

-record(state, {idles=[], jobs=[]}).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({checker_ready, Pid}, State) ->
    #state{jobs = Jobs, idles = Idles} = State,
    if length(Jobs) > 0 ->
        [Job | Rest] = Jobs,
        checker:give_job(Pid, Job),
        {noreply, State#state{jobs = Rest}};
    true ->
        {noreply, State#state{idles = [Pid | Idles]}}
    end;

handle_cast({add_job, Port}, State) ->
    #state{jobs = Jobs, idles = Idles} = State,
    if length(Idles) > 0 ->
        [Checker | Rest] = Idles,
        checker:give_job(Checker, Port),
        {noreply, State#state{idles = Rest}};
    true ->
        {noreply, State#state{jobs = [Port | Jobs]}}
    end.

handle_info(Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
