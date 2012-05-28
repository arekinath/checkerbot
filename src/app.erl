-module(app).
-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
    {ok, _Pid} = supervisor:start_link(?MODULE, []),
    spin().

spin() ->
    receive
        done ->
            done
    end,
    spin().

make_checkers(N) -> make_checkers(N, []).
make_checkers(0, Checkers) -> Checkers;
make_checkers(N, Checkers) ->
    Checker = {N, {checker, start_link, []},
               permanent, 5000, worker, [checker]},
    make_checkers(N - 1, [Checker | Checkers]).

init(_Args) ->
    Strategy = {one_for_one, 5, 30},
    DB = {dbserver,
          {db, start_link, []},
          permanent, 5000, worker, [db]},
    CheckerPool = {checkerpool,
                   {checker_pool, start_link, []},
                   permanent, 5000, worker, [checker_pool]},
    Checkers = make_checkers(10),
    Scanner = {scanner,
               {scanner, start_link, []},
               permanent, 5000, worker, [scanner]},
    Listener = {listener,
                {listener, start_link, [2309]},
                permanent, 5000, worker, [listener]},
    {ok, {Strategy, [DB, CheckerPool] ++ Checkers ++ [Scanner, Listener]}}.
    %{ok, {Strategy, [DB]}}.
