-module(scanner).
-export([start_link/0, loop/0]).

start_link() ->
    {ok, spawn_link(?MODULE, loop, [])}.

loop() ->
    receive
    after 10000 ->
        done
    end,

    ExistingPorts = lists:map(fun({Port, _Info}) -> Port end, db:get_list()),
    AllPorts = netstat:candidates(),
    NotExistingPorts = AllPorts -- ExistingPorts,

    io:format("Scan round: exist = ~p, non = ~p~n", [ExistingPorts, NotExistingPorts]),

    lists:foreach(fun(Port) -> checker_pool:add_job(Port) end, ExistingPorts),
    lists:foreach(fun(Port) -> checker_pool:add_job(Port) end, NotExistingPorts),

    loop().
