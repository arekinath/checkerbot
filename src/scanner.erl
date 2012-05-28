-module(scanner).
-export([start_link/0, loop/0]).

start_link() ->
    {ok, spawn_link(?MODULE, loop, [])}.

scan_existing() ->
    ExistingPorts = lists:map(fun({Port, _Info}) -> Port end, db:get_list()),
    lists:foreach(fun(Port) -> checker_pool:add_job(Port) end, ExistingPorts).

scan_new() ->
    ExistingPorts = lists:map(fun({Port, _Info}) -> Port end, db:get_list()),
    AllPorts = netstat:candidates(),
    NotExistingPorts = AllPorts -- ExistingPorts,
    lists:foreach(fun(Port) -> checker_pool:add_job(Port) end, NotExistingPorts).

loop() ->
    receive
    after 10000 ->
        scan_new()
    end,
    receive
    after 10000 ->
        scan_existing()
    end,
    receive
    after 10000 ->
        scan_existing()
    end,
    loop().
