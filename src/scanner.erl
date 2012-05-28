-module(scanner).
-export([start_link/0, loop/0]).

start_link() ->
    {ok, spawn_link(?MODULE, loop, [])}.

scan_existing() ->
    ExistingPorts = lists:map(fun({Port, _Info}) -> Port end, db:get_list()),
    error_logger:info_report([{scan, existing}, {count, length(ExistingPorts)}]),
    lists:foreach(fun(Port) -> checker_pool:add_job(Port) end, ExistingPorts).

scan_new() ->
    ExistingPorts = lists:map(fun({Port, _Info}) -> Port end, db:get_list()),
    AllPorts = netstat:candidates(),
    NotExistingPorts = AllPorts -- ExistingPorts,
    error_logger:info_report([{scan, new}, {count, length(NotExistingPorts)}]),
    lists:foreach(fun(Port) -> checker_pool:add_job(Port) end, NotExistingPorts).

wait(Secs) ->
    receive
    after round(Secs) * 1000 ->
        done
    end.

loop() ->
    Flong = dist_var:new(30, 10),
    Fshort = dist_var:new(15, 5),
    wait(5),
    scan_new(), wait(Flong()),
    scan_existing(), wait(Flong()),
    scan_existing(), wait(Flong()),
    scan_existing(), wait(Fshort()),
    loop().
