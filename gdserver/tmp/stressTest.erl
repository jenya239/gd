%% Author: demid
%% Created: 01.10.2008
%% Description: Запускает несколько одновременно выполняющихся сценариев

-module(stressTest).
-export([start/0, start/2, start/4]).

-define(TCP_OPTIONS, [list, {active, false}, {reuseaddr, true}, {packet, 4}]).

start()->
%    start("turbostool.game-host.org", 9000, 10).
    start("localhost", 9000).

start(Host, Port) ->
    start(Host, Port, 4, 2).

start(Host, Port, ProcessCount, IterationsCount)->
    run(Host, Port, 1, ProcessCount, IterationsCount).

run(_Host, _Port, _ClientID, 0, _IterationsCount)->
    ok;
    
run(Host, Port, ClientID, ProcessCount, IterationsCount) ->
    spawn(fun() -> 
            racingTest:run(Host, Port, ClientID, IterationsCount) 
        end),    
    timer:sleep(10),
    run(Host, Port, ClientID + 1, ProcessCount - 1, IterationsCount).
