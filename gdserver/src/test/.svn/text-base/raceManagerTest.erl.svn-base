-module(raceManagerTest).
-include("lib/eunit/include/eunit.hrl").
-include("config.hrl").

getRace_test () ->
    
    _RaceManagerPID = raceManager:start_link(),
    GameType = dbGame:getGameType(id, 2),
    log:write(trace, ?MODULE_STRING, "begore raceManager:getRace(...)~n", []),
    {racePid, PID1} = raceManager:getRace(gameType, GameType),
    for(?MAX_ON_RACE-2, 
        fun() -> 
            raceManager:getRace(gameType, GameType)
        end),
    {racePid, PIDMax} = raceManager:getRace(gameType, GameType),
    {racePid, PIDNew} = raceManager:getRace(gameType, GameType),
    
    ?assert(PIDMax =:= PID1),
    ?assert(PIDMax =/= PIDNew).   
    
for(0, _F) ->
    ok;

for(N, F) ->
    F(),
    for(N - 1, F).    
    
    
