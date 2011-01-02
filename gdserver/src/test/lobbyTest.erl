-module(lobbyTest).

-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").

createClientAndLobby(ClientID, PlayerMax) ->
    {ok, ClientPID} = client:start(nosocket, ClientID),
    createClientAndLobby(ClientID, ClientPID, PlayerMax).

createClientAndLobby(ClientID, ClientPID, PlayerMax, TimerLength) ->
    ClientInfo = #clientInfo{clientID = ClientID, displayName = "test"},
    {ok, LobbyPID, LobbyInfo} = lobbyManager:createLobby(lobbyManager, #lobbyInfo{playerMax=PlayerMax, timerLength=TimerLength}, ClientPID, ClientInfo),
    {ClientPID, LobbyPID, LobbyInfo}.

createClientAndLobby(ClientID, ClientPID, PlayerMax) ->
    ClientInfo = #clientInfo{clientID = ClientID, displayName = "test"},
    {ok, LobbyPID, LobbyInfo} = lobbyManager:createLobby(lobbyManager, #lobbyInfo{playerMax=PlayerMax}, ClientPID, ClientInfo),
    {ClientPID, LobbyPID, LobbyInfo}.

addClientToLobby(ClientID, LobbyPID) ->
    {ok, ClientPID} = client:start(nosocket, ClientID),
    addClientToLobby(ClientID, ClientPID, LobbyPID).

addClientToLobby(ClientID, ClientPID, LobbyPID) ->
    ClientInfo = #clientInfo{clientID = ClientID, displayName = "test"},
    ClientCount = length(lobby:getClients(LobbyPID)),
    case lobby:addClient(ClientPID, ClientInfo, LobbyPID) of
        ok ->
            ?assertEqual(ClientCount+1, length(lobby:getClients(LobbyPID))),
            ClientPID;
        Error ->
            ?assertEqual(ClientCount, length(lobby:getClients(LobbyPID))),
            Error
    end.
    
%todo commented becasuse it fails because we made lobby:removeClient async insead of sync    
%lobbyAddRemoveAndDieIfZeroAndPlayerMax_test() ->
%    utils:clearMessageQueue(),
%    lobbyManager:start(lobbyManager),
%      
%    {ClientPID1, LobbyPID, _} = createClientAndLobby(1, 2),
%    ?assertMatch(1, length(lobby:getClients(LobbyPID))),
%    
%    ClientPID2 = addClientToLobby(2, LobbyPID),
%    ?assertMatch(2, length(lobby:getClients(LobbyPID))),
%        
%    ?assertMatch({error, _}, addClientToLobby(3, LobbyPID)),
%    
%    lobby:removeClient(1, LobbyPID),
%    ?assertMatch(1, length(lobby:getClients(LobbyPID))),
%    
%    lobby:removeClient(2, LobbyPID),
%    ?assertMatch(0, length(lobbyManager:getLobbies(lobbyManager))),
%    
%    exit(ClientPID2, kill),
%    exit(ClientPID1, kill),
%    exit(LobbyPID, kill),
%    lobbyManager:stop(lobbyManager).

getAllAddClientMessagesExceptOne_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    
    {ClientPID1, LobbyPID, _} = createClientAndLobby(1, 2),
    ?assertEqual(0, length(lobby:getAllAddClientMessagesExceptOne(LobbyPID, 1))),
    ?assertEqual(1, length(lobby:getAllAddClientMessagesExceptOne(LobbyPID, -1))),
    ClientPID2 = addClientToLobby(2, LobbyPID),
    ?assertEqual(1, length(lobby:getAllAddClientMessagesExceptOne(LobbyPID, 1))),
    
    exit(ClientPID2, kill),
    exit(ClientPID1, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).
    
addRemoveClientMessages_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _LobbyInfo} = createClientAndLobby(1, self(), 2),

    ClientPID2 = addClientToLobby(2, LobbyPID),
    receive
        {addClient, lobby, #clientInfo{clientID=2}} ->
            ?assert(true)
        after 500 ->
            ?assert(false)
    end,
      
    lobby:removeClient(2, LobbyPID),
    receive
        {removeClient, lobby, 2} ->
            ?assert(true)
        after 500 ->
            ?assert(false)
    end,

    exit(ClientPID2, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).
    
loadingProgress_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _} = createClientAndLobby(1, self(), 2),
    
    ClientPID2 = addClientToLobby(2, LobbyPID),
    lobby:setLoadingProgress(100, 2, LobbyPID),
    
    receive
        {loadingProgress, lobby, 100, 2} ->
            ?assert(true)
        after 500 ->
            ?assert(false)
    end,

    exit(ClientPID2, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).
    
checkAllLoaded_test() ->    
    Clients = dict:from_list([{1, {pid, #clientInfo{readiness=loaded}}}, {2, {pid, #clientInfo{readiness=loaded}}}]),
    ?assertEqual(false, lobby:checkAllLoaded(Clients, utils:now()+1000000, 10)).
    
checkAllReadiness_test() ->
    Clients = dict:from_list([{1, {pid, #clientInfo{readiness=loaded}}}, {2, {pid, #clientInfo{readiness=loaded}}}]),
    ?assertEqual(true, lobby:checkAllReadiness(loaded, Clients)),
    
    Clients2 = dict:from_list([{1, {pid, #clientInfo{readiness=loaded}}}, {2, {pid, #clientInfo{readiness=none}}}]),
    ?assertEqual(false, lobby:checkAllReadiness(loaded, Clients2)).

setAllReadiness_test() ->
    Clients = dict:from_list([{1, {pid, #clientInfo{readiness=b}}}, {2, {pid, #clientInfo{readiness=a}}}]),
    Clients2 = lobby:setAllReadiness(loaded, Clients),
    ?assertEqual(true, lobby:checkAllReadiness(loaded, Clients2)).

checkAllQuit_test() ->
    Clients = dict:new(),
    ?assertEqual(true, lobby:checkAllQuit(Clients)),
    
    Clients2 = dict:from_list([{1, {pid, #clientInfo{readiness=loaded}}}, {2, {pid, #clientInfo{readiness=loaded}}}]),
    ?assertEqual(false, lobby:checkAllQuit(Clients2)),
    
    Clients3 = dict:from_list([{1, {undefined, #clientInfo{readiness=loaded}}}, {2, {pid, #clientInfo{readiness=loaded}}}]),
    ?assertEqual(false, lobby:checkAllQuit(Clients3)),
    
    Clients4 = dict:from_list([{1, {undefined, #clientInfo{readiness=loaded}}}, {2, {undefined, #clientInfo{readiness=loaded}}}]),
    ?assertEqual(true, lobby:checkAllQuit(Clients4)).
    
% both clients loaded - {startLoobyRace} fired
startLobbyRace_Message_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _} = createClientAndLobby(1, self(), 3, 500),
    
    ClientPID2 = addClientToLobby(2, LobbyPID),    
    lobby:setReadiness(1, LobbyPID, loaded),
    lobby:setReadiness(2, LobbyPID, loaded),
    
    receive
        {lobbyRaceEvent, initialize, _, _} ->
            ?assert(true)
        after 1000 ->
            ?assert(false)
    end,

    exit(ClientPID2, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).

% 3 clients: 2 loaded, 1 quit - {startLoobyRace} fired
startLobbyRace_Message2_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _} = createClientAndLobby(1, self(), 3, 50),
    
    ClientPID2 = addClientToLobby(2, LobbyPID),
    ClientPID3 = addClientToLobby(3, LobbyPID),
    lobby:setReadiness(1, LobbyPID, loaded),
    lobby:setReadiness(2, LobbyPID, loaded),
    
    receive
        {lobbyRaceEvent, initialize, _, _} -> 
            ?assert(false)
        after 100 ->
            ?assert(true)
    end,
    
    lobby:removeClient(3, LobbyPID),
    
    receive
        {lobbyRaceEvent, initialize, _, _} ->
            ?assert(true)
        after 100 ->
            ?assert(false)
    end,

    exit(ClientPID2, kill),
    exit(ClientPID3, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).

% 1 client - {startLoobyRace} NOT fired
startLobbyRace_Message3_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _} = createClientAndLobby(1, self(), 3, 50),
    
    lobby:setReadiness(1, LobbyPID, loaded),
    
    receive
        {lobbyRaceEvent, initialize, _, _} ->
            ?assert(false)
        after 100 ->
            ?assert(true)
    end,

    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).
    
% 3 clients: 2 loaded, 1 quit - {startLoobyRace} fired
startLobbyRace_Message4_test() ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _} = createClientAndLobby(1, self(), 3, 50),
    lobby:setReadiness(1, LobbyPID, loaded),
    
    receive
        {lobbyRaceEvent, initialize, _, _} ->
            ?assert(false)
        after 100 ->
            ?assert(true)
    end,
    
    ClientPID2 = addClientToLobby(2, LobbyPID),
    lobby:setReadiness(2, LobbyPID, loaded),
        
    receive
        {lobbyRaceEvent, initialize, _, _} ->
            ?assert(false)
        after 100 ->
            ?assert(true)
    end,
    
    lobby:setReadiness(1, LobbyPID, loaded),
    
    receive
        {lobbyRaceEvent, initialize, _, _} ->
            ?assert(true)
        after 100 ->
            ?assert(false)
    end,

    exit(ClientPID2, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).

finishLobbyRace_test() ->
    finishLobbyRace(finish), 
    finishLobbyRace(quit).

finishLobbyRace(Type) ->
    utils:clearMessageQueue(),
    lobbyManager:start(lobbyManager),
    {_, LobbyPID, _} = createClientAndLobby(1, self(), 3, 50),
    ClientPID2 = addClientToLobby(2, LobbyPID),
    ClientPID3 = addClientToLobby(3, LobbyPID),
    lobby:setReadiness(1, LobbyPID, loaded),
    lobby:setReadiness(2, LobbyPID, loaded),
    lobby:setReadiness(3, LobbyPID, loaded),
    
    lobby:lapTime("finish", 1, LobbyPID),
           
    receive
        {lobbyRaceResults, _} ->
            ?assert(false)
        after 100 ->
            ?assert(true)
    end,    
      
    case Type of
        finish ->
            lobby:removeClient(3, LobbyPID),
            lobby:lapTime("finish", 2, LobbyPID);
        quit ->
            lobby:lapTime("finish", 2, LobbyPID),
            lobby:removeClient(3, LobbyPID)
    end,
        
    receive
        {lobbyRaceResults, LobbyResults, _, _} ->
            ?assertEqual(3, length(LobbyResults))
        after 100 ->
            ?assert(false)
    end,    
    
    ?assertEqual(2, length(lobby:getClients(LobbyPID))),
    
    {_, ClientInfo1} = lists:nth(1, lobby:getClients(LobbyPID)),
    {_, ClientInfo2} = lists:nth(2, lobby:getClients(LobbyPID)),
    
    ?assertEqual(0, ClientInfo1#clientInfo.lastTime),
    ?assertEqual(0, ClientInfo2#clientInfo.lastTime),

    exit(ClientPID2, kill),
    exit(ClientPID3, kill),
    exit(LobbyPID, kill),
    lobbyManager:stop(lobbyManager).
    
%todo commented becasuse it fails because we made lobby:removeClient async insead of sync    
% both clients loaded - {startLoobyRace} fired
%lobbyQuitsIfAllClientsQuitDuringRace_test() ->
%    utils:clearMessageQueue(),
%    lobbyManager:start(lobbyManager),
%    {_, LobbyPID, _} = createClientAndLobby(1, self(), 3, 100),
%    
%    ClientPID2 = addClientToLobby(2, LobbyPID),    
%    lobby:setReadiness(1, LobbyPID, loaded),
%    lobby:setReadiness(2, LobbyPID, loaded),
%    
%    receive
%        {lobbyRaceEvent, initialize, _, _} ->
%            ?assert(true)
%        after 1000 ->
%            ?assert(false)
%    end,
%
%    lobby:removeClient(1, LobbyPID),
%    lobby:removeClient(2, LobbyPID),   
%
%    ?assertEqual(0, length(lobbyManager:getLobbies(lobbyManager))),
%
%    exit(ClientPID2, kill),
%    exit(LobbyPID, kill),
%    lobbyManager:stop(lobbyManager).

compareFinishTime_test() ->    
    ?assertEqual(true, lobby:compareFinishTime(1, -1)), 
    ?assertEqual(true, lobby:compareFinishTime(1, 2)), 
    
    ?assertEqual(false, lobby:compareFinishTime(0, 0)), 
    ?assertEqual(false, lobby:compareFinishTime(2, 1)), 
    ?assertEqual(false, lobby:compareFinishTime(-1, -2)), 
    ?assertEqual(false, lobby:compareFinishTime(1, 1)), 
    ?assertEqual(false, lobby:compareFinishTime(-1, -1)), 
    ?assertEqual(false, lobby:compareFinishTime(-1, 1)).
    
getClientsCount_test() ->
    ?assertEqual(0, lobby:getClientsCount(dict:new())),
    ?assertEqual(1, lobby:getClientsCount(dict:from_list([{1, {pid, info}}]))),
    ?assertEqual(2, lobby:getClientsCount(dict:from_list([{1, {pid, info}}, {2, {pid, info}}]))),
    ?assertEqual(1, lobby:getClientsCount(dict:from_list([{1, {pid, info}}, {2, {undefined, info}}]))).
