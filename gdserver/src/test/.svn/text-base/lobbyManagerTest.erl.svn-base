-module(lobbyManagerTest).

-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

createAndRemoveLobbies_test() ->
    utils:clearMessageQueue(),
    mnesia:clear_table(users),
    allTest:createTestUsers(),

    lobbyManager:start(lobbyManager),
    {ok, ClientPID1} = client:start(nosocket, 1),
    {ok, ClientPID2} = client:start(nosocket, 2),
    ?assertMatch(0, length(lobbyManager:getLobbies(lobbyManager))),
    ?assertMatch({ok, _, #lobbyInfo{}}, lobbyManager:createLobby(lobbyManager, #lobbyInfo{routeID=1, lapNumber=1}, ClientPID1, #clientInfo{clientID=1, userID=test})),
    ?assertMatch(1, length(lobbyManager:getLobbies(lobbyManager))),    
    
    lobbyManager:createLobby(lobbyManager, #lobbyInfo{routeID=1, lapNumber=1}, ClientPID2, #clientInfo{clientID=1, userID=test}),
    ?assertMatch(2, length(lobbyManager:getLobbies(lobbyManager))),
    
    [{_, Info1} | Rest] = lobbyManager:getLobbies(lobbyManager),
    [{_, Info2} | _EmptyRest] = Rest,
    lobbyManager:removeLobby(lobbyManager, Info1#lobbyInfo.id),
    lobbyManager:removeLobby(lobbyManager, Info2#lobbyInfo.id),
    ?assertMatch(0, length(lobbyManager:getLobbies(lobbyManager))),
    
    lobbyManager:removeLobby(lobbyManager, Info2#lobbyInfo.id),
    
    exit(ClientPID2, kill),
    exit(ClientPID1, kill),
    
    mnesia:clear_table(users),
    lobbyManager:stop(lobbyManager).
    
getLobbyByID_test() ->
    utils:clearMessageQueue(),
    mnesia:clear_table(users),
    allTest:createTestUsers(),

    lobbyManager:start(lobbyManager),
    
    {ok, ClientPID1} = client:start(nosocket, 1),
    ?assertMatch(0, length(lobbyManager:getLobbies(lobbyManager))),
    
    lobbyManager:createLobby(lobbyManager, #lobbyInfo{routeID=1, lapNumber=1}, ClientPID1, #clientInfo{clientID=1, userID=test}),
    ?assertMatch(1, length(lobbyManager:getLobbies(lobbyManager))),
    
    [{LobbyPID, LobbyInfo}] = lobbyManager:getLobbies(lobbyManager),
    ?assertMatch({ok, {LobbyPID, LobbyInfo}}, lobbyManager:getLobbyByID(lobbyManager, LobbyInfo#lobbyInfo.id)),   
    
    exit(ClientPID1, kill),
    exit(LobbyPID, kill),
    
    mnesia:clear_table(users),
    lobbyManager:stop(lobbyManager).

getLobbyIDByPID_test() ->
    ?assertEqual(undefined, lobbyManager:getLobbyIDByPID(pid0, dict:new())),
    ?assertEqual(undefined, lobbyManager:getLobbyIDByPID(pid0, dict:from_list([{1, {pid1, info}}, {2, {pid2, info}}]))),
    ?assertEqual(2, lobbyManager:getLobbyIDByPID(pid2, dict:from_list([{1, {pid1, info}}, {2, {pid2, info}}]))).
    
removeFromAllLobbies_test() ->
    utils:clearMessageQueue(),
    mnesia:clear_table(users),
    allTest:createTestUsers(),
    
    lobbyManager:start(lobbyManager),
    
    {ok, ClientPID1} = client:start(nosocket, 1),
    {ok, ClientPID2} = client:start(nosocket, 2),
    {ok, LobbyPID, _} = lobbyManager:createLobby(lobbyManager, #lobbyInfo{routeID=1,lapNumber=1}, ClientPID1, #clientInfo{clientID=1, userID=test}),    
    lobby:addClient(ClientPID2, #clientInfo{userID=test}, LobbyPID),
    ?assertEqual(2, length(lobby:getClients(LobbyPID))),
    lobbyManager:removeFromAllLobbies(lobbyManager, 1),
    ?assertEqual(1, length(lobby:getClients(LobbyPID))),
    
    exit(ClientPID1, kill),
    exit(ClientPID2, kill),
    exit(LobbyPID, kill),
    
    mnesia:clear_table(users),
    lobbyManager:stop(lobbyManager).
