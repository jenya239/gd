-module(quickTest).

-compile(export_all).

start() ->
    start("localhost", 9000).

start(Host, Port) ->    
    run(Host, Port, 0).

run(Host, Port, ClientID) ->
    % connect    
    Socket = futil:connect(Host, Port),
    log:write(info, "<~3..0b>{    quickTest connected to ~s:~w~n", [ClientID, Host, Port]), 
    
    "<event name='serverTime'" ++ _ = futil:receiveXml(Socket, ClientID, undefined),
    
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0'/>", ClientID, undefined),
    "<event name='authorize' result='ok'>" ++ _ = futil:receiveXml(Socket, ClientID, undefined),
    
    %testGetLobbies(Socket, ClientID),
    testCreateLobby(Socket, ClientID),
    ok.
    
testGetLobbies(Socket, ClientID) ->
    futil:sendXml(Socket, "<event name='get' property='lobbies' />", ClientID, undefined),
    "<event name='get' property='lobbies' result='ok'>" ++ _= futil:receiveXml(Socket, ClientID, undefined).
    
testCreateLobby(Socket, ClientID) ->
    futil:sendXml(Socket, "<event name='createLobby' routeID='1' lapNumber='3' direction='forward' playerMax='20' allowedCarID='-1' />", ClientID, undefined),
    "<event name='createLobby' result='ok'>" ++ _= futil:receiveXml(Socket, ClientID, undefined).
    
