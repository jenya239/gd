%% Author: snaky
%% Created: 20.05.2009
%% Description: Выполняет сценарий:
%%           0. создать вспомогательного клиента, и авторизовать его как test/mnogotestov
%%           1. --> создать вспомогательным клиентом лобби ({createLobby, ...})
%%              <-- {createLobby, ok}
%%           2. создать еще один клиент и авторизовать его как test1/mnogotestov
%%           3. --> {get, lobbies}
%%              <-- {get, lobbies, ok, LobbyInfoList}
%%           4. --> {joinLobby, 1}
%%              <-- {joinLobby, ok, LobbyInfo}
%%           5. --> {readiness, loaded}
%%              --> вспомогательный клиент: {readiness, loaded}
%%              <-- {lobbyRaceEvent, initialize}
%%              --> {readiness, initialized}
%%              --> вспомогательный клиент: {readiness, initialized}
%%              <-- {lobbyRaceEvent, go}
%%           6. --> третий клиент {joinLobby, 1}
%%              <-- {joinLobby, error, {Reason, Message}}
%%           7. --> {lapTime, finish, 1000}
%%              <-- {lobbyRaceEvent, finishCountDownStarted}

-module(lobbyTestF).

-export([start/0, start/2, run/2, runHelperClient/4]).

-include("futil.hrl").

start() ->
    start("localhost", 9000).

start(Host, Port) ->    
    run(Host, Port),
    init:stop().

connectAndAuthorizeClient(ClientID, Login, Password, Host, Port) ->
    % connect
    Socket = futil:connect(Host, Port),    
    log:write(info, "<~3..0b>{    Client~b connected to ~s:~w~n", [ClientID, ClientID, Host, Port]),
    
    % receive server time
    {ok, _} = ?receiveXml("<event name='serverTime'", ClientID, authorizing),
    
    % login - ok
    futil:sendXml(Socket, "<event name='authorize' login='" ++ Login ++ "' password='" ++ Password ++ "' vkontakteID='0' />", ClientID, authorizing),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'", ClientID, authorizing),
    Socket.   

run(Host, Port) ->
    log:write(info, "lobbyTest has been started~n", []),

    % create helper client process, connect and authorize it
	
    HelperPID = startHelperClient(0, Host, Port),
    {ok, _HelperSocket} = utils:callSync(HelperPID, connectAndAuthorize),
    
    % create lobby using helper client
    {ok, LobbyID} = utils:callSync(HelperPID, createTestLobby),

    % connect normal client
    ClientID = 1,
    Socket = connectAndAuthorizeClient(ClientID, "test1", "mnogotestov", Host, Port),
    
    % get lobbies -> ok and not empty
    futil:sendXml(Socket, "<event name='get' property='lobbies' />", ClientID, authorized),
    {ok, _} = ?receiveXml("<event name='get' property='lobbies' result='ok' channel='lobby'><list name='property'><lobbyInfo id='", ClientID, authorized),
        
    % join lobby -> ok
    futil:sendXml(Socket, "<event name='joinLobby' lobbyID='" ++ integer_to_list(LobbyID) ++ "' />", ClientID, authorized),
    {ok, _} = ?receiveXml("<event name='joinLobby' result='ok'>", ClientID, authorized),
	
    futil:sendXml(Socket, "<event name='readiness' value='loaded' />", ClientID, lobbyHall),
    ok = utils:callSync(HelperPID, sendLoaded),

    {ok, _} = ?receiveXml("<event name='lobbyRaceEvent' type='initialize'", ClientID, lobbyHall),
    
    futil:sendXml(Socket, "<event name='readiness' value='initialized' />", ClientID, lobbyHall),
    ok = utils:callSync(HelperPID, sendInitialized),
    
    {ok, _} = ?receiveXmlWithTimeout("<event name='lobbyRaceEvent' type='go'", ClientID, authorized, 4000),
    
    log:write(info, "lobbyTest has been successfuly finished~n", []),
    
    HelperPID ! {'EXIT', self(), normal},
    gen_tcp:close(Socket).    

startHelperClient(ClientID, Host, Port) ->
    spawn(lobbyTestF, runHelperClient, [ClientID, undefined, Host, Port]).

runHelperClient(ClientID, Socket, Host, Port) ->
    receive
        {Sender, connectAndAuthorize} ->
            NewSocket = connectAndAuthorizeClient(ClientID, "test", "mnogotestov", Host, Port),
            Sender ! {connectAndAuthorize, {ok, NewSocket}},
            runHelperClient(ClientID, NewSocket, Host, Port);
        {Sender, createTestLobby} ->
            futil:sendXml(Socket, "<event name='createLobby' routeID='1' lapNumber='3' direction='forward' playerMax='2' allowedCarID='-1' timerLength='1' />", ClientID, authorized),
            {ok, Tail} = ?receiveXmlTail("<event name='createLobby' result='ok'><lobbyInfo id='", ClientID, authorized),
            {LobbyID, _TailRest} = string:to_integer(Tail),
            Sender ! {createTestLobby, {ok, LobbyID}},
            runHelperClient(ClientID, Socket, Host, Port);
        {Sender, sendLoaded} ->
            futil:sendXml(Socket, "<event name='readiness' value='loaded' />", ClientID, lobbyHall),
            Sender ! {sendLoaded},
            runHelperClient(ClientID, Socket, Host, Port);
        {Sender, sendInitialized} ->
            futil:sendXml(Socket, "<event name='readiness' value='initialized' />", ClientID, lobbyHall),
            Sender ! {sendInitialized},
            runHelperClient(ClientID, Socket, Host, Port);            
        {'EXIT', _, _} ->
            gen_tcp:close(Socket),
            log:write(info, "<~3..0b> Client exited~n", [ClientID]),
            ok
    end.
