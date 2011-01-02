%% Author: valera
%% Created: 17.10.2008
%% Description: Выполняет сценарий:
%%           0. присоединиться
%%              <-- {serverTime, Time}
%%           1. --> авторизоваться test/mnogotestov ({authorize, .. })
%%              <-- ok ({authorize, ok})
%%           2. --> запросить подсоединение к заезду "test" ({connectRace, .. })
%%              <-- команда на загрузку и инициализацию трассы ({connectRace, ok, ..})
%%              --> подтвердить окончание загрузки и инициализации ({loaded})
%% (loading) 3. *--> прислать сообщение чата ({chatMessage, ..})
%%              <--* получить чужое сообщение чата ({otherChatMessage, ..})
%%              *--> прислать приглашение ({invite, ..})
%%              <--* получить чужое приглашение ({otherInvite, ..})
%%  (racing) 4. *--> отослать свое состояние ?STATE_COUNT раз (binary)
%%              <--* получить состояние другого участника (binary)
%%              <--* добавить другого участника ({addClient, ..})
%%              <--* убрать другого участника ({removeClient, ..})
%%              *--> прислать время круга ({lapTime, ..})
%%              *--> прислать сообщение чата ({chatMessage, ..})
%%              <--* получить чужое время круга ({otherLapTime, ..})
%%              <--* получить чужое сообщение чата ({otherChatMessage, ..})
%%              <--* получить медаль ({reward, ..})
%%              *--> прислать смену машины ({set, currentCarID, ..})
%%              *--> получить подтверждение смены машины ({set, currentCarID, ..})
%%              *--> получить чужую смену машины ({set, carName, ..})
%%              *--> прислать приглашение ({invite, ..})
%%              <--* получить чужое приглашение ({otherInvite, ..})
%%              --> послать команду отсоеднения от заезда ({disconnect})
%%              <-- дождаться подтверждления ({disconnect, ok})
%%              перейти в пункт 2. (и так ConnectRaceCount циклов)

-module(racingTest).

-export([start/0, start/2, run/4]).

-include("futil.hrl").

-define(STATE_COUNT, 10).
-define(LOADING_COUNT, 3).
-define(CYCLE_TIMEOUT, 1000).

start() ->
    start("localhost", 9000).

start(Host, Port) ->    
    run(Host, Port, 0, 1),
    init:stop().

run(Host, Port, ClientID, ConnectRaceCount) ->
    % connect    
    Socket = futil:connect(Host, Port),
    log:write(info, "<~3..0b>{    racingTest connected to ~s:~w~n", [ClientID, Host, Port]),
        
    % receive server time
    {ok, _} = ?receiveXml("<event name='serverTime'", ClientID, authorizing),
        
    % login - ok
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0' />", ClientID, authorizing),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'", ClientID, authorizing),
    
    connecting(Socket, ClientID, ConnectRaceCount).

connecting(Socket, ClientID, 0) ->
    finished(Socket, ClientID);

connecting(Socket, ClientID, ConnectRaceCount) ->
    % connectToRace - ok
    futil:sendXml(Socket, "<event name='connectRace' routeID='1' direction='forward' lapNumber='1' />", ClientID, connecting),

    {ok, Xml} = ?receiveAnyXml(ClientID, connecting),

    case Xml of
        "<event name='addClient' channel='city'" ++ _ ->
            connecting(Socket, ClientID, ConnectRaceCount);
        "<event name='removeClient' channel='city'" ++ _ ->
            connecting(Socket, ClientID, ConnectRaceCount);
        "<event name='chatMessage' channel='city'" ++ _ ->
            connecting(Socket, ClientID, ConnectRaceCount);
        "<event name='otherInvite' raceID='" ++ _ ->
            connecting(Socket, ClientID, ConnectRaceCount);
        "<event name='connectRace' result='ok'" ++ _ ->
            loading(Socket, ClientID, ConnectRaceCount, ?LOADING_COUNT)
    end.

loading(Socket, ClientID, ConnectRaceCount, LoadingCount) ->
    if 
        (LoadingCount == 0) ->
            futil:sendXml(Socket, "<event name='loaded' />", ClientID, loading),
            racing(Socket, ClientID, ConnectRaceCount, ?STATE_COUNT);
        true->
            futil:sendXml(Socket, "<event name='chatMessage' channel='race' text='hello guys!' nick='' />", ClientID, loading),
            futil:sendXml(Socket, "<event name='chatMessage' channel='city' text='hello globally all!' nick='' />", ClientID, loading),
            futil:sendXml(Socket, "<event name='invite' toClientID='1' />", ClientID, loading),
            
            Message = ?receiveAnyXml(ClientID, loading),
            
            case Message of
                {ok, "<event name='chatMessage' " ++ _} ->
                    loading(Socket, ClientID, ConnectRaceCount, LoadingCount - 1);
                {ok, "<event name='addClient' channel='" ++ _} ->
                    loading(Socket, ClientID, ConnectRaceCount, LoadingCount - 1);
                {ok, "<event name='removeClient' channel='" ++ _} ->
                    loading(Socket, ClientID, ConnectRaceCount, LoadingCount - 1);
                {ok, "<event name='otherInvite' raceID='" ++ _} ->
                    loading(Socket, ClientID, ConnectRaceCount, LoadingCount - 1);
                {error, timeout} ->
                    loading(Socket, ClientID, ConnectRaceCount, LoadingCount - 1)
            end
    end.

racing(Socket, ClientID, ConnectRaceCount, StateSendCount) ->    
    if 
        StateSendCount == ?STATE_COUNT/2 ->
            %send lapTime
            futil:sendXml(Socket, "<event name='lapTime' channel='race' type='reset' time='23000' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='lapTime' channel='race' type='start' time='23100' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='lapTime' channel='race' type='finish' time='23200' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='set' property='currentCarID' value='1' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='set' property='currentCarID' value='2' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='chatMessage' channel='race' text='hello all!' nick='' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='chatMessage' channel='city' text='hello globally all!' nick='' />", ClientID, racing),
            futil:sendXml(Socket, "<event name='invite' toClientID='1' />", ClientID, racing);
        true ->
            ok
    end,
    timer:sleep(?CYCLE_TIMEOUT),
    if 
        StateSendCount > 0 ->
            futil:sendBin(Socket, [1,2,3,4,5], ClientID, racing);
        StateSendCount == 0 ->
            % send quitRace
            futil:sendXml(Socket, "<event name='disconnect' />", ClientID, racing);
        true ->
            ok
    end,
    
    Message = ?receiveMessage(ClientID, racing),
    
    case Message of
        {ok, {xml, "<event name='addClient' channel='" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='removeClient' channel='" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {bin, _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='lapTime'" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='chatMessage'" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='set' property='carName'" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='set' property='currentCarID'" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='raceResult' rewardType='" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='otherInvite' raceID='" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='level'" ++ _}} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1);
        {ok, {xml, "<event name='disconnect' result='ok'" ++ _}} ->
            connecting(Socket, ClientID, ConnectRaceCount - 1);
        
        {error, timeout} ->
            racing(Socket, ClientID, ConnectRaceCount, StateSendCount - 1)
    end.
    
    
finished(Socket, ClientID) ->
    gen_tcp:close(Socket),
    log:write(info, "<~3..0b>}    racingTest is finished.~n", [ClientID]).
    
