%% Author: valera
%% Created: 17.10.2008
%% Description: Выполняет сценарий:
%%           0. присоединиться
%%              <-- {serverTime, Time}
%%           1. --> авторизоваться test/mnogotestov ({authorize, Login, Password})
%%              <-- ok ({authorize, ok, UserInfo})
%%           2.1 --> запросить подсоединение к кривому типу игры заезду "test" ({connect, RouteID, forward, 10000})
%%              <-- ошибка подсоединение ({connect, error, {Reason, Message}})
%%           2.2 --> запросить подсоединение к заезду "test" ({connect, RouteID})
%%              <-- команда на загрузку и инициализацию трассы ({connect, ok, RouteID, CarID})
%%           3. --> отменяем загрузку ({disconnect})
%%           4. <-- {disconnect, ok}
%%           5. --> 2ой раз запросить подсоединение к заезду "test" ({connect, RouteID})
%%              <-- 2ой раз команда на загрузку и инициализацию трассы ({connect, ok, RouteID, CarID})
%%           6. --> на этот раз завершаем загрузку ({loaded})
%%           7. --> 2ой раз отсоединяемся ({disconnect})
%%           8. <-- {disconnect, ok}
%%           9. --> 3й раз запросить подсоединение к заезду "test" ({connect, RouteID})
%%              <-- 3й раз команда на загрузку и инициализацию трассы ({connect, ok, RouteID, CarID})
%%          10. --> глобальный чат ({chatMessage, Text})
%%          11. --> 3й раз отменяем загрузку ({disconnect})
%%          12. <-- {disconnect, ok}
%%          13. --> глобальный чат ({chatMessage, Text})
%%          14. --> запросить информацию профайла юзера ({get, userInfo})
%%              <-- ok ({get, userInfo, ok, UserInfo}, внутри то же, что вернулось в {authorize, ok, UserInfo})
%%          15. --> запросить смену машины ({set, currentCarID, 1)
%%              <-- ok {set, currentCarID, ok, 1}
%%          16. --> запросить рекорды ({get, cars})
%%              <-- ok {get, cars, Cars}
%%          17. --> запросить трассы ({get, routes, RouteID})
%%              <-- ok {get, routes, Routes}
%%          18. --> запросить join к несуществующенму клиенту
%%              <-- error {error, ...}

%           На данный момент это не работает
%%          19. --> отослать {activationCode, 11111}
%%              <-- ok 



%%          20. --> отослать {activationCode, 22222}
%%              <-- {error, ..}
%%          21. --> отослать {get, gameTypes}
%%              <-- {get, gameTypes, ok, ..}

-module(notConnectedTest).

-export([start/0, start/2, run/3]).

-include("futil.hrl").

start() ->
    start("localhost", 9000).

start(Host, Port) ->    
    run(Host, Port, 0),
    init:stop().

run(Host, Port, ClientID) ->
    % connect    
    Socket = futil:connect(Host, Port),
    log:write(info, "<~3..0b>{    menuTest connected to ~s:~w~n", [ClientID, Host, Port]),
        
    % receive server time
    {ok, _} = ?receiveXml("<event name='serverTime'", ClientID, undefined),
        
    % {authorize, ..} - ok
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0'/>", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'>", ClientID, undefined),
    
    % {chatMessage, Text}
    futil:sendXml(Socket, "<event name='chatMessage' channel='city' text='hello' nick='' />", ClientID, undefined),
        
    % {chatMessage, Text}
    futil:sendXml(Socket, "<event name='chatMessage' channel='city' text='hello2' nick='' />", ClientID, undefined),
    
    % {get, userInfo, ..} - ok
    futil:sendXml(Socket, "<event name='get' property='userInfo' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='get' property='userInfo' result='ok'>", ClientID, undefined),
    
    % {get, cars} - ok
    futil:sendXml(Socket, "<event name='get' property='cars' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='get' property='cars' result='ok'", ClientID, undefined),
    
    % {get, routes} - ok
    futil:sendXml(Socket, "<event name='get' property='routes' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='get' property='routes' result='ok'", ClientID, undefined),
        
    % {activationCode, 22222} - {error, ..}
    futil:sendXml(Socket, "<event name='activationCode' value='22222' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='activationCode' result='error'", ClientID, undefined),
    
    % {get, gameTypes} - {get, gameType, ok..}
    futil:sendXml(Socket, "<event name='get' property='gameTypes' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='get' property='gameTypes' result='ok'", ClientID, undefined),
    
    futil:sendXml(Socket, "<event name='get' property='vkontakteInfo' />", ClientID, undefined),
    {ok, _} = ?receiveXmlWithTimeout("<event name='get' property='vkontakteInfo'", ClientID, undefined,10000),
    
    gen_tcp:close(Socket),
    
    log:write(info, "<~3..0b>}    menuTest is finished.~n", [ClientID]).
    