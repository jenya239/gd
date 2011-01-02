%% Author: valera
%% Created: 17.10.2008
%% Description: Выполняет сценарий:
%%           0. присоединиться
%%              <-- {serverTime, Time}
%%           1. --> попытаться авторизоваться неправильным юзером
%%              <-- ошибка
%%		1.a -> правильный юзер неправильный пароль
%%			<- ошибка
%%           2. --> test/mnogotestov
%%              <-- ok
%%           3. --> test/mnogotestov
%%              <-- таймаут
%%           4. --> logout
%%              --> test/mnogotestov
%%              <-- ok
%%           5. --> {logout}
%%              --> {quickConnect}
%%              <-- {quickConnect, ok, ...}
%%                -->{logout}
%%                -->  test/mnogotestov
%%              <-- ok   

-module(authorizationTest).

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
    log:write(info, "<~3..0b>{    authorizationTest connected to ~s:~w~n", [ClientID, Host, Port]),
    
    % receive server time
    {ok, _} = ?receiveXml("<event name='serverTime'", ClientID, undefined),
        
    % login error
    futil:sendXml(Socket, "<event name='authorize' login='test1' password='incorrectpassword' vkontakteID='0' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='error'", ClientID, undefined),
    
    % login error
    futil:sendXml(Socket, "<event name='authorize' login='test' password='incorrectpassword' vkontakteID='0' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='error'", ClientID, undefined),
    
    % login ok
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'", ClientID, undefined),
    
     % login timeout
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0' />", ClientID, undefined),
    {error, timeout} = ?receiveXml("<event name='authorize' result='ok'", ClientID, undefined),
    
    % logout, login ok
    futil:sendXml(Socket, "<event name='logout' />", ClientID, undefined),
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'", ClientID, undefined),
        
    % logout, login ok
    futil:sendXml(Socket, "<event name='logout' />", ClientID, undefined),
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0'/>", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'", ClientID, undefined),
    
    gen_tcp:close(Socket),
    
    log:write(info, "<~3..0b>}    authorizationTest is finished.~n", [ClientID]).
    