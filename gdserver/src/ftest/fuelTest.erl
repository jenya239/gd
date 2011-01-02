-module(fuelTest).

-export([start/0, start/2, run/3]).

-include("futil.hrl").

start() ->
    start("localhost", 9000).

start(Host, Port) ->
    run(Host, Port, 0),
    init:stop().

run(Host, Port, ClientID) ->
    Socket = futil:connect(Host, Port),
    log:write(info, "<~3..0b>{    fuelTest connected to ~s:~w~n", [ClientID, Host, Port]),
    
    futil:sendXml(Socket, "<event name='authorize' login='test' password='mnogotestov' vkontakteID='0' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='authorize' result='ok'", ClientID, undefined),

    futil:sendXml(Socket, "<event name='startWork' workID='1' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='startWork' result='ok'", ClientID, undefined),

    futil:sendXml(Socket, "<event name='cancelWork' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='cancelWork' result='ok'", ClientID, undefined),

    futil:sendXml(Socket, "<event name='startWork' workID='5' />", ClientID, undefined),
    {ok, _} = ?receiveXmlWithTimeout("<event name='finishWork'", ClientID, undefined,10000),

     futil:sendXml(Socket, "<event name='get' property='gasInfo' />", ClientID, undefined),
    {ok, _} = ?receiveXml("<event name='get' property='gasInfo' result='ok'>", ClientID, undefined),

    gen_tcp:close(Socket),

    log:write(info, "<~3..0b>}    authorizationTest is finished.~n", [ClientID]).


