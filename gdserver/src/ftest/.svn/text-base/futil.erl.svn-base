%% Author: valera
%% Created: 17.10.2008
%% Description: Содержит сервисные функции, полезные для функционально тестирования
    
-module(futil).

-export([connect/2, sendXml/4, sendBin/4, receiveBin/2, receiveBin/3]).

-define(TCP_OPTIONS, [list, {active, true}, {packet, 4}]).
-define(TIMEOUT, 500).

-include("futil.hrl").

% Соединяется с адресом Host:Port
% Возвращает: Socket или ошибку
connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS) of
        {ok, Socket} ->
            Socket;
        {error, Reason} ->
            {error, Reason}
    end.

% Добавляет в Xml сигнатуру, отправляет в сокет и выводит запись в лог
sendXml(Socket, Xml, ClientID, State) ->
    gen_tcp:send(Socket, [0, 0, 0, 128] ++ Xml),
    log:write(debug, "<~3..0b> <~4.._s> --> ~-60s~n", [ClientID, atom_to_list(State), Xml]).

% Добавляет в Bin сигнатуру, отправляет в сокет и выводит запись в лог
sendBin(Socket, Bin, ClientID, State) ->
    gen_tcp:send(Socket, [0, 0, 0, 64] ++ Bin),
    log:write(debug, "<~3..0b> <~4.._s> --> ~-60w~n", [ClientID, atom_to_list(State), Bin]).

% Ждет получение данных из сокета или таймаута (длина задана по умолчанию) и выводит запись в лог
% Возвращает: {xml, Xml сообщение}, {bin, бинарное сообщение}, {timeout}
receiveBin(ClientID, State) ->
    receiveBin(ClientID, State, ?TIMEOUT).

% Ждет получение данных из сокета или таймаута (берется из параметра Timeout)и выводит запись в лог
% Возвращает: {xml, Xml сообщение}, {bin, бинарное сообщение}, {timeout}
receiveBin(ClientID, State, Timeout) ->
    receive
        {tcp, _, [0, 0, 0, 64 | Bin]} -> 
            log:write(debug, "<~3..0b> <~4.._s> <-- ~-60w~n", [ClientID, atom_to_list(State), Bin]),
            {ok, Bin};
        {tcp_error, _, Reason } ->
            {error, Reason};
        {tcp_closed, _} ->
            {error, closed}
        after Timeout ->
            {error, timeout}
    end.

    

    