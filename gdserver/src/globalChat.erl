-module(globalChat).

-include("config.hrl").
-include("data.hrl").
-include("lib/eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([getClientPID/2,
         addClient/3, 
         removeClient/2,
         onlineUsers/1]).
        
-record(state, {cityID, clients, history}).

start_link(CityID) when is_number(CityID) ->    
    gen_server:start_link({local, list_to_atom(?MODULE_STRING ++ integer_to_list(CityID))}, ?MODULE, [CityID], []);
    
start_link(noRegister) ->
    gen_server:start_link(?MODULE, [], []).
    
%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([CityID]) ->
    log:write(debug, ?MODULE_STRING, "process started for city:~w ~n", [CityID]),    
    CityMessages = dbMessage:getLatest(type, {city, CityID}),
    
    {ok, #state{cityID=CityID, clients=dict:new(), history=CityMessages}}.
    
stop() ->
    gen_server:cast(?MODULE, stop).
    
%must NOT be used in game processes, use by YAWS and TESTS ONLY
getClientPID(GlobalChat, ClientID) ->
    gen_server:call(GlobalChat, {getClientPID, ClientID}).
            
addClient(GlobalChatPID, ClientInfo, ClientPID) ->
    gen_server:cast(GlobalChatPID, {addClient, global, ClientInfo, ClientPID}).
    
removeClient(GlobalChatPID, ClientID) ->
    gen_server:cast(GlobalChatPID, {removeClient, global, ClientID}).
    
%must NOT be used in game processes, use by YAWS and TESTS ONLY
onlineUsers(GlobalChatPID) ->
    gen_server:call(GlobalChatPID, onlineUsers, infinity).
        
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call({getClientPID, ClientID}, _From, State) ->
    Reply = getClientPIDByClientID(ClientID, State#state.clients),
    {reply, Reply, State};
    
handle_call(onlineUsers, From, State) ->
    spawn(fun() ->
        Clients = dict:fold(
            fun(_ClientID, { PID, ClientInfo}, ClientInfoList) ->
                StateName = case catch client:getState(PID) of
                    {'EXIT', _Reason} ->
                        error;
                    Other ->
                        Other
                end,
                User = dbUser:getRecord(id, ClientInfo#clientInfo.userID),
                Rec = {ClientInfo#clientInfo.userID, ClientInfo#clientInfo.displayName, StateName, User#user.roles},
                [Rec | ClientInfoList]
            end, [], State#state.clients),
        gen_server:reply(From, Clients)
    end),
    {noreply, State};

handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast({addClient, global, ClientInfo, ClientPID}, State) ->
    sendChannelChangedMessageToPid(ClientPID, State#state.cityID),
    sendMessagesToPID(State#state.history, ClientPID),
    Clients2 = addAndBroadcastAddClient(ClientPID, ClientInfo, State#state.clients),
    
    {noreply, State#state{clients=Clients2}};
    
handle_cast({removeClient, global, ClientID}, State) ->
    Clients2 = removeAndBroadcastRemoveClient(ClientID, State#state.clients),
    
    {noreply, State#state{clients=Clients2}};
    
handle_cast(_Message, State) ->
    {noreply, State}.
        
%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info({chatMessage, Channel, Text, ClientID}, State) ->
    {UserID, HomeCity, DisplayName} = case ClientID =:= system of
        true ->
            {system, 0, "[[systemMessage]]"};
        false ->
            case dict:find(ClientID, State#state.clients) of
                {ok, {_,ClientInfo}} ->
                    UserID_ = ClientInfo#clientInfo.userID,
                    User = dbUser:getRecord(id, UserID_),
                    {UserID_, User#user.homeCity, User#user.name};
                _Error ->
                    {-1, 0, error}
            end
    end,
    
    NewState = if {UserID, DisplayName} =/= {-1, error} ->
        TimeStamp = utils:now(),
        {ok, NewMessage} = dbMessage:add(UserID, {Channel, State#state.cityID}, Text, TimeStamp),
        utils:sendMessageToClient(ClientID, {otherChatMessage, Channel, Text, UserID, HomeCity, DisplayName, TimeStamp}, State#state.clients),
        utils:sendToAllExceptOne({otherChatMessage, Channel, Text, UserID, HomeCity, DisplayName, TimeStamp}, State#state.clients, ClientID),
        log:write(debug, ?MODULE_STRING, "broadcasted globalChat message from <~10000p>: ~10000p~n", [ClientID, Text]),
        addChatMessage(NewMessage, State);
    true ->
        State
    end,
    
    {noreply, NewState};

handle_info(_Event, State) ->
    {noreply, State}.
    
%%-----------------------------------------------------------------------------
%% Code change
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Terminate
%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
        
%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------    

addChatMessage(NewMessage, State) ->
    OldHistory = State#state.history,
    NewHistory = case OldHistory of
        [_OldestMessage | Tail] when length(OldHistory) =:= 10 ->
            Tail ++ [NewMessage];
        ShorterList ->
            ShorterList ++ [NewMessage]
    end,
    
    State#state{history=NewHistory}.

% findClientPIDByUserID(FindUserID, [{_, {ClientPID, #clientInfo{userID=UserID}}} | _Tail]) when FindUserID =:= UserID ->
%     ClientPID;
% 
% findClientPIDByUserID(FindUserID, [{_, {_, _}} | Tail]) ->
%     findClientPIDByUserID(FindUserID, Tail);
% 
% findClientPIDByUserID(_FindUserID, []) ->
%     undefined.

addAndBroadcastAddClient(ClientPID, ClientInfo, Clients) ->
    ClientID = ClientInfo#clientInfo.clientID,
    Clients2 = dict:store(ClientID, {ClientPID, ClientInfo}, Clients),
    %utils:sendToAllExceptOne({addClient, global, ClientInfo}, Clients, ClientID),
    log:write(debug, ?MODULE_STRING, "added client ~10000p~n", [ClientID]),
    Clients2.

removeAndBroadcastRemoveClient(ClientID, Clients) ->
    Clients2 = dict:erase(ClientID, Clients),
    utils:sendToAllExceptOne({removeClient, global, ClientID}, Clients, ClientID),
    log:write(debug, ?MODULE_STRING, "removed client ~10000p~n", [ClientID]),
    Clients2.
    
sendMessagesToPID(Messages, ClientPID) ->
    lists:foreach(
        fun(Message)->
            {UserDisplayName, HomeCity} = case Message#message.from =:= system orelse Message#message.from =:= -1 of
                true->
                    {"[[systemMessage]]", 0};
                false ->                    
                    case dbUser:getRecord(id, Message#message.from) of                
                        User when is_record(User, user) -> {User#user.name, User#user.homeCity};
                        _Error -> ""
                    end
            end,
            
            {Channel, _} = Message#message.receiver,
            ClientPID ! {otherChatMessage, Channel, Message#message.body, Message#message.from, HomeCity, UserDisplayName, Message#message.date}
        end, 
        Messages
    ).

getClientPIDByClientID(ClientID, Clients) ->
    case dict:find(ClientID, Clients) of
        {ok, {ClientPID, _ClientInfo}} ->
            {ok, ClientPID};
        error ->
            {error, notFound}
    end.
    
sendChannelChangedMessageToPid(ClientPID, CityID) ->
    Text = case CityID of
        1 ->
            "[[blueCity]]";
        2 ->
            "[[redCity]]";
        3 ->
            "[[desertCity]]"
    end,
    
    ClientPID ! {otherChatMessage, city, Text, system, 0, "[[locationChange]]", utils:now()}.
