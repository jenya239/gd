-module(cityManager).

-behaviour(gen_server).

-include("config.hrl").
-include("data.hrl").
-include("lib/eunit/include/eunit.hrl").

-export([start_link/0]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([
        getLobbyManagerByID/1,
        getCityChatByID/1, 
        countOnlineUsers/0,
        addClient/2,
        removeClient/2, 
        moveClient/4
        ]).
        
-record(state, {currentOnline=0, onlineCounter}).
    
start_link() ->    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
addClient(ClientInfo, ClientPID) ->
    gen_server:cast(?MODULE, {addClient, global, ClientInfo, ClientPID}).
    
removeClient(CityID, ClientID) ->
    gen_server:cast(?MODULE, {removeClient, global, CityID, ClientID}).
    
countOnlineUsers() ->
    gen_server:call(?MODULE, countOnlineUsers).
    
getLobbyManagerByID(CityID) ->
    LBID = case CityID of
        1 -> 1;
        2 -> 1;
        3 -> 3
    end,
    list_to_atom( "lobbyManager" ++ integer_to_list( LBID ) ).

getCityChatByID(CityID) ->
    list_to_atom( "globalChat" ++ integer_to_list( 1 ) ).   %integer_to_list(CityID)).
    
moveClient(ClientInfo, ClientPID, CitySrcID, CityDstID) ->
    gen_server:cast(?MODULE, {moveClient, ClientInfo, ClientPID, CitySrcID, CityDstID}).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([]) ->
    log:write(debug, ?MODULE_STRING, "process started~n", []),
    OnlineCounter = case dbOnlineStats:getStatsForTime(utils:now()) of
        error ->
            #onlineStats{max=0, min=0};
        Other ->
            Other
    end,
    
    {ok, #state{onlineCounter=OnlineCounter}}.
    
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(countOnlineUsers, _From, State) ->
    {reply, State#state.currentOnline, State};

handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast({addClient, global, ClientInfo, ClientPID}, State) ->
    NewOnline = State#state.currentOnline + 1,
    NewOnlineCounter = updateOnlineCounter(State#state{currentOnline=NewOnline}),
    
    %globalChat:addClient(getCityChatByID(ClientInfo#clientInfo.currentCity), ClientInfo, ClientPID),
    addToChat(ClientInfo, ClientInfo#clientInfo.currentCity, ClientPID),

    {noreply, State#state{currentOnline=NewOnline, onlineCounter=NewOnlineCounter}};

handle_cast({removeClient, global, CityID, ClientID}, State) ->
    NewOnline = State#state.currentOnline - 1,
    NewOnlineCounter = updateOnlineCounter(State#state{currentOnline=NewOnline}),
    
    globalChat:removeClient(getCityChatByID(CityID), ClientID),
    
    {noreply, State#state{currentOnline=NewOnline, onlineCounter=NewOnlineCounter}};
    
handle_cast({moveClient, ClientInfo, ClientPID, CitySrcID, CityDstID}, State) ->
    globalChat:removeClient(getCityChatByID(CitySrcID), ClientInfo#clientInfo.clientID),
    %globalChat:addClient(getCityChatByID(CityDstID), ClientInfo, ClientPID),
    addToChat(ClientInfo, CityDstID, ClientPID),
    {noreply, State};
    
handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info(_Message, State) ->
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
addToChat(ClientInfo, CityDstID, ClientPID) ->
    case users:checkRole((ClientInfo#clientInfo.userInfo)#userInfo.user, dj) of
        true ->
            globalChat:addClient(getCityChatByID(1), ClientInfo, ClientPID),
            globalChat:addClient(getCityChatByID(2), ClientInfo, ClientPID),
            globalChat:addClient(getCityChatByID(3), ClientInfo, ClientPID);
        false ->
            globalChat:addClient(getCityChatByID(CityDstID), ClientInfo, ClientPID)
    end.
    

updateOnlineCounter(State) ->
    Hour = utils:now() div (60 *60 * 1000),
    OnlineCounter = State#state.onlineCounter,
    NewSize = State#state.currentOnline,
    
    NewCounter = case OnlineCounter#onlineStats.timestamp < Hour of
        true ->
            #onlineStats{timestamp=Hour, min=NewSize, max=NewSize};
        false ->
            OC = if NewSize > OnlineCounter#onlineStats.max ->
                OnlineCounter#onlineStats{max=NewSize};
            true ->
                OnlineCounter
            end,
            
            if NewSize < OnlineCounter#onlineStats.min ->
                OC#onlineStats{min=NewSize};
            true ->
                OC
            end            
    end,
    
    if NewCounter =/= OnlineCounter ->
        dbOnlineStats:write(NewCounter);
    true ->
        ok
    end,
    
    NewCounter.