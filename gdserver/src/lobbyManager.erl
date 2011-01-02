-module(lobbyManager).

-behaviour(gen_server).

-export([start/2, start_link/2]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
    
-export([stop/1, 
        requestLobbies/1,
        createLobby/5,
        joinLobby/5,
        removeLobby/2,
        requestLobbyInfo/2,
        getLobbyIDByPID/2,
        updateLobbyInfo/2,
        routeAvailable/2,
        getState/1
    ]).
    
-include("data.hrl").
-include("config.hrl").
-include("lib/eunit/include/eunit.hrl").

-record(state, {name, lobbies, nextLobbyID, cityID}).

start(LobbyManagerName, CityID) ->    
    gen_server:start({local, LobbyManagerName}, ?MODULE, [CityID], []).

start_link(LobbyManagerName, CityID) ->    
    gen_server:start_link({local, LobbyManagerName}, ?MODULE, [CityID], []).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([CityID]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    {ok, #state{lobbies=dict:new(), nextLobbyID=1, cityID=CityID}}.
        
stop(LobbyManagerPID) ->
    gen_server:cast(LobbyManagerPID, stop).
        
requestLobbies(LobbyManagerPID) ->     
    gen_server:cast(LobbyManagerPID, {requestLobbies, self()}).

requestLobbyInfo(LobbyManagerPID, LobbyID) -> 
    gen_server:cast(LobbyManagerPID, {requestLobbyInfo, LobbyID, self()}).

createLobby(LobbyManagerPID, LobbyInfo, FirstClientPID, FirstClientInfo, User) ->
    gen_server:cast(LobbyManagerPID, {createLobby, LobbyInfo, FirstClientPID, FirstClientInfo, User}).

joinLobby(LobbyManagerPID, LobbyID, ClientPID, ClientInfo, User) ->
    gen_server:cast(LobbyManagerPID, {joinLobby, LobbyID, ClientPID, ClientInfo, User}).

%must NOT be used in game processes, use by YAWS and TESTS ONLY
removeLobby(LobbyManagerPID, LobbyID) ->
    gen_server:call(LobbyManagerPID, {removeLobby, LobbyID}).

findPID(FindPID, [{LobbyID, {LobbyPID, _}} | _Tail]) when FindPID =:= LobbyPID ->
    LobbyID;

findPID(FindPID, [{_LobbyID, {_LobbyPID, _}} | Tail]) ->
    findPID(FindPID, Tail);

findPID(_FindPID, []) ->
    undefined.

getLobbyIDByPID(LobbyPID, Lobbies) ->
    List = dict:to_list(Lobbies),
    findPID(LobbyPID, List).
    
updateLobbyInfo(LobbyManagerPID, NewLobbyInfo) ->
    gen_server:cast(LobbyManagerPID, {updateLobbyInfo, NewLobbyInfo}).    

%for debug only!!!
getState(LobbyManagerPID) ->
    gen_server:call(LobbyManagerPID, getState).    


%%-----------------------------------------------------------------------------
%% RPC calls
%%-----------------------------------------------------------------------------
    
handle_call({removeLobby, LobbyID}, _From, State) ->    
    NewLobbies = dict:erase(LobbyID, State#state.lobbies),
    log:write(debug, ?MODULE_STRING, "Lobby ~10000p has been removed ~n", [LobbyID]),

    {reply, ok, State#state{lobbies=NewLobbies}};

handle_call(getState, _From, State) ->
    {reply, State, State}.

%%-----------------------------------------------------------------------------
%% Message casts
%%-----------------------------------------------------------------------------

handle_cast({createLobby, LobbyInfo, FirstClientPID, FirstClientInfo, User}, State) ->
    removeFromAllLobbies(FirstClientInfo#clientInfo.clientID, State),

    NewState = case checkLobbyPossibility(LobbyInfo, FirstClientInfo, User) of
    ok ->
        NextLobbyID = State#state.nextLobbyID,
        
        Type = if State#state.cityID =:= 3 andalso LobbyInfo#lobbyInfo.league =/= 4 ->
            team;
        true ->
            normal
        end,
        
        NewLobbyInfo = LobbyInfo#lobbyInfo{id=NextLobbyID, type = Type},
        {ok, NewLobbyPID} = lobby:start(NewLobbyInfo, State#state.cityID, self()),
        erlang:monitor(process, NewLobbyPID),
        lobby:addClient(FirstClientPID, FirstClientInfo, NewLobbyPID, joinLobby),
        NewLobbies = dict:store(State#state.nextLobbyID, {NewLobbyPID, NewLobbyInfo}, State#state.lobbies),
        NewState1 = State#state{nextLobbyID=NextLobbyID+1, lobbies=NewLobbies},
        NewState1;
    {error, {Reason, Message}} ->
        FirstClientPID ! {createLobby, error, {Reason, Message}},
        State
    end,
    
    {noreply, NewState};
    
handle_cast({joinLobby, LobbyID, ClientPID, ClientInfo, User}, State) -> 
    removeFromAllLobbies(ClientInfo#clientInfo.clientID, State),    
    case dict:find(LobbyID, State#state.lobbies) of
        {ok, {LobbyPID, LobbyInfo}} ->
            case checkLobbyPossibility(LobbyInfo, ClientInfo, User) of
                ok ->
                    lobby:addClient(ClientPID, ClientInfo, LobbyPID, joinLobby);
                {error, {Reason, Message}} ->
                    ClientPID ! {joinLobby, error, {Reason, Message}}
            end;
        error ->
            ClientPID ! {joinLobby, error, {cannotJoinLobby, "[[lobbyNotFound]]"}}
    end,
    
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({updateLobbyInfo, NewLobbyInfo}, State) ->
    NewLobbies = try 
        dict:update(
            NewLobbyInfo#lobbyInfo.id, 
            fun({LobbyPID, _OldLobbyInfo}) ->
                {LobbyPID, NewLobbyInfo}
            end,
            State#state.lobbies)
    catch error:_ ->
        State#state.lobbies
    end,
    
    {noreply, State#state{lobbies=NewLobbies}};

handle_cast({requestLobbies, From}, State) ->
    LobbiesList = lists:reverse(dict:fold(
        fun(_Key, {_Pid, LobbyInfo}, List) -> 
            [LobbyInfo | List]
        end, [], State#state.lobbies)),    
    From ! {requestLobbies, ok, LobbiesList},
    
    {noreply, State};
    
handle_cast({requestLobbyInfo, LobbyID, From}, State) ->
    case dict:find(LobbyID, State#state.lobbies) of
        {ok, {LobbyPID, _LobbyInfo}} ->
            lobby:requestLobbyInfo(LobbyPID, From);
        error -> 
            From ! {requestLobbyInfo, error, {lobbyNotFound, "[[lobbyNotFound]]"}}
    end,
    
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Info calls
%%-----------------------------------------------------------------------------
handle_info({'DOWN', _, process, LobbyPID, Info}, State) ->     
    LobbyID = getLobbyIDByPID(LobbyPID, State#state.lobbies),
    NewState = if 
        LobbyID =/= undefined ->
            NewLobbies = dict:erase(LobbyID, State#state.lobbies),
            log:write(debug, ?MODULE_STRING, "Lobby ~10000p has died: ~10000p~n", [LobbyID, Info]),
            State#state{lobbies=NewLobbies};
        true -> 
            State
    end,
    {noreply, NewState}.
    
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
%% Internal
%%-----------------------------------------------------------------------------

routeAvailable(RouteID, User)->
    Route = mneser:getRecord(route,RouteID),
    Route#route.minLevel =< User#user.level.

removeFromAllLobbies(ClientID, State) ->
    dict:fold(
        fun(_, {LobbyPID, _}, _) ->
            lobby:removeClient(ClientID, LobbyPID)
        end, undefined, State#state.lobbies).      

checkLobbyPossibility(LobbyInfo, ClientInfo, User) ->
    Route = mneser:getRecord(route, LobbyInfo#lobbyInfo.routeID),
    EnoughFuel = fuel:checkFuel(LobbyInfo, ClientInfo),
    if User#user.level < Route#route.minLevel ->
        {error, {lowLevel, utils:fmt("[[routeAvailableFromLevelX, ~w]]", [Route#route.minLevel])}};
    not EnoughFuel ->
        {error, {notEnoughFuel, "[[notEnoughFuel]]"}};
    (User#user.rating - LobbyInfo#lobbyInfo.creatorRating) > 200 andalso LobbyInfo#lobbyInfo.league =:=4 ->
        {error,{highRating, "[[playerRatingTooLowForYou]]"}};
    (User#user.money < LobbyInfo#lobbyInfo.stake andalso LobbyInfo#lobbyInfo.league =:=4) ->
        {error,{notEnaughMoney, "[[notEnoughMoneyForDuelUsePublicRacings]]"}};
    (LobbyInfo#lobbyInfo.stake < 50 andalso LobbyInfo#lobbyInfo.league =:=4) ->
        {error,{lowStake, "[[minDuelBet, 50]]"}};
    (LobbyInfo#lobbyInfo.stake > 2000 andalso LobbyInfo#lobbyInfo.league =:=4) ->
        {error,{highStake, "[[maxDuelBet, 2000]]"}};
    true ->
        ok
    end.

