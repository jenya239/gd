-module(bot).

-include("config.hrl").
-include("futil.hrl").

-define(BOT_CFG_PATH, "cfg/bot.cfg").

-behaviour(gen_server).

-export([start/2]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([authorize/1, swarm/1, swarm/3, 
        simpleBehaviour/1, get/2, connect/1]).

-define(MAX_RECONNECT_COUNTER, 100).

-record(state, {name, socket, vkontakteID, gameHost, gamePort}).
-record(behaviourState, {botPID, reconnectCounter}).

start(VkontakteID, Config) ->    
    gen_server:start(?MODULE, [VkontakteID, Config], []).
    
%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([VkontakteID, Config]) ->
    GameHost = utils:getValue(Config, gameHost),
    GamePort =  utils:getValue(Config, gamePort),
    
    {ok, #state{gameHost=GameHost, gamePort=GamePort, vkontakteID=VkontakteID}}.
        
swarm(0, _VkontakteID, _Config) ->
    ok;
    
swarm(Counter, VkontakteID, Config) ->
    {ok, Pid} = bot:start(VkontakteID, Config),
    erlang:spawn(?MODULE, simpleBehaviour, [#behaviourState{botPID=Pid, reconnectCounter=0}]),
    
    timer:sleep(100),
    
    swarm(Counter - 1, VkontakteID + 1, Config).
    
swarm(Counter) ->
    log:start_link(""),
    Config = utils:readConfig(?BOT_CFG_PATH),
    swarm(Counter, 1, Config).
        
connect(BotPID) ->
    gen_server:call(BotPID, connect).
    
authorize(BotPID) ->
    gen_server:call(BotPID, authorize).
    
get(BotPID, Property) ->
    gen_server:call(BotPID, {get, Property}).
    
simpleBehaviour(State) ->
    BotPID = State#behaviourState.botPID,

    case bot:connect(BotPID) of
        {error, CReason} ->
            error_logger:error_msg("~w couldn't connect: ~p~n", [BotPID, CReason]),
            false;
        _ ->
            case bot:authorize(BotPID) of
                {error, AReason} ->
                    error_logger:error_msg("~w couldn't authorize: ~p~n", [BotPID, AReason]),
                    false;
                _ ->
                    log:write("Authorized, getting info~n",[]),
                    % Get ratings, routes, lobbies
                    bot:get(BotPID, ratings),
                    bot:get(BotPID, routes),
                    bot:get(BotPID, lobbies)
            end
    end.
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(authorize, _From, State) ->
    Socket = State#state.socket,
    VkontakteID = State#state.vkontakteID,
    
    futil:sendXml(Socket, utils:fmt("<event name='authorizeVkontakte' vkontakteID='~w' authKey='' firstLastName='Mr.Botovsky' />", [VkontakteID]), VkontakteID, undefined),
    Reply = case ?receiveXml("<event name='authorize' result='ok'", VkontakteID, undefined) of
        {ok, Message} ->
            case string:str(Message, "<triggerInfo name='register'") of
                0 ->
                    ok;
                _Other ->
                    Car = random:uniform(3),
                    Color = random:uniform(8),
                    City = random:uniform(2),
                    futil:sendXml(Socket, utils:fmt("<event name='register' nickname='' car='~w' color='~w' city='~w' />", [Car, Color, City]), VkontakteID, undefined),
                    case ?receiveXml("<event name='register' result='ok'", VkontakteID, undefined) of
                        {ok, _Msg} ->
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end,
        
    {reply, Reply, State#state{vkontakteID=VkontakteID}};

handle_call(connect, _From, State) ->
    if State#state.socket =/= undefined andalso State#state.socket =/= nosocket ->
        gen_tcp:close(State#state.socket);
    true ->
        ok
    end,
    
    Reply = futil:connect(State#state.gameHost, State#state.gamePort),
    Socket = case Reply of
        {error, _Reason} ->
            nosocket;
        Socket_ ->
            Socket_
    end,
    
    ?receiveXml("<event name='serverTime'", State#state.vkontakteID, undefined),
    {reply, Reply, State#state{socket=Socket}};
    
handle_call({get, Property}, _From, State) ->
    Socket = State#state.socket,
    VkontakteID = State#state.vkontakteID,
    
    futil:sendXml(Socket, utils:fmt("<event name='get' property='~w' />", [Property]), VkontakteID, undefined),
    Reply = case ?receiveXml("<event name='get' property='", State#state.vkontakteID, undefined) of
        {ok, _Msg} ->
            ok;
        _Other ->
            {error, timeout}
    end,

    {reply, Reply, State};

handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
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