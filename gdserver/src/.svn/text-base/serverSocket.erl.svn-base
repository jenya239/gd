-module(serverSocket).

-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([getNewClientID/0, 
        startAcceptor/1, 
        acceptorLoop/1, 
        registerSession/3, 
        killSessionIfExists/1, 
        removeSession/2, 
        sendMessageToClientByUserID/2]).

-include("data.hrl").
-include("config.hrl").
        
-define(TCP_OPTIONS, [list, {active, false}, {reuseaddr, true}, {packet, 4}, {send_timeout, ?SOCKET_SEND_TIMEOUT}, {packet_size, 2048}]).
%{packet_size, 1024}

-record(state, {serverSocket, currentClientID=0, sessions}).
        
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).
    
stop() ->
    gen_server:cast(?MODULE, stop).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([Port | _Args]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, ServerSocket} ->
            log:write(debug, ?MODULE_STRING, "Started server socket on port ~w~n", [Port]),
            serverSocket:startAcceptor(ServerSocket),
            {ok, #state{serverSocket=ServerSocket, sessions=dict:new()}};            
        {error, Reason} ->
            log:write(error, ?MODULE_STRING, "Failed starting server socket on port ~w~n", [Port]),
            {stop, Reason}
    end.

%must NOT be used in game processes, for module INTERNAL use ONLY
getNewClientID() ->
    gen_server:call(?MODULE, getNewClientID).

%must NOT be used in game processes, for module INTERNAL use ONLY
registerSession(UserID, ClientPID, ClientID) ->
    gen_server:call(?MODULE, {registerSession, UserID, ClientPID, ClientID}).

%must NOT be used in game processes, for module INTERNAL use ONLY
removeSession(UserID, ClientID) ->
    gen_server:call(?MODULE, {removeSession, UserID, ClientID}).

%must NOT be used in game processes, for module INTERNAL use ONLY
killSessionIfExists(UserID) ->
    gen_server:call(?MODULE, {killSessionIfExists, UserID}).
    
sendMessageToClientByUserID(UserID, Message) ->
    gen_server:cast(?MODULE, {sendMessageToClientByUserID, UserID, Message}).
    
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call({registerSession, UserID, ClientPID, ClientID}, _From, State) ->
    NewSessions = dict:store(UserID, {ClientPID, ClientID}, State#state.sessions),
    {reply, ok, State#state{sessions=NewSessions}};
    
handle_call({removeSession, UserID, ClientID}, _From, State) ->
    NewSessions = case dict:find(UserID, State#state.sessions) of
        {ok, {_ClientPID, OldClientID}} ->
            if OldClientID =:= ClientID ->
                dict:erase(UserID, State#state.sessions);
            true -> 
                State#state.sessions 
            end;
        error -> 
            State#state.sessions 
    end,
    {reply, ok, State#state{sessions=NewSessions}};
    
handle_call({killSessionIfExists, UserID}, _From, State) ->
    NewSessions = case dict:find(UserID, State#state.sessions) of
        {ok, {ClientPID, _OldClientID}} ->            
            ClientPID ! {killSelf},
            dict:erase(UserID, State#state.sessions);
        error ->
            State#state.sessions
    end,
    
    {reply, ok, State#state{sessions=NewSessions}};
    
handle_call(getNewClientID, _From, State) ->
    NewState = State#state{currentClientID=(State#state.currentClientID + 1)},
    {reply, NewState#state.currentClientID, NewState}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast({sendMessageToClientByUserID, UserID, Message}, State) ->
    case dict:find(UserID, State#state.sessions) of
        {ok, {ClientPID, _ClientID}} ->
            ClientPID ! Message;
        error ->
            ok
    end,
    
    {noreply, State};

handle_cast(stop, State) ->
    {stop, shutdown, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info({'EXIT', _From, error}, State) ->
    serverSocket:startAcceptor(State#state.serverSocket),
    {noreply, State};
    
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
terminate(Reason, State) ->
    if Reason =:= shutdown ->
        dict:map(fun(_UserID, {ClientPID, _ClientID}) ->
            ClientPID ! {killself, shutdown}
        end, State#state.sessions);
    true ->
        ok
    end.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
startAcceptor(ServerSocket) ->
    spawn_link(?MODULE, acceptorLoop, [ServerSocket]).
    
acceptorLoop(ServerSocket) ->
    case gen_tcp:accept(ServerSocket) of
        {ok, Socket} ->
            NewClientID = serverSocket:getNewClientID(),
            {Address, Port} = case inet:peername(Socket) of
                                  {ok, {Address1, Port1}} -> {Address1, Port1};
                                  {error, Error} -> {unknown, Error}
                              end,
            log:write(info, ?MODULE_STRING, "Accepted socket <~w> from ~120p:~10000p~n", [NewClientID, Address, Port]),
            
            {ok, SocketSenderPID} = socketSender:start(Socket, NewClientID),
            {ok, ClientPID} = client:start(Socket, SocketSenderPID, NewClientID),
            {ok, SocketPID} = socket:start(Socket, NewClientID, ClientPID),            
                        
            gen_tcp:controlling_process(Socket, SocketPID),
            serverSocket:acceptorLoop(ServerSocket);
        {error, enfile} ->
            log:write(error, ?MODULE_STRING, "No room for new sockets left: ~n", []),
            serverSocket:acceptorLoop(ServerSocket);
        {error, Reason} ->
            log:write(error, ?MODULE_STRING, "Error during accept: ~w~n", [Reason]),
            exit({error, Reason})
    end.