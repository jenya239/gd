-module(flashSecurity).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([startAcceptor/1, acceptorLoop/2, readerListen/1]).
        
-define(TCP_OPTIONS, [list, {active, false}, {reuseaddr, true}, {packet, 0}]).
-define(CROSS_DOMAIN_ANSWER, 
    "<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>").
    
-record(state, {serverSocket}).    
        
start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([Port | _Args]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, ServerSocket} ->
            log:write(debug, ?MODULE_STRING, "flash security listener has been started on port ~w~n", [Port]),
            flashSecurity:startAcceptor(ServerSocket),
            {ok, #state{serverSocket=ServerSocket}};            
        {error, Reason} ->
            log:write(error, ?MODULE_STRING, "Failed starting flash security listener on port ~w: Reason: ~p~n", [Port, Reason]),
            {stop, Reason}
    end.
    
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
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
handle_info({'EXIT', _From, error}, State) ->
    flashSecurity:startAcceptor(State#state.serverSocket),
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
startAcceptor(ServerSocket) ->
    spawn_link(?MODULE, acceptorLoop, [ServerSocket, self()]).
    
acceptorLoop(ServerSocket, ParentPID) ->
    case gen_tcp:accept(ServerSocket) of
        {ok, Socket} ->
            {RemoteAddress, RemotePort} = case inet:peername(Socket) of
                                  {ok, {Address1, Port1}} -> {Address1, Port1};
                                  {error, Error} -> {unknown, Error}
                              end,
            log:write(info, ?MODULE_STRING, "Accepted flash security socket from ~120p:~10000p~n", [RemoteAddress, RemotePort]),
            spawn_link(?MODULE, readerListen, [Socket]),
            flashSecurity:acceptorLoop(ServerSocket, ParentPID);
        {error, Reason} ->
            log:write(error, ?MODULE_STRING, "Error accepting flash security connection: ~w~n", [Reason]);
        Other -> 
            log:write(error, ?MODULE_STRING, "Unexpected result for flash security: ~w~n", [Other])
    end.
    
readerListen(Socket) ->
    log:write(debug, ?MODULE_STRING, "Started flash security reader~n", []),
    Request = gen_tcp:recv(Socket, 0, 20000),
    Answer = ?CROSS_DOMAIN_ANSWER ++ [0],
    log:write(debug, ?MODULE_STRING, "Received flash security request: ~w~n", [Request]),
    SendResult = gen_tcp:send(Socket, Answer),
    log:write(debug, ?MODULE_STRING, "Sent flash security answer with result:~10000p~n~w~n", [SendResult, Answer]),
    WaitResult = gen_tcp:recv(Socket, 0, 20000),
    gen_tcp:close(Socket),
    log:write(debug, ?MODULE_STRING, "Closed flash security (~10000p)~n", [WaitResult]).
    