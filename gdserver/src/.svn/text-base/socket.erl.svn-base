-module(socket).

-behaviour(gen_server).

-export([start/3]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-include("config.hrl").
    
-export([startReceiver/3, receiverLoop/3]).

-record(state, {name, socket, clientID, clientPID}).
        
start(Socket, ClientID, ClientPID) ->
    ProcessName = list_to_atom(?MODULE_STRING ++ integer_to_list(ClientID)),
    gen_server:start({local, ProcessName}, ?MODULE, [Socket, ClientID, ClientPID], []).
    
%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([Socket, ClientID, ClientPID]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    process_flag(trap_exit, true),
    
    socket:startReceiver(Socket, ClientID, ClientPID),
    {ok, #state{socket=Socket, clientID = ClientID, clientPID = ClientPID}}.
        
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
handle_info({'EXIT', _From, _Reason}, State) ->
    {stop, normal, State};
    
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
    log:write(info, ?MODULE_STRING, State#state.clientID, "closed: ~p~n", [Reason]),
    State#state.clientPID ! {socketClosed, Reason},
    ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
startReceiver(Socket, ClientID, ClientPID) ->
    spawn_link(?MODULE, receiverLoop, [Socket, ClientID, ClientPID]).
    
receiverLoop(Socket, ClientID, ClientPID) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            logIfXml(debug, ClientID, Data),
            Message = protocol:deserialize(Data),
            ClientPID ! Message,
            socket:receiverLoop(Socket, ClientID, ClientPID);
        {error, Reason} ->
            exit({normal, Reason})
    end.
    
logIfXml(Level, ClientID, Data) ->
    case Data of 
        [0, 0, 0, 128 | Xml] ->
            case utils:containsPrefix(Xml, ?XML_FILTER) of
                false ->
                    log:write(Level, ?MODULE_STRING, ClientID, "~s~n", [Xml]);
                true ->
                    ok
            end;
        _Other ->
            ok
    end.
    