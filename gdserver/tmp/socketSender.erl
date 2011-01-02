-module(socketSender).

-behaviour(gen_server).

-export([start/2]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-include("config.hrl").
    
-record(state, {socket}).

start(Socket, ClientID) ->
    ProcessName = list_to_atom(?MODULE_STRING ++ integer_to_list(ClientID)),
    gen_server:start({local, ProcessName}, ?MODULE, [Socket], []).
    
%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([Socket]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    process_flag(trap_exit, true),
    {ok, #state{socket=Socket}}.
              
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

handle_info({otherCarState, _CarState, Time, _ClientID} = Message, State) ->
    Now = utils:now(),
    if Now < Time + ?CARSTATE_DROP_TIMEOUT ->
        sendMessageToSocket(State, Message);
    true -> ok end,
    {noreply, State};

handle_info(Message, State) ->
    sendMessageToSocket(State, Message),
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
sendMessageToSocket(State, Message) ->
    SerializedMessage = protocol:serialize(Message),
    %log:writeMessage(debug, ?MODULE_STRING, State#state.clientID, State#state.name, Message, "--> "),
    %systemStats:track_Message(getMessageName(Message), length(SerializedMessage) ),
    case catch gen_tcp:send(State#state.socket, SerializedMessage) of
        ok -> 
            ok;
        {error, _Reason} -> 
            exit(normal);
        Exception ->
            log:write("sendMessageToSocket thown: ~p", [Exception])
    end.

% do not remove function
% it used in traffic stat
% getMessageName(Tuple) ->
%    if element(1, Tuple) =:= get ->
%        lists:flatten(io_lib:format("~w", [element(2, Tuple)]));
%    true ->
%        lists:flatten(io_lib:format("~w",[element(1, Tuple)]))
%    end.

