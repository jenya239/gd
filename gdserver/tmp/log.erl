-module(log).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
    
-include("config.hrl").    
-include_lib("kernel/include/file.hrl").
-include("lib/eunit/include/eunit.hrl").
    
-export([write/2, write/3, write/6, write/5, write/4, writeMessage/6, changeTagsFilter/1, 
        changeMessagesFilter/1]).

-record(state, {name, io_device = standard_io, tags_filter = [idle], 
                messages_filter = ?MESSAGE_FILTER, file_name, postfix_count = 0}).

start_link(BasePath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BasePath], []).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([BasePath | _Args]) ->    
    if 
        BasePath == "" ->
            {ok, #state{}};
        true ->
            Filename = attachTimestamp(BasePath),
            case openNextFile(Filename, 0) of
                {ok, IoDevice, NewPostfixCount} ->
                    io:format("Logging has been started.~n", []),
                    {ok, #state{io_device=IoDevice, file_name=Filename, postfix_count=NewPostfixCount}};
                {error, Reason} ->
                    {stop, {error, Reason}}
            end
    end.
    
write(Text, Params) ->
    gen_server:cast(?MODULE, {write, self(), mark, none, undefined, "", Text, Params}).

write(Tag, Text, Params) ->
    write(Tag, none, Text, Params).

write(_Tag, _Module, _ID, _StateName, _Text, _Params) ->
    ?LOG_WRITE(gen_server:cast(?MODULE, {write, self(), _Tag, _Module, _ID, atom_to_list(_StateName), _Text, _Params})).

write(_Tag, _Module, _ID, _Text, _Params) ->    
    ?LOG_WRITE(gen_server:cast(?MODULE, {write, self(), _Tag, _Module, _ID, "", _Text, _Params})).

write(_Tag, _Module, _Text, _Params) ->    
    ?LOG_WRITE(gen_server:cast(?MODULE, {write, self(), _Tag, _Module, undefined, "", _Text, _Params})).

writeMessage(_Tag, _Module, _ID, _StateName, _Message, _Prefix) ->
    ?LOG_WRITE(gen_server:cast(?MODULE, {writeMessage, self(), _Tag, _Module, _ID, atom_to_list(_StateName), _Message, _Prefix})).

changeTagsFilter(Filter) ->
    gen_server:cast(?MODULE, {changeTagsFilter, Filter}).

changeMessagesFilter(Filter) ->
    gen_server:cast(?MODULE, {changeMessagesFilter, Filter}).

%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(_Anything, _From, State) ->
    {noreply, State}.
    
%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast({write, From, Tag, Module, ID, StateName, Text, Params}, State) ->
    {_, NewState} = case utils:inList(Tag, State#state.tags_filter) of
        false ->
            writeInternal(Tag, Module, ID, From, StateName, Text, Params, State);
        true ->
            {ok, State}
    end,
    
    {noreply, NewState};   
    
handle_cast({writeMessage, From, Tag, Module, ID, StateName, Message, Prefix}, State) ->
    {_, NewState} = case utils:inList(Tag, State#state.tags_filter) of
        false ->
            Signature = if 
                element(1, Message) =:= get ->
                    list_to_tuple([element(1, Message), element(2, Message)]);
                true ->
                    element(1, Message)
            end,

            case utils:inList(Signature, State#state.messages_filter) of
                false ->
                    writeInternal(Tag, Module, ID, From, StateName, 
                        "~s~10000p~n", [Prefix, Message], State);
                true ->
                    {ok, State}
            end;
        true ->
            {ok, State}
    end,
    
    {noreply, NewState};
    
handle_cast({changeTagsFilter, Filter}, State) ->
    NewState = State#state{tags_filter = Filter},

    {noreply, NewState};
    
handle_cast({changeMessagesFilter, Filter}, State) ->
    NewState = State#state{messages_filter = Filter},

    {noreply, NewState};
    
handle_cast(stop, State) ->
    {stop, normal, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info(_Anything, _State) ->
    ok.

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
%% Private
%%-----------------------------------------------------------------------------
writeInternal(Text, Params, State) ->
    {Reply, NewState} = case State#state.io_device =/= standard_io of
        true ->
            Filename = attachPostfixCount(State#state.file_name, State#state.postfix_count),
            case file:read_file_info(Filename) of
                {ok, Fileinfo} when Fileinfo#file_info.size > ?MAX_LOG_SIZE ->                    
                    case openNextFile(State#state.file_name, State#state.postfix_count) of
                        {ok, IoDevice, NewPostfixCount} ->
                            {ok, State#state{io_device=IoDevice, postfix_count=NewPostfixCount}};
                        {error, Reason} ->
                            {{error, Reason}, State}
                    end;
                _Anything ->
                    {ok, State}
            end;            
        false ->
            {ok, State}
    end,
    
    io:format(State#state.io_device, "~s " ++ Text, [utils:nowString() | Params]),
    
    {Reply, NewState}.

writeInternal(Tag, Module, ID, PID, StateName, Text, Params, State) ->
    {Reply, NewState} = if ID =/= undefined ->
            writeInternal(" [~s] (~p) ~s ~w, ~s: " ++ Text,
                [atom_to_list(Tag), ID, Module, PID, StateName] ++ Params, State);
        true ->
            writeInternal(" [~s] ~s ~w, ~s: " ++ Text, 
                [atom_to_list(Tag), Module, PID, StateName] ++ Params, State)
    end,
    
    {Reply, NewState}.
    
attachPostfixCount(Filename, PostfixCount) ->
    Extension = filename:extension(Filename),
    Rootname = filename:rootname(Filename),    
    Rootname ++ "_" ++ integer_to_list(PostfixCount) ++ Extension.
    
attachTimestamp(Filename) ->
    Extension = filename:extension(Filename),
    Rootname = filename:rootname(Filename),    
    Rootname ++ "_" ++ utils:timestampString() ++ Extension.    
    
openNextFile(Filename, PostfixCount) ->
    NewPostfixCount = PostfixCount+1,
    NewFilename = attachPostfixCount(Filename, NewPostfixCount),
    
    case file:open(NewFilename, [read, write]) of
        {ok, IoDevice} ->
            {ok, IoDevice, NewPostfixCount};
        {error, Reason} ->
            {error, Reason}
    end.    
    
    
    
    

