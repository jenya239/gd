-module(systemStats).

-behaviour(gen_server).

-include("data.hrl").

-export([start_link/0]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3,
        track_Message/2]).

-define(MONITORING_PERIOD, 60000).
        
start_link() ->    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

track_Message(MessageName,Size) ->
    gen_server:cast(?MODULE, {MessageName,Size}).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    erlang:send_after(?MONITORING_PERIOD, self(), timer),
    {ok, dict:new()}.
    
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast({MessageName,Size}, State) ->
    {noreply, dict:update(MessageName, fun (Old) -> Old + Size end, Size ,State)}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info(timer, State) ->
    Avg = cpu_sup:avg1(),
    Percentage = case Avg of 
        {error, _} ->
            0;
        Value ->
            Value / 512
    end,
    
    {_, TotalMemory, _} = memsup:get_memory_data(),
    MemUsage = erlang:memory(total),
    dbSystemStats:write(#systemInfo{timestamp=utils:now() div 60000, cpuLoad=Percentage, memUsage=MemUsage, memTotal=TotalMemory}),
    writeStat(State),
    erlang:send_after(?MONITORING_PERIOD, self(), timer),
    {noreply, dict:new()};
    
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
writeStat(State) ->
    Timestamp=utils:now() div 60000,
    Fun = fun ({Name,Size}) ->
            mneser:writeRecord(#trafficStat{time=Timestamp,name=Name,size=Size})
          end,
    lists:foreach(Fun, dict:to_list(State)).


