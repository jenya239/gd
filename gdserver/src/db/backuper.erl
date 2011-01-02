-module(backuper).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
start_link(BackUpPeriod) ->    
    gen_server:start_link(?MODULE, [BackUpPeriod], []).
    
%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([BackUpPeriod | _Args]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    timer:send_after(BackUpPeriod, self(), timer),
    
    {ok, BackUpPeriod}.
    
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
%% Handle calls
%%-----------------------------------------------------------------------------
handle_info(timer, State) ->
    {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
    Filename = [integer_to_list(Year) ++ integer_to_list(Month) ++ integer_to_list(Day) ++ integer_to_list(Hour) ++ integer_to_list(Minute) ++ integer_to_list(Second) ++ "-mnesia.backup"],
    mnesia:backup("backup/" ++ Filename),
    log:write(info, ?MODULE_STRING, "backup has been created in file: ~s~n", [Filename]),
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
    
% 
% start(BackUpPeriod)->
%     spawn(fun() -> run(BackUpPeriod) end).
% 
% run(BackUpPeriod) ->
%     {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
%   Filename = [integer_to_list(Year) ++ integer_to_list(Month) ++ integer_to_list(Day) ++ integer_to_list(Hour) ++ integer_to_list(Minute) ++ integer_to_list(Second) ++ "-mnesia.backup"],
%   mnesia:backup("backup/"++Filename),
%   log:write(critical, ?MODULE_STRING, "backup was created in file: ~s~n", [Filename]),
%   timer:sleep(BackUpPeriod),
%   run(BackUpPeriod).
