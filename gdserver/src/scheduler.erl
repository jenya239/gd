-module(scheduler).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([onBattleEnd/1]).

-include("data.hrl").
-include("config.hrl").
        
-record(state, {tasks}).    
        
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:cast(?MODULE, stop).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    ScheduledTask = case mnesia:dirty_read({scheduledTask, dailyBattleEnd}) of
        [] ->
            NewTask = #scheduledTask{id=dailyBattleEnd, dateTime=getNextDailyBattleEnd()},
            mnesia:dirty_write(NewTask),
            NewTask;
        [ScheduledTask_] ->
            ScheduledTask_        
    end,
    self() ! timer,
    timer:send_interval(60*1000, timer),
    {ok, #state{tasks = dict:from_list([{dailyBattleEnd, ScheduledTask}])}}.

%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
  
handle_call(_Message, _From, State) ->    
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop, shutdown, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------

handle_info(timer, State) ->
    Task = dict:fetch(dailyBattleEnd, State#state.tasks),
    Now = calendar:universal_time(),
    if Now > Task#scheduledTask.dateTime ->
        NextBattleEndRecord = #scheduledTask{id=dailyBattleEnd, dateTime=getNextDailyBattleEnd()},        
        mnesia:dirty_write(NextBattleEndRecord),
        NewTasks = dict:store(dailyBattleEnd, NextBattleEndRecord, State#state.tasks),
        {Date, _Time} = removeDay(Task#scheduledTask.dateTime),
        onBattleEnd(Date),
        {noreply, State#state{tasks=NewTasks}};
    true -> 
        {noreply, State}
    end;

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

getNextDailyBattleEnd() ->
    Now = {NowDate, _} = calendar:universal_time(),
    EndHour = dbGlobal:get(dailyBattleEndHour),
    TodayBattleEnd = {NowDate, {EndHour, 0, 0}},
    if Now < TodayBattleEnd ->
        TodayBattleEnd;
    true ->
        addDay(TodayBattleEnd)
    end.        
    
addDay(DateTime) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime)+ 24*60*60).

removeDay(DateTime) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime)- 24*60*60).

onBattleEnd(Date) ->
    
    UserScores = dbUser:getTopUserDailyScores(),
    CityScores = dbCity:getScores(),
    mnesia:dirty_write(#scoreHistory{date=Date, table={CityScores, UserScores}}),

    { { Y, M, D }, _} = utils:subtractDays( calendar:local_time(), 1 ),
    os:cmd( io_lib:format( "wget -qO- http://62.109.8.97/stats/war.yaws > /home/user/gdserver/src/web/stats/war/war-~4..0B-~2..0B-~2..0B.html ", [Y, M, D] ) ),
    os:cmd( io_lib:format( "wget -qO- http://62.109.8.97/stats/war.yaws | /home/user/sendEmail-v1.56/sendEmail -f a.evgeniy@rambler.ru -u 'gd war ~4..0B-~2..0B-~2B' -t artemmoskvin86@gmail.com -s mail.rambler.ru:587 -xu a.evgeniy -xp evgeniypiter", [Y, M, D] ) ),

    {atomic, ok} = mnesia:transaction(fun() ->
        dbCity:resetScore_nt(1),
        dbCity:resetScore_nt(2)
    end),    
    mnesia:clear_table(userDailyScore),
    queryCache:invalidate(userTopDailyScores).

