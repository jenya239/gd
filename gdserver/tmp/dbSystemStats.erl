-module(dbSystemStats).

-export([getStatsForTime/1, write/1, createJSONData/1, getForTimeRange/2, getMinuteOfDay/1, generateRandomStats/2]).

-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MILLISECONDS_IN_MINUTE, 60000).
-define(PROCESSORS_COUNT, 2).
-define(DEFAULT_MAX_MEMORY, 2*1024*1024*1024).

getStatsForTime(Time) ->
    TimeM = (Time div 1000) div ?MILLISECONDS_IN_MINUTE,
    mnesia:dirty_read({systemInfo, TimeM}).
    
write(Stats) ->
    _CheckingRecord = Stats#systemInfo.cpuLoad,
    mnesia:transaction(fun() -> 
        mnesia:write(Stats)
    end).
    
getForTimeRange(From, To) ->
    FromM = (From div 1000) div ?MILLISECONDS_IN_MINUTE,
    ToM = (To div 1000) div ?MILLISECONDS_IN_MINUTE,

    TransactionResult = mnesia:transaction(fun() -> 
    	Q = qlc:q([X || X <- mnesia:table(systemInfo), X#systemInfo.timestamp >= FromM andalso X#systemInfo.timestamp < ToM]),
    	qlc:e(qlc:sort(Q))
    end),

    case TransactionResult of
        {atomic, Stats} ->
            Stats;
        {aborted, _Reason} ->
            error
    end.
    
createJSONData(StatsList) ->
    % Эти переменные нужны для того, чтобы затем заполнить массив данных 
    % недостающими элементами (пропущенные, а также ненаступившие еще минуты)
	FirstPoint = if length(StatsList) > 0 ->
	    dbSystemStats:getMinuteOfDay(element(2, lists:nth(1, StatsList)));
	true ->
		0
	end,

    MinNumberOfValues = 1440,
    
    SkippedBeforeList = lists:foldl(fun(_E, Skipped) ->
        [#systemInfo{timestamp=0, cpuLoad=null, memUsage=null, memTotal=null} | Skipped]
    end, [], lists:seq(1, 1 + FirstPoint)),
    
    % Заполняем массив пропущенными элементами
    {_, FilledStatsList} = if length(StatsList) > 1 ->
        lists:foldl(fun(E, {LastComponent, List}) -> 
            Comp = getMinuteOfDay(E#systemInfo.timestamp),

            Diff = Comp - LastComponent,
            if Diff > 1 ->
                Missed = lists:foldl(fun(_M, Acc) -> 
                    Acc ++ [#systemInfo{timestamp=0, cpuLoad=null, memUsage=null, memTotal=null}]
                end, [], lists:seq(1, Diff-1)),
                {Comp, List ++ Missed ++ [E]};
            true ->
                {Comp, List ++ [E]}
            end        
        end, {FirstPoint, []}, StatsList);
    true ->
        {0, StatsList}
    end,

    % Дополняем массив элементами для тех моментов времени, которые еще не наступили
    NewStatsList = if length(FilledStatsList) + length(SkippedBeforeList) < MinNumberOfValues ->
        L = lists:foldl(fun(_E, Acc) -> 
            [#systemInfo{timestamp=0, cpuLoad=null, memUsage=null, memTotal=null} | Acc]
        end, [], lists:seq(1, MinNumberOfValues - length(FilledStatsList) - length(SkippedBeforeList))),
        lists:append(FilledStatsList, L);
    true ->
        FilledStatsList
    end,
    
    {MaxMemory, _, _} = memsup:get_memory_data(),

    % Создаем строку со списком элементов для столбцов
    {_Max, CPUValues, MemValues, TotalValues} = lists:foldl(fun(Stats, {MaxAcc, CPUValuesAcc, MemValuesAcc, TotalValuesAcc}) -> 
        CPUValue = case is_number(Stats#systemInfo.cpuLoad) of
            true ->
                io_lib:format("~.1f,", [Stats#systemInfo.cpuLoad * 100.0]);
            false ->
                "0.0,"
        end,
        
        MemValue = case is_number(Stats#systemInfo.memUsage) of
            true ->
                io_lib:format("{
                  \"type\": \"dot\",
                  \"value\": ~.1f,
                  \"tip\": \"~.1fMb\"
                },", [(Stats#systemInfo.memUsage / MaxMemory) * (?PROCESSORS_COUNT * 100), Stats#systemInfo.memUsage / (1024*1024)]);
            false ->
                "0.0,"
        end,

        TotalValue = case is_number(Stats#systemInfo.memTotal) of
            true ->
                io_lib:format("{
                  \"type\": \"dot\",
                  \"value\": ~.1f,
                  \"tip\": \"~.1fMb\"
                },", [(Stats#systemInfo.memTotal / MaxMemory) * (?PROCESSORS_COUNT * 100), Stats#systemInfo.memTotal / (1024*1024)]);
            false ->
                "0.0,"
        end,
        
        NewMax = if is_number(Stats#systemInfo.cpuLoad) andalso (Stats#systemInfo.cpuLoad * 100) > MaxAcc ->
            Stats#systemInfo.cpuLoad * 100;
        true ->
            MaxAcc
        end,

        {NewMax, CPUValuesAcc ++ CPUValue, MemValuesAcc ++ MemValue, TotalValuesAcc ++ TotalValue}
    end, {0.0, "", "", ""}, SkippedBeforeList ++ NewStatsList),

    SCPUValues = string:strip(CPUValues, right, $,),
    SMemValues = string:strip(MemValues, right, $,),
    STotalValues = string:strip(TotalValues, right, $,),

    io_lib:format(
    "
    {
      \"elements\": [
        {
          \"type\": \"line\",
          \"width\": 1,
          \"values\": [~s],
          \"text\": \"CPU\",
          \"colour\": \"#FF0000\",
          \"dot-style\": {
            \"type\": \"dot\",
            \"dot-size\": 4,
            \"halo-size\": 0
          },          
        },
        {
          \"type\": \"line\",
          \"width\": 1,
          \"values\": [~s],
          \"text\": \"Erlang memory\",
          \"colour\": \"#0000FF\",
          \"dot-style\": {
            \"type\": \"dot\",
            \"dot-size\": 4,
            \"halo-size\": 0
          },          
        },
        {
          \"type\": \"line\",
          \"width\": 1,
          \"values\": [~s],
          \"text\": \"Total memory\",
          \"colour\": \"#00FF00\",
          \"dot-style\": {
            \"type\": \"dot\",
            \"dot-size\": 4,
            \"halo-size\": 0
          },          
        }        
      ],
      \"title\": {
        \"text\": \"Использование процессора и памяти\"
      },
      \"bg_colour\": \"#FFFFFF\",
      \"y_axis\": {
        \"min\": 0,
        \"max\": ~b,
        \"steps\": 5
      },
      \"x_axis\": {
        \"steps\": 1
      }      
    }    
    ",
    [SCPUValues, SMemValues, STotalValues, 100 * ?PROCESSORS_COUNT]
    ).
    
getMinuteOfDay(Minutes) ->
    Timestamp = Minutes * ?MILLISECONDS_IN_MINUTE * 1000,
    {{_Year, _Month, _Day}, {Hour, Min, _Sec}} = calendar:now_to_local_time(utils:microsecsToTimestamp(Timestamp)),
    Hour * 60 + Min.
    
generateRandomStats(From, To) ->
    FromM = (utils:dateToUniversalErlangTime(From) div 1000) div ?MILLISECONDS_IN_MINUTE,
    ToM = (utils:dateToUniversalErlangTime(To) div 1000) div ?MILLISECONDS_IN_MINUTE,

    Stats = lists:foldl(fun(Minute, Acc) ->
        Cpu = (random:uniform(200) / 1000),
        Mem = 1024 * 1024 * 1024 + random:uniform(1024 * 1024 * 1024),
        S = #systemInfo{timestamp=Minute, cpuLoad=Cpu, memUsage=Mem},
        Acc ++ [S]
    end, [], lists:seq(FromM, ToM)),

    lists:foreach(fun(E) -> 
        write(E)
    end, Stats).