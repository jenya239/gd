-module(dbOnlineStats).

-export([getOnlineUsersStats/3, write/1, createTable/1, createJSONData/2, generateRandomStats/2, 
        getTimeComponent/2, getStatsForTime/1, updateStatsForTime/3]).

-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").

groupByDate(Stats, [Head | Tail] = List) ->
    STimestamp = Stats#onlineStats.timestamp * ?MILLISECONDS_IN_HOUR * 1000,
    HTimestamp = Head#onlineStats.timestamp * ?MILLISECONDS_IN_HOUR * 1000,
    
    {SDate, _STime} = calendar:now_to_local_time(utils:microsecsToTimestamp(STimestamp)),
    {HDate, _HTime} = calendar:now_to_local_time(utils:microsecsToTimestamp(HTimestamp)),
    
    case SDate =:= HDate of
        true ->
            Min = if Stats#onlineStats.min < Head#onlineStats.min ->
                Stats#onlineStats.min;
            true ->
                Head#onlineStats.min
            end,
    
            Max = if Stats#onlineStats.max > Head#onlineStats.max ->
                Stats#onlineStats.max;
            true ->
                Head#onlineStats.max
            end,    
    
            [Stats#onlineStats{min=Min, max=Max} | Tail];
        false ->
            [Stats | List]
    end;
    
groupByDate(Stats, []) ->
    [Stats].    

getOnlineUsersStats(From, To, Type) ->
    FromH = (From div 1000) div ?MILLISECONDS_IN_HOUR,
    ToH = (To div 1000) div ?MILLISECONDS_IN_HOUR,
    
    TransactionResult = mnesia:transaction(fun() -> 
    	Q = qlc:q([X || X <- mnesia:table(onlineStats), X#onlineStats.timestamp >= FromH andalso X#onlineStats.timestamp < ToH]),
    	qlc:e(qlc:sort(Q))    	
    end),
    
    Stats = case TransactionResult of
        {atomic, Stats1} ->
            Stats1;
        {aborted, _Reason} ->
            error
    end,
    
    case Type of 
        daily ->
            Stats;
        monthly ->
            lists:reverse(lists:foldl(fun groupByDate/2, [], Stats))
    end.
    
getStatsForTime(Time) ->
    TimeH = Time div ?MILLISECONDS_IN_HOUR,
    TransactionResult = mnesia:transaction(fun() -> 
    	qlc:e(qlc:q([X || X <- mnesia:table(onlineStats), X#onlineStats.timestamp =:= TimeH]))
    end),
    
    case TransactionResult of
        {atomic, Stats1} ->
            case Stats1 of
                [Element] ->
                    Element;
                _Other ->
                    error
            end;
        {aborted, _Reason} ->
            error
    end.
    
write(Stats) ->
    _Counter = Stats#onlineStats.max,
    mnesia:transaction(fun() -> 
        mnesia:write(Stats)
    end).
    
updateStatsForTime({Date, Time}, Max, Min) ->
    Hour = (utils:dateToUniversalErlangTime(Date, Time) div 1000) div ?MILLISECONDS_IN_HOUR,
    Stats = #onlineStats{timestamp=Hour, max=Max, min=Min},
    dbOnlineStats:write(Stats).
    
createTable(StatsList) ->
    Rows = lists:foldl(
        fun(Stat, Agg) ->
			TS = Stat#onlineStats.timestamp * ?MILLISECONDS_IN_HOUR * 1000,
			Min = Stat#onlineStats.min,
			Max = Stat#onlineStats.max,
            Row = {tr, [], [{td, [], utils:timestampToHumanString(utils:microsecsToTimestamp(TS))},
							{td, [], io_lib:format("~w", [Min])},
							{td, [], io_lib:format("~w", [Max])}]},
            [Row | Agg]
        end, [], StatsList), 
    {table, [{border, "1"}], Rows}.
    
createJSONData(StatsList, Type) ->
    % Эти переменные нужны для того, чтобы затем заполнить массив данных 
    % недостающими элементами (пропущенные, а также ненаступившие еще дни и часы)
    {ForToday, Component} = case Type of
        daily ->
            StatsDay = if length(StatsList) > 0 ->
                dbOnlineStats:getTimeComponent(day, element(2, lists:nth(1, StatsList)));
            true ->
                0
            end,
            
            NowDay = dbOnlineStats:getTimeComponent(day, utils:now() div ?MILLISECONDS_IN_HOUR),
            {StatsDay =:= NowDay, hour};
        monthly ->
            {false, day}
    end,    
    
	FirstPoint = if length(StatsList) > 0 ->
        dbOnlineStats:getTimeComponent(Component, element(2, lists:nth(1, StatsList)));
	true ->
		0
	end,
    
    MinNumberOfValues = case Type of
        daily ->
            24;
        monthly ->
            29
    end,
    
    MinNumberOfValues1 = case MinNumberOfValues > FirstPoint of
        true ->
            MinNumberOfValues - FirstPoint;
        false ->
            0
    end,
        
    % Заполняем массив пропущенными элементами
    {_, FilledStatsList} = lists:foldl(fun(E, {LastComponent, List}) -> 
        Comp = getTimeComponent(Component, E#onlineStats.timestamp),
        
        Diff = Comp - LastComponent,
        if Diff > 1 ->
            Missed = lists:foldl(fun(_M, Acc) -> 
                Acc ++ [#onlineStats{timestamp=0, min=null, max=null}]
            end, [], lists:seq(1, Diff-1)),
            {Comp, List ++ Missed ++ [E]};
        true ->
            {Comp, List ++ [E]}
        end        
    end, {FirstPoint, []}, StatsList),
    
    % Дополняем массив элементами для тех моментов времени, которые еще не наступили
    NewStatsList = if length(StatsList) < MinNumberOfValues1 ->
        L = lists:foldl(fun(_E, Acc) -> 
            [#onlineStats{timestamp=0, min=null, max=null} | Acc]
        end, [], lists:seq(1, MinNumberOfValues1 - length(FilledStatsList))),
        lists:append(FilledStatsList, L);
    true ->
        FilledStatsList
    end,
    
    % Создаем строку со списком элементов для столбцов
    {Max, Values} = lists:foldl(fun(Stats, {MaxAcc, ValuesAcc}) -> 
        Value = case is_number(Stats#onlineStats.max) of
            true ->
                io_lib:format("{\"top\":~w, \"bottom\":~w},", [Stats#onlineStats.max, Stats#onlineStats.min]);
            false ->
                "null,"
        end,
        
        NewMax = if is_number(Stats#onlineStats.max) andalso Stats#onlineStats.max > MaxAcc ->
            Stats#onlineStats.max;
        true ->
            MaxAcc
        end,
        
        {NewMax, ValuesAcc ++ Value}
    end, {0, ""}, NewStatsList),
    
    SValues = string:strip(Values, right, $,),
        
    {_, Labels} = lists:foldl(fun(Stats, {LastComponent, LabelsAcc}) -> 
        {NewLastComponent, DayOfWeek} = if Stats#onlineStats.timestamp > 0 ->
            DOW = case Type of
                daily ->
                    "";
                monthly ->
                    io_lib:format("<br>(~s)", [getDayOfWeek(Stats#onlineStats.timestamp)])
            end,
            
            {getTimeComponent(Component, Stats#onlineStats.timestamp), DOW};
        true ->
            {LastComponent + 1, ""}
        end,
        
        Label = io_lib:format("\"~3b~s\",", [NewLastComponent, DayOfWeek]),
        {NewLastComponent, LabelsAcc ++ Label}
    end, {FirstPoint, ""}, NewStatsList),
    
    SLabels = string:strip(Labels, right, $,),
    
    FirstDateTS = if length(StatsList) > 0 ->
        [First | _Tail] = StatsList,
        First#onlineStats.timestamp * ?MILLISECONDS_IN_HOUR * 1000;
    true ->
        utils:now() * 1000
    end,
    
    {{Year, Month, Day}, _Time} = calendar:now_to_local_time(utils:microsecsToTimestamp(FirstDateTS)),
    
    DateString = case Type of
        daily ->
            io_lib:format("~b/~b/~b", [Year, Month, Day]);
        monthly ->
            io_lib:format("~s ~b", [utils:monthToString(Month), Year])
    end,
    
    OnClick = case Type of
        daily ->
            "";
        monthly ->
            ", \"on-click\": \"on_click\""
    end,
    
    CO = cityManager:countOnlineUsers(),
    CurrentOnlineValues = lists:foldl(fun(_E, List) -> 
        List ++ io_lib:format("~b,", [CO])
    end, "", NewStatsList),
    
    SCO = string:strip(CurrentOnlineValues, right, $,),
    
    LineJSON = case Type of
        daily when ForToday ->
            io_lib:format(
            "
            {
              \"type\": \"line\",
              \"width\": 1,
              \"text\": \"Current online - [~b]\",
              \"colour\": \"#00AA00\",
              \"values\": [~s]
            },      
            ", [CO, SCO]);
        _Other ->
            ""
    end,
        
    io_lib:format(
    "
    {
      \"elements\": [
        ~s
        {
          \"type\": \"bar_filled\",
          \"values\": [~s],
          \"colour\": \"#FFEF3F\",
          \"tip\": \"#x_label#<br>#bottom# - #top#\"~s
        }
      ],
      \"title\": {
        \"text\": \"Статистика за ~s\"
      },
      \"bg_colour\": \"#FFFFFF\",
      \"y_axis\": {
        \"min\": 0,
        \"max\": ~b,
        \"steps\": 5
      },
      \"x_axis\": {
        \"labels\": {
          \"labels\": [~s]
        }
      },
      \"tooltip\": {
        \"mouse\": 2
      }
    }    
    ",
    [LineJSON, SValues, OnClick, DateString, Max + 10, SLabels]
    ).
    
getTimeComponent(Component, Hours) ->
    Timestamp = Hours * ?MILLISECONDS_IN_HOUR * 1000,
    {{Year, Month, Day}, {Hour, _Min, _Sec}} = calendar:now_to_local_time(utils:microsecsToTimestamp(Timestamp)),
    case Component of
        hour ->
            Hour;
        day ->
            Day;
        month ->
            Month;
        year ->
            Year
    end.
    
getDayOfWeek(Hours) ->
    Timestamp = Hours * ?MILLISECONDS_IN_HOUR * 1000,
    {Date, _} = calendar:now_to_local_time(utils:microsecsToTimestamp(Timestamp)),
    DOW = calendar:day_of_the_week(Date),
    case DOW of
        1 ->
            "Пн";
        2 ->
            "Вт";
        3 ->
            "Ср";
        4 ->
            "Чт";
        5 ->
            "Пт";
        6 ->
            "Сб";
        7 ->
            "Вс"
    end.
        
    
generateRandomStats(From, To) ->
    FromH = (utils:dateToErlangTime(From) div 1000) div ?MILLISECONDS_IN_HOUR,
    ToH = (utils:dateToErlangTime(To) div 1000) div ?MILLISECONDS_IN_HOUR,
    
    Stats = lists:foldl(fun(Hour, Acc) ->
        Min = random:uniform(5000),
        Max = Min + random:uniform(5000),
        S = #onlineStats{timestamp=Hour, min=Min, max=Max},
        Acc ++ [S]
    end, [], lists:seq(FromH, ToH)),
    
    lists:foreach(fun(E) -> 
        write(E)
    end, Stats).
    
    