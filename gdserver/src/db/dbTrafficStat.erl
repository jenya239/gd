-module(dbTrafficStat).

-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([getStatsForTime/1, createJSONData/1, getForTimeRange/2, getMinuteOfDay/1]).

-define(MILLISECONDS_IN_MINUTE, 60000).
-define(PROCESSORS_COUNT, 2).
-define(DEFAULT_MAX_MEMORY, 2*1024*1024*1024).

createJSONData(StatsList) ->
    % Эти переменные нужны для того, чтобы затем заполнить массив данных
    % недостающими элементами (пропущенные, а также ненаступившие еще минуты)
	{Day,FirstPoint} = if length(StatsList) > 0 ->
          Minutes = element(2, lists:nth(1, StatsList)),
          FP = getMinuteOfDay(Minutes),
          {Minutes-FP,FP};
	true ->
		{0,0}
	end,

    MinNumberOfValues = 1440,

    SkippedBeforeList = 
        lists:foldl(
          fun(E, Skipped) ->
            [#trafficStat{time=Day+E, name=name,size=0} | Skipped]
          end,
              [], lists:reverse(lists:seq(0, FirstPoint) ) ),

    % Заполняем массив пропущенными элементами
    {_, FilledStatsList} =
    if length(StatsList) > 1 ->
        lists:foldl(fun(E, {LastComponent, List}) ->
            Comp = getMinuteOfDay(E#trafficStat.time),

            Diff = Comp - LastComponent,
            if Diff > 1 ->
                Missed = lists:foldl(fun(M, Acc) ->
                    Acc ++ [#trafficStat{time=LastComponent + M, name=null, size=0}]
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
        L = lists:foldl(fun(E, Acc) ->
            [#trafficStat{time=Day+E,name=name,size=0} | Acc]
        end, [], lists:reverse(lists:seq(1 + length(FilledStatsList) + length(SkippedBeforeList), MinNumberOfValues) ) ),
        lists:append(FilledStatsList, L);
    true ->
        FilledStatsList
    end,

    % Создаем строку со списком элементов для столбцов
    {MaxTraffic, CarState, AllTraff, TotalValues} =
        lists:foldl(fun(Stats, {MaxAcc, CarStateAcc, AllAcc, TotalValuesAcc}) ->
        CarState1 = case is_number(Stats#trafficStat.size) of
            true ->
                io_lib:format("~w,", [Stats#trafficStat.size]);
            false ->
                "0,"
        end,

        {atomic, AllStat} = mnesia:transaction(
            fun() ->
    	        qlc:e(qlc:q([X#trafficStat.size ||
                      X <- mnesia:table(trafficStat),
                      X#trafficStat.time =:= Stats#trafficStat.time ]
                         )
                   )
            end),

        AllCurr = lists:sum(AllStat),
        AllCurr1 = io_lib:format("~w,", [AllCurr]),


        NewMax = utils:max(MaxAcc,AllCurr),

        {NewMax, CarStateAcc ++ CarState1, AllAcc ++ AllCurr1, TotalValuesAcc ++ []}
    end, {0, "", "", ""}, SkippedBeforeList ++ NewStatsList),

    SCarState = string:strip(CarState, right, $,),
    SAll = string:strip(AllTraff, right, $,),
    _STotalValues = string:strip(TotalValues, right, $,),

    io_lib:format(
    "
    {
      \"elements\": [
        {
          \"type\": \"line\",
          \"width\": 1,
          \"values\": [~s],
          \"text\": \"ALL\",
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
          \"text\": \"CarState\",
          \"colour\": \"#0000FF\",
          \"dot-style\": {
            \"type\": \"dot\",
            \"dot-size\": 4,
            \"halo-size\": 0
          },
        }
      ],
      \"title\": {
        \"text\": \"исходящий трафик\"
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
    [SAll, SCarState,  MaxTraffic]
    ).

getStatsForTime(Time) ->
    TimeM = (Time div 1000) div ?MILLISECONDS_IN_MINUTE,
    mnesia:dirty_read({trafficStat, TimeM}).
    
getForTimeRange(From, To) ->
    FromM = (From div 1000) div ?MILLISECONDS_IN_MINUTE,
    ToM = (To div 1000) div ?MILLISECONDS_IN_MINUTE,

    TransactionResult = mnesia:transaction(fun() -> 
    	Q = qlc:q([X || X <- mnesia:table(trafficStat), 
                      X#trafficStat.name=:= "otherCarState",
                      X#trafficStat.time >= FromM,
                      X#trafficStat.time < ToM]),
    	qlc:e(qlc:sort(Q))
    end),

    case TransactionResult of
        {atomic, Stats} ->
            Stats;
        {aborted, _Reason} ->
            error
    end.

getMinuteOfDay(Minutes) ->
    Timestamp = Minutes * ?MILLISECONDS_IN_MINUTE * 1000,
    {{_Year, _Month, _Day}, {Hour, Min, _Sec}} = calendar:now_to_local_time(utils:microsecsToTimestamp(Timestamp)),
    Hour * 60 + Min.