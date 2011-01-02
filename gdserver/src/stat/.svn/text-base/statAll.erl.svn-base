-module(statAll).

-compile(export_all).


-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Milliseconds
endStatDate() -> 1264*1000000000 + 419963*1000 + 249.
startStatDate()-> endStatDate() - 3 * 7 * 24 * 3600 * 1000.


-record(
    userTemp,
    {
      userID,
      currentLevel=1,
      hasPaid=false,
      lastActivityDate={2009,1,1} % {year,month,day}
    }
).

-record(
    accum,
    {
        peopleOnLevel = voteStat:tupleInit(), % 
        firstPayLevel = voteStat:tupleInit(), %{232,34543,456,665} количество игроков сделавших первый платеж на этом уровне
        payOnLevel = voteStat:tupleInit(), %{}
        kmOnLevel = voteStat:tupleInit(), %{}
        dayOnLevel = voteStat:tupleInit(), %{}
        timeOnLevel = voteStat:tupleInit(), %{}
        spendRepairItemOnLevel = voteStat:tupleInit(), %{}
        spendFuelOnLevel = voteStat:tupleInit(),  %{}
        spendRepairCarOnLevel = voteStat:tupleInit() %{}
    }
).

-record(
    activity2,
    {
        date,
        userid = none,
        action,
        result = ok
    }
).

migrateActivity() ->
   mnesia:start(),
   io:format("start migration ~n"),
   mnesia:wait_for_tables([activity,activity1,session], 1000000),
   mnesia:delete_table(activity),
   {atomic, ok} = mnesia:transform_table(activity2, ignore, record_info(fields, activity), activity),
   mnesia:delete_table(activity2),
   io:format("end migration ~n"),
   mnesia:stop().

migrateActivity0()->
   mnesia:start(),
   mnesia:wait_for_tables([activity,session], 1000000),
   mneser:createTables([
		{activity2,			[{attributes, record_info(fields, activity2)},			{disc_only_copies, [node()]}, {type, bag}]}]),
   {atomic, _ } = mnesia:transaction (
     fun() ->
           QH = qlc:q([
                     A || A <- mnesia:table(activity)
                    ]),
           io:format("start start QH processing ~n"),
           Fun = fun({activity,D,U,A,R},N) ->
               mnesia:dirty_write({activity2,D,U,A,R}),
               io:format("~w ~n",[N]),
               N+1
           end,
           qlc:fold(Fun, 0, QH)
     end ).

start() ->
   mnesia:start(),
   mnesia:wait_for_tables([activity,session], 1000000),
   io:format("start statistic ~n"),
   createTableUS(),
   %insertSessions(),
   processActivity(),
   %processUserStat(),
   deleteTableUS().

createTableUS() ->
    mneser:createTables([
		{userTemp,
      [{attributes, record_info(fields, userTemp)},
       {ram_copies,  [node()]},
       {type, set}]}]).

deleteTableUS() ->
    mnesia:delete_table(userTemp).

% add_to_tuple(N, Delta, Tuple) -> Tuple2
add_to_tuple(N, Delta, Tuple) ->
    OldValue = element(N, Tuple),
    setelement(N, Tuple, OldValue + Delta).

insertSessions() ->
    {atomic, _ } = mnesia:transaction (
     fun() ->
           QH = qlc:q([
                     A || A <- mnesia:table(session),
                     A#session.startTime < endStatDate(),
                     A#session.startTime > startStatDate()
                    ]),
           io:format("start insert sessions ~n"),
           qlc:fold(fun processSession/2, ok, QH)
     end ).

processSession({session, _ID, UserID, StartTime, EndTime, Kilometers, _WorkCounter}, ok) when is_integer(StartTime) andalso is_integer(EndTime) ->
    mnesia:write({activity, (StartTime + EndTime) / 2, UserID , {session, EndTime - StartTime, Kilometers}, ok} ) ,ok;

processSession(_, ok) -> ok.
    
processActivity() ->
    {atomic, _ } = mnesia:transaction (
     fun() ->
           QH = qlc:keysort(2, qlc:q([
                     A || A <- mnesia:table(activity),
                     A#activity.date > startStatDate()
                    ]), {order ,ascending}),
           io:format("start start QH-activity processing ~n"),
           Acc = qlc:fold(fun processRow0/2, #accum{}, QH),
           io:format("activity processed ~n~w~n",[Acc])
       end ).

processRow0(Row,Acc)->
    {activity,Date,UserID,_,_} = Row,
    Acc2 = case mnesia:read(userTemp,UserID) of
        [UserTemp] ->
             Level = UserTemp#userTemp.currentLevel,
             Timestamp = utils:microsecsToTimestamp(Date*1000),
             {CalendarDate,_} = calendar:now_to_local_time(Timestamp),
             {Days,UserTemp2} = case  UserTemp#userTemp.lastActivityDate < CalendarDate of
                 true -> {1,UserTemp#userTemp{lastActivityDate = CalendarDate}};
                 false -> {0,UserTemp}
             end,
             DayOnLevel =  Acc#accum.dayOnLevel,
             DayOnLevel2 = add_to_tuple(Level,Days,DayOnLevel),
             mnesia:write(UserTemp2),
             Acc#accum{dayOnLevel=DayOnLevel2};
        [] -> Acc
    end,
    processRow(Row,Acc2).

processRow({activity,_Date,UserID,{session,Duration,KM},ok},Accum) ->
    case mnesia:read(userTemp,UserID) of
        [UserTemp] ->
             Level = UserTemp#userTemp.currentLevel,
             TimeOnLevel = Accum#accum.timeOnLevel,
             TimeOnLevel2 = add_to_tuple(Level,Duration,TimeOnLevel),
             KmOnLevel = Accum#accum.kmOnLevel,
             KmOnLevel2 = add_to_tuple(Level,KM,KmOnLevel),
             Accum#accum{kmOnLevel=KmOnLevel2, timeOnLevel = TimeOnLevel2};
        [] -> Accum % io:format("session, user has not been found ~w ~n",[UserID]), Accum
    end;

processRow({activity,_Time,ID,register,ok},Acc) ->
    mnesia:write( #userTemp{ userID=ID} ),
    PeopleOnLevel =  Acc#accum.peopleOnLevel,
    PeopleOnLevel2 = add_to_tuple(1,1,PeopleOnLevel),
    Acc#accum{peopleOnLevel=PeopleOnLevel2};

processRow({activity,_Time,ID,{newLevel,Level},ok},Acc)->
    case mnesia:read(userTemp,ID) of
        [UserTemp] ->
             mnesia:write(UserTemp#userTemp{currentLevel=Level}),
             PeopleOnLevel =  Acc#accum.peopleOnLevel,
             PeopleOnLevel2 = add_to_tuple(Level,1,PeopleOnLevel),
             Acc#accum{peopleOnLevel=PeopleOnLevel2};
        [] -> Acc % io:format("level up, user has not been found ~w ~n",[ID]),Acc
    end;

processRow({activity,_Time,ID,{repairCar, _CarID, RealMoney, NewRealMoney },ok},Acc)->
    case mnesia:read(userTemp,ID) of
        [UserTemp] ->
             Level = UserTemp#userTemp.currentLevel,
             SpendRepairCarOnLevel = Acc#accum.spendRepairCarOnLevel,
             SpendRepairCarOnLevel2 =  add_to_tuple(Level,RealMoney - NewRealMoney,SpendRepairCarOnLevel),
             Acc#accum{spendRepairCarOnLevel = SpendRepairCarOnLevel2};
        [] -> Acc % io:format("repair car, user has not been found ~w ~n",[ID]), Acc
    end;

processRow({activity,_Time,ID,{repair, _ItemID, OldMoney, NewMoney },ok},Acc)->
    case mnesia:read(userTemp,ID) of
        [UserTemp] ->
             Level = UserTemp#userTemp.currentLevel,
             SpendRepairItemOnLevel = Acc#accum.spendRepairItemOnLevel,
             SpendRepairItemOnLevel2 =  add_to_tuple(Level,OldMoney - NewMoney,SpendRepairItemOnLevel),
             Acc#accum{spendRepairItemOnLevel = SpendRepairItemOnLevel2};
        [] -> Acc % io:format("repair item, user has not been found ~w ~n",[ID]), Acc
    end;

processRow({activity,_Time,ID, {exchangeVkontakteVotes, Votes, _Rate, _OldRealMoney, _NewRealMoney}, ok},Acc) ->
    case mnesia:read(userTemp,ID) of
        [UserTemp] ->
             io:format("yeah, vote! ~w ~n",[ID]),
             Level = UserTemp#userTemp.currentLevel,
             PayOnLevel = Acc#accum.payOnLevel,
             PayOnLevel2 =  add_to_tuple(Level,Votes,PayOnLevel),
             {FirstPay,UserTemp2} =
             case UserTemp#userTemp.hasPaid of
                 false -> {1, UserTemp#userTemp{hasPaid = true} };
                 true -> { 0, UserTemp}
             end,
             FirstPayLevel = Acc#accum.firstPayLevel,
             FirstPayLevel2 =  add_to_tuple(Level,FirstPay,FirstPayLevel),
             mnesia:write(UserTemp2),
             Acc#accum{payOnLevel = PayOnLevel2, firstPayLevel = FirstPayLevel2};
        [] -> io:format("vote, user has not been found ~w ~n",[ID]), Acc
    end;


processRow(_,A) -> A.
