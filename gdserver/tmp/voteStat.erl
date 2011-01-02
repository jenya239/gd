-module(voteStat).

-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).
-record(
    userStat,
    {
    	userID,
      currentLevel=1,
      sum=0,
      payments={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
    }
).

start() ->
   mnesia:start(),
   mnesia:wait_for_tables([activity], 1000000),
   io:format("start statistic ~n"),
   createTableUS(),
   processActivity(),
   processUserStat(),
   deleteTableUS().

getLastDate() ->
   mnesia:start(),
   mnesia:wait_for_tables([activity], 1000000),
   {atomic, _ } = mnesia:transaction (
     fun() ->
           QH = qlc:keysort(2, qlc:q([
                     A || A <- mnesia:table(activity)
                    ]), {order ,descending}),
           io:format("start start QH processing getLastDate ~n"),
           Cursor = qlc:cursor(QH),
           [Row|_] = qlc:next_answers(Cursor,1),
           Microsecs = element(2, Row),
           S = utils:timestampToHumanString( utils:microsecsToTimestamp( Microsecs ) ),
           io:format("~s~n",[ S ] ),
           qlc:delete_cursor(Cursor)

           %qlc:fold(fun processRow/2, ok, QH)
     end ).

processActivity() ->
    {atomic, _ } = mnesia:transaction (
     fun() ->
           QH = qlc:keysort(2, qlc:q([
                     A || A <- mnesia:table(activity)
                    ]), {order ,ascending}),
           io:format("start start QH processing ~n"),
           qlc:fold(fun processRow/2, ok, QH)
     end ).

processRow(Row,_) ->
    processRow(Row),ok.
    
processRow({activity,_Time,ID,register,ok}) ->
    % io:format("yeah, register user! ~w ~n",[ID]),
    mnesia:write( #userStat{ userID=ID} );

processRow({activity,_Time,ID,{newLevel,Level},ok})->
    case mnesia:read(userStat,ID) of
        [UserStat] ->             
             mnesia:write(UserStat#userStat{currentLevel=Level});
        [] -> ok
    end;

processRow({activity,_Time,ID, {exchangeVkontakteVotes, Votes, _Rate, _OldRealMoney, _NewRealMoney}, ok}) ->
    case mnesia:read(userStat,ID) of
        [UserStat] ->
             io:format("yeah, vote! ~w ~n",[ID]),
             addVotes(UserStat, Votes);
        [] -> io:format("vote, user has not been found ~w ~n",[ID]), ok
    end;
processRow(_) -> ok.

addVotes(UserStat, Votes) ->
    Level = UserStat#userStat.currentLevel,
    Payments0 = UserStat#userStat.payments,
    Payments = setelement(Level, Payments0, element(Level,Payments0)+Votes),
    mnesia:write(UserStat#userStat{sum=Votes+UserStat#userStat.sum, payments=Payments}).


createTableUS() ->
    mneser:createTables([
		{userStat,
      [{attributes, record_info(fields, userStat)},
       {ram_copies,  [node()]},
       {type, set}]}]).

deleteTableUS() ->
    mnesia:delete_table(userStat).

processUserStat() ->
 Function = fun(Record,Acc) ->
     {Payments,Levels} = Acc,
     Level = Record#userStat.currentLevel,
     case Record#userStat.sum of
         0 -> Acc;
         _ -> {lists:zipwith(fun(A,B) -> A + B end,
                             Payments,
                             tuple_to_list(Record#userStat.payments)),
               setelement(Level, Levels, element(Level,Levels)+1) }
     end
 end,
 {atomic, {Payments,LevelTuple}} = mnesia:transaction(fun() ->
        mnesia:foldl(Function, {listInit(),tupleInit()}, userStat)
	end),
  {LevelsList,_} = convertLevels(LevelTuple),
  io:format("result ~w ~n",[{Payments,LevelsList}]),
  ResultList = lists:zipwith(fun(A,B) -> A / B end, Payments,LevelsList),
  io:format("final result ~w~n",[ResultList]).

listInit()->[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0].
tupleInit()->{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}.

%convertLevels(LevelTuple) -> LevelList
convertLevels(LevelTuple) ->
    LevelList0 = tuple_to_list(LevelTuple),
    lists:mapfoldr( fun(A,B) -> {A+B,A+B} end,
                    1,LevelList0).







