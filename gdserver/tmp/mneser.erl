-module(mneser).

-compile(export_all).

-include("data.hrl").
-include("config.hrl").

-include_lib("stdlib/include/qlc.hrl").
-include("lib/eunit/include/eunit.hrl").

-export([
	deleteDatabase/0,
	createTables/1,
	waitForTables/1,
	start/0,
	transactionResult/3,
	writeRecord/1,
	checkDate/2,
	do/1,
	getAllRecords/1,
	getRecord/2,
	recordExists/2,
	getRecord_nt/2,
	delete/2,
	dbVersion/0,
	deleteRecordsList_nt/3,
	deleteRecords_nt/3,
	deleteRecordsList/3,
	deleteRecords/3,
	getAllRoutes/0,
	getWorkOffers/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%       common, service (по идее в mneser.erl должны остаться только функции из этого раздела)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%аааааа!
deleteDatabase() ->
	mnesia:delete_schema([node()]).

createTables(Tables) ->
	lists:foreach(
			fun({TableName, Params}) ->
					log:write(critical, ?MODULE_STRING, "Creating table.. ~10000p~n", [TableName]),
					%io:format("Creating table.. ~10000p~n", [TableName]),
					Result = mnesia:create_table(TableName, Params),
					log:write(critical, ?MODULE_STRING, "    ~10000p~n", [Result])
					%, io:format("    ~10000p~n", [Result])
			end,
					Tables).

waitForTables() ->
	waitForTables(60).

waitForTables(Seconds) ->
    %Tables = [uuid,activity,user,item,level,onlineStats,route,message,
    %    workOffer,advertisement,car,global,systemInfo,vapiTest,
    %    userDetails,postMessage,schema,version,session,
    %    userProgress,stopList,itemClass], %carClass
    Tables = mnesia:system_info(local_tables),
    io:format("Waiting ~p sec for tables:~n~p~n", [Seconds, Tables]),
    ok = mnesia:wait_for_tables(Tables, Seconds * 1000),
    io:format("All tables loaded~n", []).

start() ->
	mnesia:start(),
	crypto:start()
	%, waitForTables() %вызовется в миграторе
.

transactionResult(Result, UserID, Activity) ->
	case Result of
		{atomic, ok} ->
			ok;
		{atomic, R} ->
			{ok, R};
		{aborted, {Reason, Message}} = Error ->
			dbActivity:register(UserID, Activity, Error),
			{error, {Reason, Message}};
		{error, {_Reason, _Message}} = Error ->
			dbActivity:register(UserID, Activity, Error),
			Error;
		OtherResult ->
			dbActivity:register(UserID, Activity, OtherResult),
			log:write(error, ?MODULE_STRING, "~p~n", [OtherResult]),
			{error, {database, "[[databaseError]]"}}
	end.
				
writeRecord(Record) ->
	{atomic, Executed} = mnesia:transaction(fun() ->
        mnesia:write(Record),
        ok
	end),
	Executed.

checkDate(DayCount, {MegaSeconds, Seconds}) ->
	checkDate(DayCount, {MegaSeconds, Seconds, 0 });

checkDate(DayCount, {MegaSeconds, Seconds, _ }) ->
	{NowMegaSec, NowSec, _} = erlang:now(),
	NowSecs = (NowMegaSec * 1000 * 1000) + NowSec,
	PastDateSecs = NowSecs - (DayCount * 24 * 60 * 60),
	{ExpectedDate, _} = calendar:gregorian_seconds_to_datetime(PastDateSecs),
	{ActualDate, _} = calendar:gregorian_seconds_to_datetime((MegaSeconds * 1000 * 1000) + Seconds),
	%%log:write(info, ?MODULE_STRING, ActualDate ,"ActualDate ~n", []),
	%%log:write(info, ?MODULE_STRING, ExpectedDate ,"ExpectedDate ~n", []),
	ActualDate == ExpectedDate.

do(Q) ->
	{atomic, Val} =
			mnesia:transaction
			(
			fun() ->
					qlc:e(Q)
			end
			),
	Val.

getAllRecords(Table) ->
	mneser:do(qlc:q([X || X <- mnesia:table(Table)])).

getRecord_nt(Table, Id) ->
	case mnesia:read({Table, Id}) of
		[Element] -> Element;
		[] -> erlang:error({error, noId, Table, Id})
	end.

getRecord(Table, Id) ->
	{atomic, Result} = mnesia:transaction(fun() ->
        getRecord_nt(Table, Id)
    end),
    Result.

recordExists(Table, Id) ->
	Result = do(qlc:q([X || X <- mnesia:table(Table), element(2, X) =:= Id])),
	case Result of
		[_Element] -> true;
		[] -> false
	end.

delete(Table, Key) -> mnesia:transaction(fun mnesia:delete/3, [Table, Key, write]).

dbVersion() ->
	Result = mnesia:transaction( fun() -> qlc:e( qlc:q( [ X || X <- mnesia:table(version) ] ) ) end ),
	case Result of
		{atomic, [Record]} -> Record#version.number;
		{aborted, _} -> 0
	end.

deleteRecords_nt(Table, Nth, Value) ->
	Records = qlc:e(qlc:q([ X || X <- mnesia:table( Table ), element( Nth, X ) =:= Value ])),
	lists:foreach( fun( X ) -> mnesia:delete_object( X ) end, Records ).

deleteRecords(Table, Nth, Value) ->
	{atomic, Val} = mnesia:transaction(fun() ->
        deleteRecords_nt(Table, Nth, Value)
	end), Val.

deleteRecordsList_nt(Table, Nth, ValueList) ->
	Records = qlc:e(qlc:q([ X || X <- mnesia:table( Table ),
                             Y <- ValueList,
                       element( Nth, X ) =:= Y ])),
	lists:foreach( fun( X ) -> mnesia:delete_object( X ) end, Records ).

deleteRecordsList(Table, Nth, ValueList) ->
	{atomic, Val} = mnesia:transaction(fun() ->
	    deleteRecordsList_nt(Table, Nth, ValueList)
	end), Val.

rewrite(Table, NewRecords) ->
	{atomic, ok} = mnesia:clear_table(Table),
	MaxId = lists:foldl(
		fun(Record, Max) ->
			case element(2, Record) > Max of
				true -> element(2, Record);
				false -> Max
			end
		end,
		0,
		NewRecords
	),
	{atomic, ok} = mnesia:transaction(
		fun() ->
			lists:foreach(
				fun(Record) -> mnesia:write(Record)	end,
				NewRecords
			),
			mnesia:write({uuid, Table, MaxId})
		end
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%       cars, routes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


getAllRoutes() ->
	ets:tab2list(route).

getWorkOffers() ->
	{atomic, WorkOffers} = mnesia:transaction(
			fun() ->
					qlc:e(qlc:q([X || X <- mnesia:table(workOffer),
											 X#workOffer.fuel > 0.1]))
			end
	),
	WorkOffers.

groupBy_Count(Tuple, {KeyIndex, [{Key, Count} | Tail]}) when Key =:= element(KeyIndex, Tuple) ->
    [{Key, Count+1} | Tail];

groupBy_Count(Tuple, {KeyIndex, List}) ->
    {KeyIndex, [{element(KeyIndex, Tuple), 1} | List]}.

groupBy_Sum(Tuple, {KeyIndex, {ValueIndex, _ValueIndex2} = VI, [{Key, Sum, Value2} | Tail]}) 
when Key =:= element(KeyIndex, Tuple) ->
    {KeyIndex, VI, [{Key, Sum+element(ValueIndex, Tuple), Value2} | Tail]};

groupBy_Sum(Tuple, {KeyIndex, {ValueIndex, ValueIndex2} = VI, List}) ->
    {KeyIndex, VI, [{element(KeyIndex, Tuple), element(ValueIndex, Tuple), element(ValueIndex2, Tuple)  } | List]}.
