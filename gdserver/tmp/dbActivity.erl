-module(dbActivity).

-export([
	register/3,
	register_nt/3,
	getAll/1,
	getAll/2,
	getSales/0,
    getAllUserActivities/3
]).
        
-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

%-record(
%    activity,
%    {
%    date,
%    userid = none,
%    action,
%    result = ok
%    }).

register(UserID, Action, Result) ->
	{atomic, ok} = mnesia:transaction(fun() -> register_nt(UserID, Action, Result) end).

register_nt(UserID, Action, Result) ->
	NewActivity = #activity{userid=UserID, date=utils:erlangTime(), action=Action, result=Result},
	mnesia:write(NewActivity).

getAll(DayCount) ->
	mneser:do(qlc:q([
		{ Y#user.name, Y#user.vkontakteID }
		||
		X <- mnesia:table(activity),
		Y <- mnesia:table(user),
		X#activity.action =:= authorize,
		X#activity.userid =:= Y#user.id,
		mneser:checkDate( DayCount, X#activity.date )
	])).

getAll(StartTime, EndTime) ->
	mneser:do(qlc:keysort(2, qlc:q([
		A
		||
		A <- mnesia:table(activity),
		A#activity.date > StartTime,
		A#activity.date < EndTime
	]), {order ,descending})).

%mneser:do(qlc:q([X||X<-mnesia:table(activity),is_tuple(element(4,X)),element(1,element(4,X))=:=buyItem,element(5,X)=:=ok])).
getSales() ->
	mneser:do(qlc:keysort(1, qlc:q([
		{ A#activity.date, A#activity.action, IC, U }
		||
		A <- mnesia:table(activity),
		IC <- mnesia:table(itemClass),
		U <- mnesia:table(user),
		is_record(A#activity.action, buyItem),
		A#activity.result =:= ok,
		(A#activity.action)#buyItem.itemClassID =:= IC#itemClass.id,
		U#user.id =:= A#activity.userid
	]), {order ,descending})).

getAllUserActivities(UserId, Offset, Limit) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
        QueryHandle = qlc:q([{A#activity.date, A#activity.action, A#activity.result} || 
                            A <- mnesia:table(activity), A#activity.userid =:= UserId]),
        SortedHandle = qlc:keysort(1, QueryHandle, {order ,descending}),
        Cursor = qlc:cursor(SortedHandle),
        
        if Offset > 0 ->
            qlc:next_answers(Cursor, Offset);
        true ->
            ok
        end,
        
        PagedHandle = qlc:next_answers(Cursor, Limit),
        qlc:delete_cursor(Cursor),
        qlc:e(PagedHandle)
    end),
    Result.
    
    % mneser:do(
    %     qlc:keysort(1, qlc:q([
    %   {A#activity.date, A#activity.action, A#activity.result}
    %   ||
    %   A <- mnesia:table(activity),
    %   A#activity.userid =:= UserId
    % ]), {order ,descending})).