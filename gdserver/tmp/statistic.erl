-module(statistic).

-export([ userTable/1,
         generateStatJS/1,
         generateStatJS1/1,
         calcUserStat/1,
         authCount/1,
         getMessages/1,
         getResultsByDate/1]).

-include("data.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

generateStatJS1(_Arg) ->
  Data = "$(document).ready(function(){$for(var el in document.getElementsByTagName('table')) {el.tablesorter({sortList:[[0,0]]});}}",
  {script,[{type,"text/javascript"}],Data}.

generateStatJS(_Arg) ->
    List = [ {X,Y} || Y <- [0,1,2,3,4,5,6] , X <- [auth] ],
    F = fun ({X,Y}) ->
       Data = lists:flatten(io_lib:format("$(document).ready(function(){$(\"#~p~p\").tablesorter({sortList:[[0,0]]});});",[X,Y])),
       {script,[{type,"text/javascript"}],Data}
    end,
    lists:map(F,List).

authCount(_Arg) ->
    %Activity = getStatisticHtml(fun dbActivity:getAll/1,true),
    Activity = getStatisticHtml(fun statistic:calcUserStat/1,true),
    Registry = getStatisticHtml(fun dbUser:getRegUsersByDate/1,false),
    createHtmlFromTupleList(lists:zip(Activity,Registry),innerHTML).

getHeader(ShowCount) ->
    if ShowCount ->
        {name,time,route,auth};
       true -> {name}
    end.

getTableID(ShowCount,Day) ->
   Atom =
     if ShowCount ->
        auth;
       true ->
         reg
     end,
   list_to_atom( ( atom_to_list(Atom) ++ integer_to_list(Day) ) ).

getStatisticHtml(F,ShowCount) ->
    lists:map( fun(Day) ->
                   F1 = F(Day),
                   Activity = if ShowCount ->
                                   lists:map(fun({Name,Time,C,D}) -> {createVkontakteLink(Name),utils:sec_to_string(Time div 1000),C,D} end, F1); % utils:countElem( F1 )); %
                                 true ->lists:map(fun(E) -> {createVkontakteLink(E)} end,   F1)
                               end,
                   [integer_to_list(Day) ++ " days ago, total - "
                       ++ integer_to_list(length(Activity)),
                    createHtmlFromTupleList(header, Activity, getHeader(ShowCount),getTableID(ShowCount,Day) ) ]
               end, [0,1,2,3,4,5,6]).

getMessages(Arg) ->
    case (Arg#arg.req)#http_request.method of
        'POST' -> Vars = yaws_api:parse_post(Arg),
                  {value, {_,Message}} = lists:keysearch("message", 1, Vars),
                  Ucs = Message, %xmerl_ucs:from_utf8(Message),
                  dbAdvertisement:put(Ucs);
        _Other -> ok
    end,
    Messages = dbAdvertisement:getAll(),
    createHtmlFromTupleList(lists:reverse(Messages)).

createVkontakteLink({Name,0}) -> cutName(Name);
createVkontakteLink({Name,Id}) ->
    Text = "http://vkontakte.ru/id" ++ integer_to_list(Id),
    lists:flatten(io_lib:format("<a href='~s'>~s</a>", [Text,cutName(Name)]));

createVkontakteLink(Id) ->
    Text = "http://vkontakte.ru/id" ++ integer_to_list(Id),
    createVkontakteLink({Text,Id}).
userTable(Arg) ->
    case (Arg#arg.req)#http_request.method of
        'POST' ->
            Vars = yaws_api:parse_post(Arg),
            {value, {_,UserID}}  = lists:keysearch("userID", 1, Vars),
            {value, {_,Level}} = lists:keysearch("level", 1, Vars),
            {value, {_,Exp}} = lists:keysearch("exp", 1, Vars),
            {value, {_,NitroCount}} = lists:keysearch("nitroCount", 1, Vars),
            {value,{_,Money}} = lists:keysearch("money", 1, Vars),
            {value,{_,RealMoney}} = lists:keysearch("realMoney", 1, Vars),
            User = dbUser:getRecord(id, list_to_integer(UserID) ),
            Car = mneser:getRecord(car, User#user.currentCarID),            
            mneser:writeRecord(Car#car{nitroCount=list_to_integer(NitroCount)}),
            mneser:writeRecord(
                User#user{
                  level= list_to_integer(Level),
                  experience=list_to_integer(Exp),                  
                  money = list_to_integer(Money),
                  realMoney = list_to_integer(RealMoney)
                                                                     });
        _Other ->
            ok
    end,
    Users0 = dbUser:getAllRecords(),
    Users = lists:map ( fun (Rec) ->
                {Rec#user.id,
                 cutName(Rec#user.name),
                 cutName( Rec#user.name) ,
                 Rec#user.level,
                 Rec#user.experience,
                 0,%Rec#user.nitroCount,
                 createVkontakteLink(Rec#user.vkontakteID),
                 Rec#user.money,
                 Rec#user.realMoney}
                        end, Users0),
    UsersTable = createHtmlFromTupleList(header, Users,
                                         {id,login, name,level,exp,nitro,vkontakte,money,realMoney}),
    %RowNumbers0 = lists:map(fun({Id,_,_,_,_,_,_}) -> if (Id == guest) -> {0}; true -> {Id} end end, Users),
    %RowNumbers = lists:sort(RowNumbers0),
    RowNumbers = genList(1,length(Users)),
    RowNumbersSubTable = createHtmlFromTupleList(header,RowNumbers,{n}),
    createHtmlFromTupleList([{UsersTable,RowNumbersSubTable}],innerHTML).
%

    createHtmlFromTupleList(header, TupleList, HeaderTuple) ->
    createHtmlFromTupleList(header, TupleList, HeaderTuple,user_table).

    createHtmlFromTupleList(header, TupleList, HeaderTuple,TableID) ->
    Header = [{thead, [], createHtmlFromTuple(th,HeaderTuple)}] ,
    Rows = lists:foldl(
        fun(Tuple, Acc) ->
            Acc ++ [{tr, [], createHtmlFromTuple(Tuple)}]
        end,
        [], TupleList
    ),
    {table, [{border, 1},{id,TableID}], Header ++ Rows}.

    createHtmlFromTuple(th, El) ->
    lists:foldl(
        fun(Element, Acc) ->
            Acc ++ [{th, [], utils:toString(Element)}]
        end,
        [], tuple_to_list (El) );
createHtmlFromTuple(El,innerHTML) ->
    lists:foldl(
        fun(Element, Acc) ->
            Acc ++ [{td, [{valign, "top"}], Element}]
        end,
        [], tuple_to_list (El) ).

createHtmlFromTupleList(TupleList,innerHTML) ->

    Rows = lists:foldl(
        fun(Tuple, Acc) ->
            Acc ++ [{tr, [], createHtmlFromTuple(Tuple,innerHTML)}]
        end,
        [], TupleList
    ),
    {table, [{border, 1}], Rows}.



createHtmlFromTupleList(TupleList) ->

    Rows = lists:foldl(
        fun(Tuple, Acc) ->
            Acc ++ [{tr, [], createHtmlFromTuple(Tuple)}]
        end,
        [], TupleList
    ),
    {table, [{border, 1}], Rows}.


createHtmlFromTuple(El) ->
    lists:foldl(
        fun(Element, Acc) ->
            Acc ++ [{td, [], utils:toString(Element)}]
        end,
        [], tuple_to_list (El) ).

calcUserStat(DayCount) ->

    Activity0 = utils:countElem(dbActivity:getAll(DayCount)),
    Results0 = getResultsByDate(DayCount),
    Activity = lists:keysort(1, Activity0),
    Results = lists:keysort(1, Results0),
    %log:write(debug, ?MODULE_STRING, "calc stat ~n", []),
    zipStat(Activity,Results,[]).

% dbActivity:getAll/1 getResultsByDate
zipStat([],[],Acc) -> lists:reverse(Acc);
zipStat([{{Name,Id},Auth}|Lst],[],Acc) -> zipStat(Lst,[],[{{Name,Id},0,0,Auth}|Acc]);
zipStat([],[{Name,Time,Route}|Lst],Acc) -> zipStat([],Lst,[{{Name,0},Time,Route,0}|Acc]);
zipStat([{{Name,Id},Auth}|Lst1],[{Name,Time,Route}|Lst2],Acc) -> zipStat(Lst1,Lst2,[{{Name,Id},Time,Route,Auth}|Acc]);
zipStat([{{Name1,Id},Auth}|Lst1],[{Name2,Time,Route}|Lst2],Acc) ->
    if (Name1 < Name2)
        -> zipStat(Lst1,[{Name2,Time,Route}|Lst2],[{{Name1,Id},0,0,Auth}|Acc]);
      true -> zipStat([{{Name1,Id},Auth}|Lst1],Lst2,[{{Name2,0},Time,Route,0}|Acc])
    end.


reduceResults([]) -> [];
reduceResults(Lst1) ->
    Fun = fun({CurrName,CTime,CCount},{Name,Time,Count,Res})->
            case {Name,Time,Count} of
                {[],0,0} -> {CurrName,CTime,CCount,Res};
                {CurrName,_,_} -> {CurrName,CTime + Time, CCount + Count,Res};
                _    ->  {CurrName,CTime,CCount,[{Name,Time,Count}|Res]}
            end
           end,
    {CurrName,CTime,CCount,Res} = lists:foldl(Fun, {[],0,0,[]},Lst1),
    [{CurrName,CTime,CCount}|Res].

getResultsByDate(DayCount) ->
    Lst0 = mneser:getResultsByDate(DayCount),
    Lst1 = lists:keysort(1,Lst0),
    reduceResults(Lst1).

 cutName(Name) ->lists:sublist(Name,1,16).


genList(Start,Finish) ->
    Fun = fun (_El,{Curr, Res}) -> {Curr-1,[{Curr}|Res]} end,
    {_,Res} = lists:foldl(Fun,{Finish,[]},lists:duplicate(Finish-Start + 1,1)),
    Res.