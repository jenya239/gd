-module(tableEditorView).

-compile(export_all).

-include("config.hrl").
-include("data.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

out(Arg) ->
   EmptyAdjustment = fun (_A,_B,_C) -> [] end,
   Table = list_to_atom( helper:getGETValue(Arg, table) ),
   CreateInput =
       fun(Record) ->
        RecordMap = lists:zip( tableEditorView:tableFieldsWithoutId(Table),
        tableEditorView:tableValuesWithoutId(Record) ),
        lists:map(fun({Name,Value}) -> createTextInput(Name,Value) end, RecordMap )
       end,
   processRecordIndex(Table, Arg, EmptyAdjustment, CreateInput).

getTables() ->
    %mnesia:system_info(tables),
    Tables = [
        level,
        route,
        workOffer,
        car,
        carClass,
        recolorPrice,
        league,
        tip,
        global,
        postMessage
             ],
    lists:map(fun (Table) -> createTableRef(Table) end, Tables).

createTableRef(Table) -> 
    Str = atom_to_list(Table),
    {br,[],{a,[{href,"editdb.yaws?table="++Str}],Str}}.

createTextInput(Name,Value) ->
    io_lib:format("~w <input name='~w' value='~s' style='width: 100%;'/>", [Name, Name, utils:toStringForScalars(Value)]).

processRecordIndex(Table, Arg, AdjustmentFun, CreateInput) ->
  case (Arg#arg.req)#http_request.method of
		'POST' ->
			Rec = tableEditorView:createTableRecord(Table, yaws_api:parse_post(Arg)),
            mneser:writeRecord(setelement(2, Rec, dbUuid:get(Table)));
		_Other -> ok
  end,
  
  SortIndex = case yaws_api:getvar(Arg, "sort") of {ok, S1} -> list_to_integer(S1); undefined -> 2 end,
          
  Result = [tableEditorView:createTable(Table, Arg),
            tableEditorView:createRecordForm(create, tableEditorView:getDefaultRecord(Table),
                                             Table, CreateInput, SortIndex)],
  Result2 =
  case helper:paramGETExists(Arg, editId) of
    true ->
        EditID0 = helper:getGETValue(Arg, editId),
        EditID =
            if is_list( EditID0 ) ->
                list_to_atom(EditID0);
               true ->  EditID0
            end,
        Record =  mneser:getRecord( Table, EditID),
        AdjustmentFun(Table, Record, helper:getGETValue(Arg, editId) )
        ++ [ createBoldID(element(2,Record) ),
            tableEditorView:createRecordForm(update, Record, Table, CreateInput, SortIndex),
            Result];
    false -> Result
  end,
  [createTableRef(Table),Result2, {pre, [], io_lib:format("~p.", [mneser:getAllRecords(Table)])} ].

%вещи (есть) - глобал константы - машины - трассы - левелы
createMainRecordsPre() ->
	{pre, [], io_lib:format("~p.", [[
		{itemClasses, mneser:getAllRecords(itemClass)},
		{globals, mneser:getAllRecords(global)},
		{carClasses, mneser:getAllRecords(carClass)},
		{routes, mneser:getAllRecords(route)},
		{levels, mneser:getAllRecords(level)},
    {recolorPrices, mneser:getAllRecords(recolorPrice)}
	]])}.

createSendNotificationForm() ->
	{form, [ {action, helper:urlFor(sendNotifications, send)}, {method, "post"} ],	[
		{font, [], "Введите сообщение, проверьте грамматику и чистописание:"},
    {br, [], []},
    {input, [{name,"userID1"}],[]},
    {input, [{name,"userID2"}],[]},
		{br, [], []},
		{textarea, [ {name, "message"}, {rows, 10}, {cols, 60} ], []},
		{br, [], []},
		{input, [
			{type, button},
			{onclick, "try_submit(this.parentNode, 'Точно отправить сообщение всем юзерам через ВКонтакт?'); return false;"},
			{value, "отправить сообщение всем юзерам через ВКонтакт"}
		], []}
	]}.

createGiveFuelButton() ->
	{form, [ {action, helper:urlFor(sendNotifications, giveFuel)}, {method, "post"} ],	[
        {br, [], []},
		{input, [
			{type, button},
			{onclick, "try_submit(this.parentNode, 'Вы уверены, что хотите наполнить все баки в городе горог?'); return false;"},
			{value, "наполнить все баки города дорог!"}
		], []}
	]}.

createStopSendingNotificationForm() ->
	{form, [ {action, helper:urlFor(sendNotifications, stop)}, {method, "post"} ],	[
		{input, [
			{type, button},
			{onclick, "try_submit(this.parentNode, 'Точно остановить отправку сообщений всем юзерам через ВКонтакт?'); return false;"},
			{value, "остановить отправку сообщение всем юзерам через ВКонтакт"}
		], []}
	]}.
createResetMainTablesForm() ->
	{form, [ {action, helper:urlFor(mainTables, reset)}, {method, "post"} ],	[
		{font, [], "Введите список кортежей:"},
		{br, [], []},
		{textarea, [ {name, "message"}, {rows, 10}, {cols, 60} ], []},
		{br, [], []},
		{input, [
			{type, button},
			{onclick, "try_submit(this.parentNode, 'Точно перезаписать таблицы itemClasses, globals, cars, routes и levels?'); return false;"},
			{value, "записать itemClasses, globals, carClasses, routes, levels, recolorPrices вместо существующих"}
		], []}
	]}.

processMainTablesReset(Arg) ->
	Str = helper:getPOSTValue(Arg, message),
	All = utils:stringToTerm(Str),
	mneser:rewrite( itemClass, utils:extractNamedList( itemClasses, All ) ),
	mneser:rewrite( carClass, utils:extractNamedList( carClasses, All ) ),
	mneser:rewrite( route, utils:extractNamedList( routes, All ) ),
	mneser:rewrite( level, utils:extractNamedList( levels, All ) ),
  mneser:rewrite( recolorPrice, utils:extractNamedList( recolorPrices, All ) ),
	{atomic, ok} = mnesia:clear_table(global),
	{atomic, ok} = mnesia:transaction( fun() -> lists:foreach(
		fun( Record ) -> mnesia:write( Record ) end,
		utils:extractNamedList( globals, All )
	) end ),
	{ redirect, helper:urlFor(mainTables, resetView) }.

createBoldID(ID) ->
  List =
      if is_integer(ID) ->
          utils:toStringForScalars(ID);
         true -> atom_to_list( ID )
      end,
 {b, [], [ List]}.

tableFields(Table) -> mnesia:table_info(Table,attributes).

getDefaultRecord(Table) ->
    list_to_tuple([Table] ++ lists:map(fun(_A) -> "" end, tableFields(Table)) ).

tableFieldsWithoutId(Table) ->
	Fs = tableFields(Table),
	lists:sublist(Fs, 2, length(Fs) - 1).
	
sortLinks(List, Arg, StartIndex) ->
    QueryList = yaws_api:parse_query(Arg),
    NewQueryPart = lists:foldl(fun(E, Acc) -> 
        {QKey, QVal} = E,
        QString = if QKey =:= "sort" ->
            "";
        true ->
            QKey ++ "=" ++ QVal
        end,
        
        if length(Acc) > 0 ->
            Acc ++ "&" ++ QString;
        true ->
            QString
        end
            
    end, [], QueryList),
    
    Url = yaws_api:request_url(Arg),
    
    {NewList, _} = lists:foldl(fun(Element, {ListAcc, Index}) -> 
        NewUrl = case length(NewQueryPart) > 0 of 
            true -> 
                Url#url{querypart=NewQueryPart ++ "&sort=" ++ integer_to_list(Index)};
            _Other ->
                Url#url{querypart="sort=" ++ integer_to_list(Index)}
        end,
        
        NewElement = {a, [{href, yaws_api:format_url(NewUrl)}], atom_to_list(Element)},
        {ListAcc ++ [NewElement], Index + 1}
    end, {[], StartIndex}, List),
    NewList.

tableValues(Item) ->
	IL = tuple_to_list(Item),
	lists:sublist(IL, 2, length(IL) - 1).

tableValuesWithoutId(Item) ->
	IL = tuple_to_list(Item),
	lists:sublist(IL, 3, length(IL) - 2).

processCreateItem(Table,Vars) ->
	Rec = tableEditorView:createTableRecord(Table, Vars ),
    Rec2 = setelement(2,Rec, dbUuid:get(Table) ),
	mneser:writeRecord( Rec2 ),
    queryCache:invalidate(list_to_atom(atom_to_list(Table) ++ "s")),
    SortIndex = case helper:paramPOSTExists(Vars, "sort") of true -> helper:getPOSTValue(Vars, "sort"); false -> 2 end,
	{redirect, helper:urlFor(Table, edit, element(2, Rec2), SortIndex) }.

processUpdateRecord(Table, Vars) ->
    Rec = tableEditorView:createTableRecord(Table, Vars ),
    Id = case list_to_atom(helper:getPOSTValue(Vars, action)) of
        update -> helper:listTo(element(2,Rec));
        copy   -> dbUuid:get(Table)
    end,
    
    SortIndex = case helper:paramPOSTExists(Vars, "sort") of true -> helper:getPOSTValue(Vars, "sort"); false -> 2 end,
    mneser:writeRecord(setelement(2, Rec, Id)),
    queryCache:invalidate(list_to_atom(atom_to_list(Table) ++ "s")),
	{redirect, helper:urlFor(Table, edit, Id, SortIndex)}. 

processDeleteRecord(Table, Arg) ->
  Id = helper:getPOSTValue(Arg, id),
	case Table of
		user ->
			Url = helper:urlFor( user, show, mults, dbUser:getVkontakteId(Id) ),
			dbUser:delete( Id ),
			{ redirect, Url };
		_Other ->
			mneser:delete( Table, Id ),
            queryCache:invalidate(list_to_atom(atom_to_list(Table) ++ "s")),
			{ redirect, helper:urlFor( Table, index ) }
	end.

createTableRecord(Table,Vars) ->
	Fun = fun(Field) ->    
		{value, {_, Value}} = lists:keysearch( utils:toStringForScalars(Field), 1, Vars ),
		helper:listTo(Value)
	end,
	Fields = tableFields(Table),
  Rec = list_to_tuple( [Table | lists:map(Fun, Fields )] ),
  if ( is_list( element(2,Rec)  ) )
           -> setelement(2,Rec, list_to_atom( element(2,Rec)  )  );
      true -> Rec
  end.

createButtonForm(ActionCaption, Url, Question, Id, SortIndex) ->
	{form, [ {action, Url}, {method, post}, {style, "margin: 0px"} ], [
		{input, [{type, hidden}, {name, id}, {value, Id}], []},
		{input, [{type, hidden}, {name, sort}, {value, SortIndex}], []},
		{input, [
			{type, button},
			{onclick, "try_submit(this.parentNode, '"++Question++"'); return false;"},
			{value, ActionCaption}
		], []}
   ]}.

createDeleteForm(Table, Id) ->
    createDeleteForm(Table, Id, sortIndex).
   
createDeleteForm(Table, Id, SortIndex) ->
	createButtonForm("del", helper:urlFor(Table, delete), "delete id "++utils:toStringForScalars(Id)++"?", Id, SortIndex).

createTable(Table, Arg) ->
    SortIndex = case yaws_api:getvar(Arg, "sort") of {ok, S1} -> list_to_integer(S1); undefined -> 2 end,
    
	Records0 = mneser:getAllRecords(Table),
    Records = lists:keysort(SortIndex, Records0),

	Fun = fun(Record) ->
		helper:createRow(
			[
				tableEditorView:createDeleteForm(Table, element(2, Record), SortIndex),
				{a, [{href, helper:urlFor(Table, edit, element(2, Record), SortIndex)}], [utils:toStringForScalars(element(2, Record))]}
			]
			++ tableEditorView:tableValuesWithoutId(Record)
		)
	end,
	{table, [{bgcolor, "tan"}, {width, "100%"}], [
		helper:createRow(th, [del | [edit | tableEditorView:sortLinks(tableEditorView:tableFieldsWithoutId(Table), Arg, 3)]]),
		lists:map(Fun, Records)
	]}.

createRecordForm(Mode, Record, Table, CreateInputs, SortIndex) ->
	{form, [ {action, helper:urlFor(Table, Mode)}, {method, post} ], [
		{input, [ {type, hidden}, {name, hd(tableFields(Table))}, {value, element(2, Record)} ], []},
		{input, [{type, hidden}, {name, sort}, {value, SortIndex}], []},
		{table, [{width, "100%"}], [
			helper:createRow( CreateInputs(Record) )
		]},
		{input, [{type, submit}, {name, action}, {value, Mode}], []},
		case Mode of update -> {input, [{type, submit}, {name, action}, {value, copy}], []}; _ -> [] end
	]}.

processReset(Table, Arg) ->
	Str = helper:getPOSTValue(Arg, list),
	List = utils:stringToTerm(Str),
	mneser:rewrite(Table, List),
  dbUuid:fix(Table),
	{ redirect, helper:urlFor(Table, index) }.

createResetItemClassesForm(Table) ->
  TableStr = atom_to_list(Table),
	{form, [ {action, helper:urlFor(Table, reset)}, {method, "post"} ],	[
		{font, [], "Введите список кортежей:"},
		{br, [], []},
		{textarea, [ {name, "list"}, {rows, 10}, {cols, 60} ], []},
		{br, [], []},
		{input, [
			{type, button},
			{onclick, io_lib:format("try_submit(this.parentNode, 'перезаписать таблицу \\'~s\\'?'); return false;",[TableStr] )},
			{value, io_lib:format("записать '~s' вместо существующих",[TableStr])}
		], []}
	]}.
