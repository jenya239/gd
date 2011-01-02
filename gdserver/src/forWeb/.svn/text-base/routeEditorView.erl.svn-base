-module(routeEditorView).

-compile(export_all).

-include("config.hrl").
-include("data.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

itemFields() -> record_info(fields, route).

itemFieldsWithoutId() ->
	Fs = itemFields(),
	lists:sublist(Fs, 2, length(Fs) - 1).

itemValues(Item) ->
	IL = tuple_to_list(Item),
	lists:sublist(IL, 2, length(IL) - 1).

itemValuesWithoutId(Item) ->
	IL = tuple_to_list(Item),
	lists:sublist(IL, 3, length(IL) - 2).

createRouteRecord(Vars) ->
	Fun = fun(Field) ->
		{value, {_, Value}} = lists:keysearch( utils:toStringForScalars(Field), 1, Vars ),
		helper:listTo(Value)
	end,
	list_to_tuple( [route | lists:map(Fun, itemFields())] ).

processItemsIndex(Arg) ->
	case (Arg#arg.req)#http_request.method of
		'POST' -> 
			Rec = createRouteRecord( yaws_api:parse_post(Arg) ),
			mneser:writeRecord( Rec#route{id = dbUuid:get(route)} );
		_Other -> ok
	end,
	Result = [ createRoutesTable(), createRouteForm( create, #route{} ) ],
        case helper:paramGETExists(Arg, editId) of
		true ->
			Item = mneser:getRecord( route, helper:getGETValue(Arg, editId) ),
			L = [
				{b, [], [utils:toStringForScalars(Item#route.id)]}
				, createRouteForm( update, Item )
			],
			Result2 = L ++ Result;
		false -> Result2 = Result
	end,
	Result2.

createDeleteForm(Id) ->
	{form, [ {action, helper:urlFor(route, delete)}, {method, post}, {style, "margin: 0px"} ], [
		{input, [{type, hidden}, {name, id}, {value, Id}], []},
		{input, [{type, submit}, {value, "del"}], []}
	]}.


createInputs(Item) ->
	RecordMap = lists:zip( itemFieldsWithoutId(), itemValuesWithoutId(Item) ),
	lists:map(fun({Name, Value}) ->
		case Name of
			%slot -> helper:createSelect(Name, carSlots(), Value);
			%category -> helper:createSelect(Name, categories(), Value);
			%usingType -> helper:createSelect(Name, [slot, inventory], Value);
			%targetType -> helper:createSelect(Name, [self, other, world], Value);
			_Other -> io_lib:format("~w <input name='~w' value='~s' style='width: 100%;'/>", [Name, Name, utils:toStringForScalars(Value)])
		end
	end, RecordMap).


createRouteForm(Mode, Item) ->
	{form, [ {action, helper:urlFor(route, Mode)}, {method, post} ], [
		{input, [ {type, hidden}, {name, id}, {value, Item#route.id} ], []},
		{table, [{width, "100%"}], [
			helper:createRow( createInputs(Item) )
		]},
		{input, [{type, submit}, {name, action}, {value, Mode}], []},
		case Mode of update -> {input, [{type, submit}, {name, action}, {value, copy}], []}; _ -> [] end
	]}.

createRoutesTable() ->
	Items = mneser:getAllRoutes(),
	Fun = fun(Item) ->
		helper:createRow(
				[{a, [{href, helper:urlFor(route, edit, Item#route.id)}], [utils:toStringForScalars(Item#route.id)]}
					| itemValuesWithoutId(Item)]
		)
	end,
	{table, [{bgcolor, "tan"}, {width, "100%"}], [
		helper:createRow(th, [edit | itemFieldsWithoutId() ] ),
		lists:map(Fun, Items)
	]}.

processCreateItem(Vars) ->
	Rec = createRouteRecord( Vars ),
	Rec2 = Rec#route{id = dbUuid:get(route)},
	mneser:writeRecord( Rec2 ),
	{redirect, helper:urlFor(route, edit, Rec2#route.id)}.

processUpdateRoute(Vars) ->
	Rec = createRouteRecord( Vars ),
	Id = case list_to_atom(helper:getPOSTValue(Vars, action)) of
		update -> helper:listTo(Rec#route.id);
		copy ->	dbUuid:get(route)
	end,
	mneser:writeRecord(Rec#route{id = Id}),
	{redirect, helper:urlFor(route, edit, Id)}.

processDeleteItem(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	mneser:delete(route, Id),
	Imagepath = helper:pathTo(route, image, Id),
	case filelib:is_regular(Imagepath) of true -> file:delete(Imagepath); _ -> [] end,
	{redirect, helper:urlFor(route, index)}.

processItemImageUpload(Arg) ->
	{ok, _Uploadedname, Content} = fileUpload:receive_file(Arg),
	Id = helper:getGETValue(Arg, id),
	file:write_file(helper:pathTo(route, image, Id), Content),
	{redirect, helper:urlFor(route, edit, Id)}.
