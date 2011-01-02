-module(itemEditorView).

-compile(export_all).

-include("config.hrl").
-include("data.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

processItemsIndex(Arg) ->
    ImageRow = fun(Arg1,Arg2,Arg3) -> imageRow(Arg1,Arg2,Arg3)  end,
    CreateInputs = fun(Arg1) -> createInputs(Arg1) end,
    tableEditorView:processRecordIndex(itemClass, Arg, ImageRow, CreateInputs).

imageRow(_Table,Item, _ID) ->
			[
				{b, [], [utils:toStringForScalars(Item#itemClass.id)]}
				, {img, [ {src, helper:urlFor(itemClass, image, Item#itemClass.id)} ]}
				, createImageUploadForm(Item#itemClass.id)].

carSlots() ->
	[engine, wheel, brakes, hull, tailPipe, turbo, nitro].
    
categories() ->	[
	'тормоза',
	'&nbsp;&nbsp;аццкие тормоза',
	'&nbsp;&nbsp;плохие тормоза',
	'двигатели',
	'&nbsp;&nbsp;хорошие',
	'&nbsp;&nbsp;ужасные',
	'спойлеры',
	'уширители'
].

createInputs(Item) ->
	RecordMap = lists:zip( tableEditorView:tableFieldsWithoutId(itemClass),
                        tableEditorView:tableValuesWithoutId(Item) ),
	lists:map(fun({Name, Value}) ->
		case Name of
			slot -> helper:createSelect(Name, carSlots(), Value);
			category -> helper:createSelect(Name, categories(), Value);
			usingType -> helper:createSelect(Name, [slot, inventory], Value);
			targetType -> helper:createSelect(Name, [self, other, world], Value);
			_Other -> tableEditorView:createTextInput(Name,Value)
		end
	end, RecordMap).

createImageUploadForm(ItemId) ->
	{form, [ {enctype, "multipart/form-data" }, {action, helper:urlFor(itemClass, imageUpload, ItemId)},
			{method, "post"}, {style, "margin: 0px"} ],	[
		"Картинка: ",
		{input, [ {type, file}, {name, "file"} ], []},
		{input, [ {type, submit}, {value, "загрузить"} ], []}
	]}.

processUpdateItem(Vars) ->
	Rec = tableEditorView:createTableRecord(itemClass, Vars ),
	case list_to_atom(helper:getPOSTValue(Vars, action)) of
		update -> helper:listTo(Rec#itemClass.id);
		copy ->
			NewId = dbUuid:get(itemClass),
			BaseImagepath = helper:pathTo(itemClass, image, Rec#itemClass.id),
			case filelib:is_regular(BaseImagepath) of
				true -> file:copy(BaseImagepath, helper:pathTo(itemClass, image, NewId));
				_ -> []
			end,
			NewId
	end,
	tableEditorView:processUpdateRecord(itemClass,Vars).

processDeleteItem(Arg) ->
	Imagepath = helper:pathTo(itemClass, image, helper:getPOSTValue(Arg, id)),
	case filelib:is_regular(Imagepath) of true -> file:delete(Imagepath); _ -> [] end,
	tableEditorView:processDeleteRecord(itemClass, Arg).

processItemImageUpload(Arg) ->
	{ok, _Uploadedname, Content} = fileUpload:receive_file(Arg),
	Id = helper:getGETValue(Arg, id),
	file:write_file(helper:pathTo(itemClass, image, Id), Content),
	{redirect, helper:urlFor(itemClass, edit, Id)}.