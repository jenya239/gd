-module(helper).

-compile(export_all).

-include("data.hrl").
-include("forWeb.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

paramGETExists(Arg, Key) ->
	case lists:keysearch( utils:toStringForScalars(Key), 1, yaws_api:parse_query(Arg) ) of
		false -> false;
		_Other -> true
	end.
	
paramPOSTExists(Arg, Key) when is_record(Arg, arg) ->
	case lists:keysearch(utils:toStringForScalars(Key), 1, yaws_api:parse_post(Arg)) of
		false -> false;
		_Other -> true
	end;
	
paramPOSTExists(Vars, Key) ->
    case lists:keysearch(utils:toStringForScalars(Key), 1, Vars) of
        false -> false;
        _Other -> true
    end.

listTo(Val) ->
	Value = case is_list(Val) of true -> string:strip(Val); false -> Val end,
	case utils:isInt(Value) of
		true -> list_to_integer(Value);
		false -> case utils:isFloat(Value) of
			true -> list_to_float(Value);
			false -> case Value of
                "true" -> true;
                "false" -> false;
                Value2 ->
                    Value2
            end
		end         
	end.

getPOSTValue(Arg, Key) when is_record(Arg, arg) ->
	getPOSTValue(yaws_api:parse_post(Arg), Key);

getPOSTValue(Vars, Key) ->
	{value, {_, Value}} = lists:keysearch( utils:toStringForScalars(Key), 1, Vars ),
	listTo(Value).

getGETValue(Arg, Key) ->
	{value, {_, Value}} = lists:keysearch( utils:toStringForScalars(Key), 1, yaws_api:parse_query(Arg) ),
	listTo(Value).
	
getPOSTorGETValue(Arg, Key) ->
    case paramGETExists(Arg, Key) of
        false ->
            case paramPOSTExists(Arg, Key) of
                false ->
                    undefined;
                true ->
                    getPOSTValue(Arg, Key)
            end;
        true ->
            getGETValue(Arg, Key)
    end.

createRow(CellTag, Cells) ->
	TDs = [{CellTag, [], [utils:toStringForScalars(Cell)]} || Cell <- Cells],
	{tr, [], TDs}.
createRow(Cells) ->
	createRow(td, Cells).

createSelect(Name, Values, Selected) ->
	[ utils:toStringForScalars(Name), { select, [{name, Name}], createSelectOptions(Values, Selected) } ].

createSelectOptions(Values, Selected) ->
	Opts = [{option, [{value, Value}], [utils:toStringForScalars(Value)]} || Value <- Values],
	Fun = fun({option, [{value, Value}], Caption}) ->
		case atom_to_list(Value) == Selected of
			true -> {option, [{value, Value}, {selected, true}], Caption};
			false -> {option, [{value, Value}], Caption}
		end
	end,
	lists:map(Fun, Opts).

urlFor(itemClass, edit, Id) ->
	io_lib:format("/editor/items.yaws?editId=~w", [Id]);
urlFor(user, show, Id) ->
	io_lib:format("/admin/user.yaws?id=~w", [Id]);
urlFor(user, admin, Id) ->
	io_lib:format("/admin/user.yaws?id=~w&admin", [Id]);
urlFor(itemClass, imageUpload, Id) ->
	urlFor(itemClass, imageUpload) ++ "&id=" ++ integer_to_list(Id);
urlFor(itemClass, image, Id) ->
	io_lib:format("../client/data/items/~b.png", [Id]);
urlFor(Table, edit, Id) ->
  io_lib:format("/editor/editdb.yaws?table=~w&editId=~w", [Table, Id]);
urlFor(Table, Action, Id) ->
  io_lib:format("/editor/editdb.yaws?table=~w&action=~w&id=~w", [Table, Action, Id]).
    
urlFor(itemClass, index) ->
	io_lib:format("/editor/items.yaws", []);
urlFor(editor, index) ->
	io_lib:format("/editor/index.yaws", []);
urlFor(admin, index) ->
	io_lib:format("/admin/index.yaws", []);
urlFor(mainTables, resetView) ->
	io_lib:format("/admin/rewrite.yaws", []);
urlFor(Table, index) ->
	io_lib:format("/editor/editdb.yaws?table=~w", [Table]);
urlFor(Table, Action) ->
	io_lib:format("/admin/actions.yaws?table=~w&action=~w", [Table, Action]).
	
urlFor(user, show, mults, VkontakteId) ->
	io_lib:format("/admin/user.yaws?vkontakteId=~w", [VkontakteId]);	
urlFor(itemClass, edit, Id, SortIndex) ->
    io_lib:format("/editor/items.yaws?editId=~w&sort=~w", [Id, SortIndex]);
urlFor(itemClass, imageUpload, Id, SortIndex) ->
	urlFor(itemClass, imageUpload) ++ "&id=" ++ integer_to_list(Id) ++ "&sort=" ++ integer_to_list(SortIndex);
urlFor(Table, edit, Id, SortIndex) ->
  io_lib:format("/editor/editdb.yaws?table=~w&editId=~w&sort=~w", [Table, Id, SortIndex]);	
urlFor(Table, Action, Id, SortIndex) ->
  io_lib:format("/editor/editdb.yaws?table=~w&action=~w&id=~w&sort=~w", [Table, Action, Id, SortIndex]).

urlFor(updateBlackList) ->
	"/admin/actions.yaws?table=none&action=updateBlackList".
	

pathTo(itemClass, image, Id) ->
	io_lib:format("src/web/client/data/items/~b.png", [Id]).

linkFor(user, show, Id) ->
		{a, [ { href, urlFor(user, show, Id) } ], utils:toString(Id) }.

processAction(Arg) ->
	case (Arg#arg.req)#http_request.method of
		'POST' -> ok;
		Other -> erlang:error({error, incorrectHTTPMethod, Other})
	end,
	Table = list_to_atom( getGETValue(Arg, table) ),
	Action = list_to_atom( getGETValue(Arg, action) ),
	Msg = { ehtml, { b, [], io_lib:format("unknownAction ~p, ~p", [ Table, Action ]) } },
	DefaultProcess = fun() ->
		case Action of
			create -> tableEditorView:processCreateItem(Table, yaws_api:parse_post(Arg));
			delete -> tableEditorView:processDeleteRecord(Table, Arg);
			update -> tableEditorView:processUpdateRecord(Table, yaws_api:parse_post(Arg));
			_Other -> Msg
		end
	end,
	case Table of
		car ->
			case Action of
				changeColor -> userAdmin:processChangeCarColor( Arg );
				delete -> userAdmin:processCarDelete( Arg );
				_Other -> DefaultProcess()
			end;
    carClass ->
			case Action of
				reset -> tableEditorView:processReset(Table,Arg);
				_Other -> DefaultProcess()
			end;
    recolorPrice ->
			case Action of
				reset -> tableEditorView:processReset(Table,Arg);
				_Other -> DefaultProcess()
			end;
    level ->
			case Action of
				reset -> tableEditorView:processReset(Table,Arg);
				_Other -> DefaultProcess()
			end;			
		itemClass ->
			case Action of
				create -> tableEditorView:processCreateItem(Table,yaws_api:parse_post(Arg));
				delete -> itemEditorView:processDeleteItem(Arg);
				update -> itemEditorView:processUpdateItem(yaws_api:parse_post(Arg));
				imageUpload -> itemEditorView:processItemImageUpload(Arg);
				reset -> tableEditorView:processReset(Table,Arg);
				_Other -> Msg
			end;
		user ->
			case Action of
				addAdminRole -> userAdmin:processAddAdminRole(Arg);
				removeAdminRole -> userAdmin:processRemoveAdminRole(Arg);
				addMoney -> userAdmin:processAddMoney(Arg);
				addRealMoney -> userAdmin:processAddRealMoney(Arg);
				searchByName -> userAdmin:processSearchByName(Arg);
				searchByVkontakteId -> userAdmin:processSearchByVkontakteId(Arg);
				changeNickname -> userAdmin:processChangeNickname(Arg);
				give -> userAdmin:processGiveCar(Arg);
				unban -> userAdmin:processUnban(Arg);
				switchHomeCity -> userAdmin:processSwitchHomeCity(Arg);
				changeLevel -> userAdmin:processChangeLevel(Arg);
				_Other -> DefaultProcess()
			end;
		item ->
			case Action of
				add -> userAdmin:processAddItem( Arg );
				delete -> userAdmin:processItemDelete( Arg );
				_Other -> DefaultProcess()
			end;
		mainTables ->
			case Action of
				reset -> tableEditorView:processMainTablesReset(Arg);
				_Other -> Msg
			end;
        sendNotifications ->
            case Action of
                send -> 
                    Message = helper:getPOSTValue(Arg, message),
                    UserID1 = helper:getPOSTValue(Arg, userID1),
                    UserID2 = helper:getPOSTValue(Arg, userID2),
                    vkontakte:startSendingMessage(UserID1,UserID2,Message);
                stop ->
                    vkontakte:stopSendingMessage();
                giveFuel ->
                    dbCar:giveFuel();
                _Other -> Msg
            end,
            {redirect, "sendNotifications.yaws"};
    none ->
      case Action of
				updateBlackList -> vkontakte:updateBlackList(), {redirect, helper:urlFor( admin, index )};
				_Other -> Msg
			end;      
    _Other -> DefaultProcess()
	end.

%????????????. ???????????? ?????? ??????????, ????????????????, ???????????? ???? ?????????? ????????????

getRouteNamesTable() ->

    Routes = mneser:getAllRoutes(),
    Fun = fun(Route, RouteRowsList) ->
        Row = {tr, [],
            [
                {td, [], [ Route#route.displayName ]}
            ]
        },
        [Row | RouteRowsList]
    end,

    RouteRowsList = lists:foldl(Fun, [], Routes),


        {table, [],
        [
            {tr, [],
            [
                {td, [], [ "Route Name" ]}
            ]
            }
            | RouteRowsList
        ]
        }.

%???????????????????? ??????????????????. ???????????????? ?????? ?????????????? ?????????? ?????????????? ?? ?????????????????? ????????????

createGlobalInfoTable() ->
	GI = dbGlobal:getAll(),
	{table, [],
		lists:map(
			fun({global, Key, Value}) ->
				{tr, [], [
					{td, [], utils:toStringForScalars(Key)},
					{td, [], utils:toStringForScalars(Value)}
				]}
			end,
			GI
		)
	}.

createGlobalConstForm() ->
	{form, [ {action, helper:urlFor(global, set)}, {method, post} ], [
		{input, [ {type, text}, {name, key} ], []},
		{input, [ {type, text}, {name, value} ], []},
		{input, [ {type, submit}, {value, "???????????????????? ????????????????"}], []}
	]}.

processSetGlobalConst(Arg) ->
	mneser:writeRecord({global,
		list_to_atom(helper:getPOSTValue(Arg, key)),
		helper:getPOSTValue(Arg, value)
	}),
	{redirect, helper:urlFor(editor, index)}.

%?????????????? ?????????? ??????????
createRouteForm(Route) ->
	{form, [ {action, helper:urlFor(route, update)}, {method, post}, {class, route} ], [
		Route#route.displayName,
		{input, [ {type, hidden}, {name, id}, {value, Route#route.id} ], []},
		{input, [ {type, text}, {name, moneyPrize}, {value, Route#route.moneyPrize}, {size, 6} ], []},
		{input, [ {type, submit}, {value, "????????????????"}], []}
	]}.

createBasePrizeTable() ->
	lists:map(
		fun(Route) ->
			createRouteForm(Route)
		end,
		mneser:getAllRoutes()
	).

processUpdateRoute(Arg) ->
	mnesia:transaction(fun() ->
		Route = mneser:getRecord_nt(route, helper:getPOSTValue(Arg, id)),
		mnesia:write(Route#route{moneyPrize = helper:getPOSTValue(Arg, moneyPrize)})
	end),
	{redirect, helper:urlFor(editor, index)}.

createOnlineUsersTable() ->
	Clients = globalChat:onlineUsers(cityManager:getCityChatByID(1)) ++ globalChat:onlineUsers(cityManager:getCityChatByID(2)) ++ globalChat:onlineUsers(cityManager:getCityChatByID(3)),
    TablePrefix =              {tr, [],
                                [{td, [], [ "userID" ]},
                                 {td, [], [ "name" ]},
                                 {td, [], [ "state" ]},
                                 {td, [], [ "roles" ]}]},
    Fun = fun({UserID1, Name, StateName, Roles}) ->
        {tr, [],
            [
                {td, [], [ utils:toString(UserID1) ]},
                {td, [], [{a, [{href, helper:urlFor(user, show, UserID1)}], [Name]}]},
                {td, [], [ atom_to_list( StateName ) ]},
                {td, [], [ utils:fmt("~p", [Roles]) ]} 
            ]}
          end,
    [  {h4, [], "count=" ++ integer_to_list(length(Clients) ) },
                {table, [{border, "1"},{id, "user_table"}],
                [TablePrefix | lists:map(Fun, Clients)]}].

createSalesTable() ->
	Sales = dbActivity:getSales(),
	{table, [ {cellpadding, 4} ], [
		createRow(th, [
			"??????????",
			"??????",
			"??????",
			"???????? ??????.",
			"??????????????",
			"???????? ????????<b>????</b> ??????.",
			"??????????????",
			"???????????????? ??????.",
			"??????????????"
		]),
		lists:map(
			fun({ Microsecs, B, IC, U }) ->
				createRow([
					utils:timestampToHumanString( utils:microsecsToTimestamp( Microsecs ) ),
					{a, [ { href, urlFor(user, show, U#user.id) } ], U#user.name },
					IC#itemClass.name,
					B#buyItem.oldUserMoney,
					B#buyItem.oldUserRealMoney,
					B#buyItem.itemPrice,
					B#buyItem.itemRealPrice,
					B#buyItem.newUserMoney,
					B#buyItem.newUserRealMoney
				])
			end,
			Sales
		)
	]}.

processPeriodActivities(Arg) ->
	{html, utils:termToJson(
		dbActivity:getAll( helper:getGETValue(Arg, startTime), helper:getGETValue(Arg, endTime) )
	)}.

createNicksChangesTable() ->
	Changes = dbActivity:getNicksChanges(),
	{table, [ {cellpadding, 4} ], [
		createRow(th, [
			"??????????", "????????", "???????????? ??????", "?????????? ??????", "?????? ????????????", "???????? ?? ??????????????"
		]),
		lists:map(
			fun({_, Microsecs, UserId, {_, OldName, NewName, ModerId, Cost }, _}) ->
				createRow([
					utils:timestampToHumanString( utils:microsecsToTimestamp( Microsecs ) ),
					linkFor(user, show, UserId), OldName, NewName, linkFor(user, show, ModerId), Cost
				])
			end,
			Changes
		)
	]}.

log( Place, Data ) ->
	% FormatSymbol = fun( Type ) ->
	% 	case Type of
	% 		string -> "~s";
	% 		erlang -> "~p";
	% 		_ -> "~w"
	% 	end
	% end,
	% {FormatPart, DataValues} = lists:foldl( fun( {Type, Val}, {F, V} ) -> {F ++ "\t" ++ FormatSymbol(Type), V ++ [Val]} end, {"", []}, Data ),
	% {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(erlang:now()),
	% TimeStr = lists:flatten(io_lib:fwrite("~4..0B.~2B.~2B ~2B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])),
	% LogStr = io_lib:format( "~s\t~s" ++ FormatPart ++ "\n", [TimeStr, Place] ++ DataValues ),
	% file:write_file( "logs/log.txt", LogStr, [append] ).
	ok.

checkAuth( SessionCookie, Login, Password ) ->
	NewSession = case ( ( Login =:= "admin" ) and ( Password =:= "mnogoeuro" ) ) of
		false -> #session_info{ username = guest, last_msg = {error, {incorrect_login_or_password, "incorrect login or password"} } };
		true -> #session_info{ username = admin, last_msg = {ok, {login_success, "login successfully"} } }
	end,
	{SessionStatus, Session} = case SessionCookie of
		[] -> {created, NewSession};
		_ ->
			case yaws_api:cookieval_to_opaque( SessionCookie ) of
				{error, _} -> {created, NewSession};
				{ok, Sess} -> 
					case Sess#session_info.username of
						guest -> {found, NewSession};
						admin -> {found, #session_info{ username = admin, last_msg = {error, {already_signed_in, "already signed in"} } } }
					end
			end
	end,
	Cookie = case SessionStatus of
		created -> yaws_api:new_cookie_session( Session );
		found -> SessionCookie
	end,
	case SessionStatus of 
		found -> yaws_api:replace_cookie_session( Cookie, Session );
	_ -> ok end,
	RedirectPath = case Session#session_info.username of
		guest -> "/";
		admin -> "/admin/"
	end,
	AuthCookie = case SessionStatus of
		created -> yaws_api:setcookie( "sid", Cookie );
		found -> []
	end,
	{RedirectPath, AuthCookie}.

logout( SessionCookie ) ->
	NewSession = #session_info{ username = guest, last_msg = {warning, {not_signed_in, "you not signed in"} } },
	{SessionStatus, Session} = case SessionCookie of
		[] -> {created, NewSession};
		_ ->
			case yaws_api:cookieval_to_opaque( SessionCookie ) of
				{error, _} -> {created, NewSession};
				{ok, Sess} -> 
					case Sess#session_info.username of
						guest -> {found, NewSession};
						admin -> {found, #session_info{ username = guest, last_msg = {ok, {logout_success, "logout successfully"} } } }
					end
			end
	end,
	Cookie = case SessionStatus of
		created -> yaws_api:new_cookie_session( Session );
		found -> SessionCookie
	end,
	case SessionStatus of 
		found -> yaws_api:replace_cookie_session( Cookie, Session );
	_ -> ok end,
	RedirectPath = "/",
	AuthCookie = case SessionStatus of
		created -> yaws_api:setcookie( "sid", Cookie );
		found -> []
	end,
	{RedirectPath, AuthCookie}.

stripDuplicatesSid( WasSid, Source ) -> 
	[H|T] = Source, 
	IsSid = lists:nth( 1, string:tokens( H, "=" ) ) =:= "sid", 
	Current = case IsSid of 
		true -> case WasSid of 
				true -> ""; 
				false -> H 
			end; 
		false -> H 
	end,
	Sep = case Current of 
		"" -> ""; 
		_ -> "; " 
	end,
	Next = case T of 
		[] -> ""; 
		_ -> Sep ++ stripDuplicatesSid( WasSid orelse IsSid, T ) 
	end,
	Current ++ Next.

arg_rewrite( ArgOriginal ) ->
	file:write_file( "arg_oorig.txt", io_lib:format( "~p", [ArgOriginal] ) ),
	file:write_file( "logs/log.txt", "\n", [append] ),
	log( "arg_rewrite:begin ip", [{erlang, ArgOriginal#arg.client_ip_port}] ),
	log( "arg_rewrite:begin path", [{string, element( 2, (ArgOriginal#arg.req)#http_request.path )}] ),
	log( "arg_rewrite:begin cookie", [{string, element( 14, ArgOriginal#arg.headers )}] ),
	SessionForLog = yaws_api:cookieval_to_opaque( yaws_api:find_cookie_val( "sid", element( 14, ArgOriginal#arg.headers ) ) ),
	log( "arg_rewrite:begin session", [{terms, SessionForLog}] ),

	NewCookies = case element( 14, ArgOriginal#arg.headers ) of
		[] -> [];
		_ -> [stripDuplicatesSid( false, lists:reverse( string:tokens( lists:nth( 1, element( 14, ArgOriginal#arg.headers ) ), "; " ) ) )]
	end,
	log( "arg_rewrite new cookie", [{string, NewCookies}] ),
	Arg = ArgOriginal#arg{ headers = setelement( 14, ArgOriginal#arg.headers, NewCookies ) },
	log( "arg_rewrite new cookie check", [{string, element( 14, Arg#arg.headers )}] ),

	GuestGetPaths = [
		"/",
		"/admin/styles/bootstrap.min.css",
		"/admin/javascripts/bootstrap.min.js",
		"/stats/war.yaws"
	],
	GuestPostPaths = [
		"/session.yaws?action=login",
		"/callback/GLOBxVB4OW7RuQn6r6WuG8dK4mW97I.yaws"
	],
	ProcessArgForGuest = fun( A ) ->
		Req = A#arg.req,
		{abs_path, Path} = Req#http_request.path,
		case (A#arg.req)#http_request.method of
			'GET' -> case lists:member( Path, GuestGetPaths ) of
					true -> A;
					false ->
						A#arg{ req = Req#http_request{ path = { abs_path, "/" } }, state = { abs_path, Path } }
				end;
			'POST' -> case lists:member( Path, GuestPostPaths ) of
					true -> A;
					false ->
						A#arg{ req = Req#http_request{ path = { abs_path, "/" } }, state = { abs_path, Path } }
				end;
			_ -> A#arg{ req = Req#http_request{ path = { abs_path, "/" } }, state = { abs_path, Path } }
		end
	end,
	SessionCookie = yaws_api:find_cookie_val( "sid", element( 14, Arg#arg.headers ) ),
	log( "arg_rewrite session cookie", [{string, SessionCookie}] ),
	file:write_file( "cook.txt", io_lib:format( "~p~n~p~n~p~n~p~n~p", [NewCookies, element( 14, Arg#arg.headers ), Arg, SessionCookie, yaws_api:cookieval_to_opaque( SessionCookie )] ) ),
	case SessionCookie of
		[] -> ProcessArgForGuest( Arg );
		_ ->
			CTO = yaws_api:cookieval_to_opaque( SessionCookie ),
			log( "arg_rewrite cookieval_to_opaque", [{terms, CTO}] ),
			case CTO of
				{error, _} -> ProcessArgForGuest( Arg );
				{ok, Sess} -> 
					case Sess#session_info.username of
						guest -> ProcessArgForGuest( Arg );
						admin -> Arg
					end
			end
	end
	.

blackListBlock() ->
	Form = {form, [ {action, helper:urlFor(updateBlackList)}, {method, post} ], [
			{input, [{type, submit}, {value, "????????????????"}], []}
		]},
	{ehtml, [Form, {pre, [], lists:foldl(fun(Id, Res) -> Res ++ "\n" ++ integer_to_list(Id) end, "", vkontakte:getBlackList())}]}.

