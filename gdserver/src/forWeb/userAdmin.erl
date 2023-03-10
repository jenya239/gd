-module(userAdmin).
-export([process/1,
        processActivity/1,
        processAddAdminRole/1, 
        processRemoveAdminRole/1, 
        processGetAllIds/0, 
        processAddMoney/1, 
        processAddRealMoney/1, 
        processSearchByName/1, 
        processSearchByVkontakteId/1, 
        processChangeNickname/1,
        processGiveCar/1,
				processUnban/1,
			  processSwitchHomeCity/1,
				processChangeCarColor/1,
			  processAddItem/1,
			  processCarDelete/1,
			  processItemDelete/1,
			  processChangeLevel/1]).

-include("data.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

-define(MILLISECONDS_IN_HOUR, 3600000).

createRow(Name, Value) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [], case is_tuple(Value) of true -> Value; false -> utils:toStringForScalars(Value) end}
	]}.

showAtomList(LS) ->
    lists:map(fun atom_to_list/1, LS).
        
showTriggerList(LS) ->
    lists:map(fun(E) ->
        case is_atom(E) of
            true ->
                atom_to_list(E) ++ " ";
            false ->
                atom_to_list(element(1, E)) ++ " "
        end
    end, LS).

createItemsTable( User, Items2, DomId ) ->
	Items = lists:sort( fun( I1, I2 ) -> I1#item.id < I2#item.id end, Items2 ),
	{table, [{id, DomId}], lists:map( fun( Item ) ->
		Class = dbItem:getClass( Item#item.itemClassID ),
		helper:createRow([ Class#itemClass.name, utils:toString( round( Item#item.durability ) )
			++ " / " ++ utils:toString( Item#item.durabilityMax ),
		 {input,
				[ {type, button}, {class, "del"}, {id, utils:fmt( "btnItemDel~B", [Item#item.id] ) },
					{onclick,
						utils:fmt( "deleteItem( ~B, ~B, '~s' );",
							[ User#user.id, Item#item.id, helper:urlFor( item, delete ) ] ) }
				], [] } ])
end, Items )}.
createItemsTable( User, Items ) ->
	createItemsTable( User, Items, "" ).

createAddItemForm( UserId ) ->
	Classes = lists:sort( 
		fun( IC1, IC2 ) -> IC1#itemClass.id < IC2#itemClass.id end, mneser:getAllRecords( itemClass ) ),
	{span, [], [
		{select, [{id, "slctItemClass"}],
			lists:map( fun( IC ) -> {option, [{value, IC#itemClass.id}], IC#itemClass.name} end, Classes)
		},
		{input, [{type, button}, {value, "????????????????"},
			{onclick, utils:fmt( "addItem( ~B, '~s' )", [UserId, utils:toString( helper:urlFor( item, add ) )] )}], []}
	]
}.

createCarsTable( User, Cars ) ->
	{table, [], [
		helper:createRow(th, ["id", "??????????", "????????????", "????????", "??????????????????????"]),
		lists:map( fun(CarInfo) ->
			CarId = (CarInfo#carInfo.car)#car.id,
			CarClassId = (CarInfo#carInfo.carClass)#carClass.id,
			ColorId = (CarInfo#carInfo.car)#car.color,
			ChangingSpanStr = "<span class='color_changing'>"
				++ utils:toString( CarId ) ++ " "
				++ utils:toString( CarClassId ) ++ " "
				++ utils:toString( ColorId ) ++ "</span>",
			helper:createRow([
				(CarInfo#carInfo.car)#car.id,
				(CarInfo#carInfo.carClass)#carClass.displayName,
				utils:toString( round( (CarInfo#carInfo.car)#car.durability ) ) ++ " / "
					++ utils:toString( (CarInfo#carInfo.car)#car.durabilityMax ),
				ChangingSpanStr,
				lists:foldl( fun( Id, Str ) -> Str ++ utils:toString( Id ) end, [],
					(CarInfo#carInfo.car)#car.upgrades ),
				if CarId /=	User#user.currentCarID ->
					{input,
						[ {type, button}, {class, "del"}, {id, utils:fmt( "btnCarDel~B", [CarId] ) },
							{onclick,
								utils:fmt( "deleteCar( ~B, ~B, '~s' );",
									[ User#user.id, CarId, helper:urlFor( car, delete ) ] ) }
						], [] };
					true -> ""
				end
			])
		end, Cars)
	]}.

createRowLink(Name, Value, Href) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [], 
		    {a, [{href, Href}], utils:toStringForScalars(Value)}}
	]}.

createInput(Name, Value, Id, Url, Question) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [], 
		    {form, [ {action, Url}, {method, post}, {style, "margin: 0px"} ], [
         		{input, [{type, hidden}, {name, id}, {value, Id}], []},
         		{input, [{type, text}, {name, value}, {value, Value}], []}, 
         		{input, [{type, button}, {value, "Ok"}, {onclick, "try_submit(this.parentNode, '"++Question++"'); return false;"}], []}
            ]}
         }
	]}.
	
createSearch(Name, Value, Url) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [], 
		    {form, [ {action, Url}, {method, post}, {style, "margin: 0px"} ], [
         		{input, [{type, text}, {name, value}, {value, Value}], []}, 
         		{input, [{type, button}, {value, "??????????"}, {onclick, "this.parentNode.submit()"}], []}
            ]}
         }
	]}.
	
createBlankRow() ->
	{tr, [], [
		{td, [{align, right}], "&nbsp;"},
		{td, [], "&nbsp;"}
	]}.
	
createUserTable(Id) ->
	Rec = mneser:getRecord(user, Id),
	Car = mneser:getRecord(car, Rec#user.currentCarID),
	Cars = dbCar:getUserCars(Id),
  {Inventory, Equipment} = dbItem:getUserInventoryAndEquipment(Id),
	UserProgress = dbUserProgress:getOrCreate(Id),
	{table, [], [
		createSearch("?????????? ???? ??????????", Rec#user.name, helper:urlFor(user, searchByName)),
		createSearch("?????????? ???? vkontakteId", Rec#user.vkontakteID, helper:urlFor(user, searchByVkontakteId)),
		createBlankRow(),
		createRow("Id", {a, [ {href, helper:urlFor(user, show, Id)}], utils:toStringForScalars(Id)}),
		createInput("??????", Rec#user.name, Rec#user.id, helper:urlFor(user, changeNickname), "?????????? ???????????????? ???????"),
		createRow("??????????", formatCity(Rec)),
		createInput("????????????", utils:toString(utils:trunc(Rec#user.money, 2)), Rec#user.id, helper:urlFor(user, addMoney), "?????????? ???????????????? ???????????"),
		createInput("??????????????", utils:toString(utils:trunc(Rec#user.realMoney, 2)), Rec#user.id, helper:urlFor(user, addRealMoney), "?????????? ???????????????? ?????????????? (??????????????!!) ?"),
		createRow("??????????????????????????????????", utils:timestampToHumanString(Rec#user.date)),
		createRow("??????????????", Car#car.fuel),
		createInput("??????????????", utils:toString(Rec#user.level), Rec#user.id, helper:urlFor(user, changeLevel), "?????????? ???????????????? ???????????????"),
		createRow("??????????????", Rec#user.rating),
		createRow("????????", Rec#user.experience),
		createRow("??????????", Car#car.nitroCount),
		createRow("?????????? ????????????", UserProgress#userProgress.onlineTime / ?MILLISECONDS_IN_HOUR),
		createRow("??????????????", UserProgress#userProgress.kilometers),
		createRow("??????????????", UserProgress#userProgress.worksCounter),
		createRow("????????????????", showTriggerList(Rec#user.triggers)),
		createRow("????????", showAtomList(Rec#user.roles) ),
		createRow("????????????", createCarsTable( Rec, Cars ) ),
		createRow("??????????????????", {'div', [], [createItemsTable( Rec, Inventory, "tblInventory" ), createAddItemForm( Id )]}),
		createRow("????????????", createItemsTable(Rec, Equipment)),
		createRowLink("Vkontakte", Rec#user.vkontakteID, "http://vkontakte.ru/id" ++ integer_to_list(Rec#user.vkontakteID)),
		createInputGiveCar("???????????? ????????",Rec#user.id,helper:urlFor(user, give), "?????????? ???????????? ?????????"),
		createRowLink("Activity", "Activity", "/editor/activity.yaws?id=" ++ integer_to_list(Id)),
		createRow("?????????????????? ??????", formatLastBan(Id))
	]}.

formatCity(Rec) ->
		{span, [], [
			utils:toStringForScalars(Rec#user.homeCity),
			{form, [ {action, helper:urlFor(user, switchHomeCity)}, {method, post}, {style, "margin: 0px; float: right;"} ], [
				{input, [{type, hidden}, {name, id}, {value, Rec#user.id}], []},
				{input, [{type, button}, {value, "??????????????"}, {style, "height: 18px;"}, {onclick, "try_submit(this.parentNode, '?????????? ???????????????'); return false;"}], []}
			]}
		]}.

formatLastBan(UserId) ->
		{atomic, Ban} = mnesia:transaction(fun() -> 
			Bans = mnesia:read({stopList, UserId}),
			case Bans of
					[] -> none;
					_ -> lists:last( Bans )
			end
		end),
		{span, [],
			case Ban of
					none -> ["none"];
					_ -> [
						utils:timestampToHumanString(utils:microsecsToTimestamp(element(3, Ban) * 1000))
							++ " ???? " ++ utils:sec_to_string( round( element(4, Ban) / 1000 ) )
							++ " by " ++ helper:linkFor(user, show, Ban#stopList.admin),
						{form, [ {action, helper:urlFor(user, unban)}, {method, post}, {style, "margin: 0px; float: right;"} ], [
							{input, [{type, hidden}, {name, id}, {value, UserId}], []},
							{input, [{type, button}, {value, "unban"}, {style, "height: 18px;"}, {onclick, "try_submit(this.parentNode, '?????????? ???????????????????'); return false;"}], []}
						]}
					]
			end
		}.

createInputGiveCar(Name, Id, Url, Question) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [],
		    {form, [ {action, Url}, {method, post}, {style, "margin: 0px"} ], [
         		{input, [{type, hidden}, {name, id}, {value, Id}], []},
         		{input, [{type, text}, {name, carClassID}, {value, carClassID}], []},
            {input, [{type, text}, {name, color}, {value, color}], []},
         		{input, [{type, button}, {value, "????????????"}, {onclick, "try_submit(this.parentNode, '"++Question++"'); return false;"}], []}
            ]}
         }
	]}.

createUserTables(VkontakteId) ->
	Users = dbUser:getRecords(vkontakteId, VkontakteId),
	Tables = lists:map(
		fun(User) ->
			{list_to_atom("div"), [ {class, user} ], [
				createUserTable(User#user.id),
				tableEditorView:createDeleteForm(user, User#user.id)
			]}
		end,
		Users
	),
	{p, [], [
		{ h1, [], utils:toStringForScalars(length(Users)) },
		Tables
	]}.

formatAction(
  {buyItem, ItemClassID, ItemPrice, ItemRealPrice, OldUserMoney, OldUserRealMoney, NewUserMoney, NewUserRealMoney}
) ->
  io_lib:format("buyItem <a href='~s'>~b</a>: ???????? ???????????? ~p??????, ~p??; ???????? ~p??????, ~p??; ?????????? ~p??????, ~p??",
      [helper:urlFor(itemClass, edit, ItemClassID), ItemClassID, ItemPrice, ItemRealPrice, OldUserMoney, OldUserRealMoney, NewUserMoney, NewUserRealMoney]
  );
formatAction(
  {sellItem, ItemClassID, ItemPrice, OldUserMoney, NewUserMoney}
) ->
  io_lib:format("sellItem <a href='~s'>~b</a>: ???????? ???????????? ~p??????; ???????? ~p??????; ?????????? ~p??????",
      [helper:urlFor(itemClass, edit, ItemClassID), ItemClassID, ItemPrice, OldUserMoney, NewUserMoney]
  );
formatAction(
  {deleteItem, ItemID, ItemClassID}
) ->
  io_lib:format("buyItem <a href='~s'>~b</a>: ~b",
      [helper:urlFor(itemClass, edit, ItemClassID), ItemClassID, ItemID]
  );
formatAction(
  {buyFuel, Price, OldFuel, NewFuel, OldMoney, NewMoney}
) ->
  io_lib:format("buyFuel: price ~p??????, ???????? ?????????????? ~p, ?????????? ~p; ???????? ~p??????, ?????????? ~p??????",
      [Price, OldFuel, NewFuel, OldMoney, NewMoney]
  );
formatAction(
  {consumeFuel, OldFuel, NewFuel}
) ->
  io_lib:format("consumeFuel: ???????? ?????????????? ~p, ?????????? ~p",
      [OldFuel, NewFuel]
  );
formatAction(
  {workFuel, OldFuel, NewFuel}
) ->
  io_lib:format("workFuel: ???????? ?????????????? ~p, ?????????? ~p",
      [OldFuel, NewFuel]
  );
formatAction(
  {newLevel, NewLevel}
) ->
  io_lib:format("newLevel: ~b",
      [NewLevel]
  );
formatAction(
  {repair, ItemID, OldMoney, NewMoney}
) ->
  io_lib:format("repair: ~b; ???????? ~p??????, ?????????? ~p??????",
      [ItemID, OldMoney, NewMoney]
  );
formatAction(Action) ->
  io_lib:format("~p", [Action]).

createActivitiesTable(UserId, Offset, Limit) ->
  Activities = dbActivity:getAllUserActivities(UserId, Offset, Limit),
  PrevHtml = if Offset > 0 ->
      NewOffset = if Offset > Limit ->
          Offset - Limit;
        true ->
           0
        end,
      {a, [{href, io_lib:format("?id=~w&offset=~w&limit=~w", [UserId, NewOffset, Limit])}], "????????."};
    true ->
       {span, [], "????????."}
    end,
   
    NextHtml = {a, [{href, io_lib:format("?id=~w&offset=~w&limit=~w", [UserId, Offset + Limit, Limit])}], "????????."},
  
  {table, [ {cellpadding, 4} ], [
      helper:createRow(th, [
          "??????????",
          "????????????????",
          "??????????????????"
      ]),
      lists:map(
          fun({ Microsecs, Action, Result }) ->
              helper:createRow([
                  utils:timestampToHumanString( utils:microsecsToTimestamp( Microsecs ) ),
                  formatAction(Action),
                  utils:toString(Result)
              ])
          end,
          Activities
      ),       
      helper:createRow([PrevHtml, NextHtml])
  ]}.

createSingleUserPage(Id, _Offset, _Limit) ->
    case erlang:is_number(Id) of
        true ->
        	{p, [], [
        		createUserTable(Id),
        		{a, [ {href, helper:urlFor(user, show, mults, dbUser:getVkontakteId(Id))} ], "????????????"},
        		tableEditorView:createButtonForm(
        			"?????????????????? ??????????????",
        			helper:urlFor(user, addAdminRole),
        			"?????????? ?????????????????? id "++utils:toStringForScalars(Id)++" ???????????????",
        			Id,0
        		),
                tableEditorView:createButtonForm(
        			"?????????????????????? ???? ??????????????",
        			helper:urlFor(user, removeAdminRole),
        			"?????????? ?????????????????????? id "++utils:toStringForScalars(Id)++" ???? ???????????????",
        			Id,0
        		)
        	]};
    	false ->
    	    case Id of
    	        "notFound" ->
                    {p, [], "User not found"};
        	    "userExists" ->
                    {p, [], "User already exists"};
        	    _Other ->
        	        {p, [], "?? ???????? ???? ?????????? ???? ?????? ??????????????????"}
	        end
   end.

processAddAdminRole(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	dbUser:addAdminRole(Id),
	{ redirect, helper:urlFor( user, show, Id ) }.

processGiveCar(Arg) ->
	UserID = helper:getPOSTValue(Arg, id),
  CarClassID = helper:getPOSTValue(Arg, carClassID),
  ColorID = helper:getPOSTValue(Arg, color),
	dbCar:give(UserID, CarClassID, ColorID),
	{ redirect, helper:urlFor( user, show, UserID ) }.

processRemoveAdminRole(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	dbUser:removeAdminRole(Id),
	{ redirect, helper:urlFor( user, show, Id ) }.
	
processAddMoney(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	Value = helper:getPOSTValue(Arg, value),
	dbUser:setMoney(Id, Value),
	{redirect, helper:urlFor( user, show, Id )}.

processAddRealMoney(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	Value = helper:getPOSTValue(Arg, value),
	dbUser:setRealMoney(Id, Value),
	{redirect, helper:urlFor(user, show, Id)}.
	
processSearchByName(Arg) ->
    Name = helper:getPOSTValue(Arg, value),
    User = dbUser:getRecord(nicknameIgnoreCase, Name),
    case erlang:is_record(User, user) of
        true ->
            {redirect, helper:urlFor(user, show, User#user.id)};
        false ->
            {redirect, helper:urlFor(user, show, notFound)}
    end.
    
processSearchByVkontakteId(Arg) ->
    ID = helper:getPOSTValue(Arg, value),
    User = dbUser:getRecord(vkontakteID, ID),
    case erlang:is_record(User, user) of
        true ->
            {redirect, helper:urlFor(user, show, User#user.id)};
        false ->
            {redirect, helper:urlFor(user, show, notFound)}
    end.
    
processChangeNickname(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	Value = helper:getPOSTValue(Arg, value),
	Result = dbUser:changeNickname(Id, Value),
	case Result of 
	    ok ->
	        {redirect, helper:urlFor(user, show, Id)};
        {error, userExists} ->
            {redirect, helper:urlFor(user, show, userExists)};
        {error, notFound} ->
            {redirect, helper:urlFor(user, show, notFound)}
    end.

process(Arg) ->
  [
		"<script type='text/javascript'>var car_color_change_url = '"
			++ helper:urlFor( car, changeColor ) ++ "';</script>",
		case helper:paramGETExists(Arg, id) of
			true  -> createSingleUserPage(helper:getGETValue(Arg, id), 0, 10);
			false -> createUserTables(helper:getGETValue(Arg, vkontakteId))
		end,
		{br, [], []},
		{br, [], []}
	].

processActivity(Arg) ->
    [
    	case helper:paramGETExists(Arg, id) of
    		true -> 
    		    Offset = case helper:paramGETExists(Arg, offset) of
    		        true ->
    		            helper:getGETValue(Arg, offset);
    	            false ->
    	              0
                end,
                {p, [], [createActivitiesTable(helper:getGETValue(Arg, id), Offset, 10000)]};
    		false -> {p, [], "No User"}
    	end,
    	{br, [], []},
    	{br, [], []}
    ].

processGetAllIds() ->
	{html, utils:termToJson( dbUser:getAllIds() )}.

processUnban(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	mnesia:transaction( fun() ->
		mnesia:delete_object(	lists:last( mnesia:read({stopList, Id}) ) )
	end ),
	{redirect, helper:urlFor(user, show, Id)}.

processSwitchHomeCity(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	mnesia:transaction( fun() ->
		Rec = dbUser:getRecord_nt(id, Id),
		NewCity = case Rec#user.homeCity of
			1 -> 2;
			2 -> 1
    end,
    mnesia:write(Rec#user{homeCity=NewCity, currentCity=NewCity})
	end ),
	{redirect, helper:urlFor(user, show, Id)}.

processChangeCarColor( Arg ) ->
	Id = helper:getPOSTValue( Arg, id ),
	NewColor = helper:getPOSTValue(Arg, newColor),
	mnesia:transaction( fun() ->
		Rec = mneser:getRecord_nt( car, Id ),
		mnesia:write( Rec#car{color=NewColor} )
	end ),
	{html, utils:toString( NewColor )}.

processAddItem( Arg ) ->
	UserId = helper:getPOSTValue( Arg, userId ),
	ItemClassId = helper:getPOSTValue(Arg, itemClassId),
	ItemClass = dbItem:getClass( ItemClassId ),
	{atomic, ItemId} = mnesia:transaction( fun() ->
		UserDetails = dbUser:getDetails_nt( UserId ),
		NewItem = #item{ id=dbUuid:get_nt(item), itemClassID=ItemClassId, durability=ItemClass#itemClass.durabilityMax, durabilityMax=ItemClass#itemClass.durabilityMax },
    NewUserDetails = UserDetails#userDetails{ inventory=UserDetails#userDetails.inventory ++ [NewItem#item.id] },
    mnesia:write( NewItem ),
    mnesia:write( NewUserDetails ),
    dbActivity:register_nt( UserId, {adminAddItem, ItemClassId, NewItem#item.id}, ok ),
		NewItem#item.id
	end ),
	{content, "application/json; charset=utf8",
    utils:fmt( "{itemClassName: '~s', durability: ~p, durabilityMax: ~p, itemId: ~p}", [
			ItemClass#itemClass.name, ItemClass#itemClass.durabilityMax, ItemClass#itemClass.durabilityMax, ItemId] )}.

processCarDelete( Arg ) ->
	UserId = helper:getPOSTValue( Arg, userId ),
	CarId = helper:getPOSTValue(Arg, carId),
	mnesia:transaction( fun() ->
		User = mneser:getRecord_nt(user, UserId),
		UserDetails = mneser:getRecord_nt(userDetails, UserId),
		Car = mneser:getRecord_nt(car, CarId),
		Cars = UserDetails#userDetails.cars,
		NewCars = lists:delete( CarId, Cars ),
		CurrentCarID =
				case( CarId =:= User#user.currentCarID ) of
						true ->
							 lists:nth( 1, NewCars );
						false ->
							 User#user.currentCarID
				end,
		NewUser = User#user{currentCarID = CurrentCarID},
		mnesia:write( NewUser ),
		mnesia:write( UserDetails#userDetails{ cars = NewCars } ),
		lists:foreach(fun dbItem:deleteItem_nt/1, Car#car.upgrades),
		mnesia:delete(car, CarId, write),
		dbActivity:register_nt( UserId, {adminDeleteCar, CarId, Car#car.carClassID}, ok)
	end ),
{html, "ok"}.

processItemDelete( Arg ) ->
	UserId = helper:getPOSTValue( Arg, userId ),
	ItemId = helper:getPOSTValue(Arg, itemId),
	mnesia:transaction( fun() ->
		UserDetails = mneser:getRecord_nt(userDetails, UserId),
		NewInventory = lists:delete( ItemId, UserDetails#userDetails.inventory ),
		mnesia:write( UserDetails#userDetails{ inventory = NewInventory } ),
		mnesia:delete(item, ItemId, write),
		dbActivity:register_nt( UserId, {adminDeleteItem, ItemId}, ok)
	end ),
	{html, "ok"}.
	
processChangeLevel(Arg) ->
	Id = helper:getPOSTValue(Arg, id),
	Value = helper:getPOSTValue(Arg, value),
	mnesia:transaction( fun() -> 
		User = dbUser:getRecord_nt(id, Id),
		mnesia:write( User#user{level=Value} )
	end ),
	{redirect, helper:urlFor( user, show, Id )}.