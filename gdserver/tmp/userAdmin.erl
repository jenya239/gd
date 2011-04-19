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
        processGiveCar/1]).

-include("data.hrl").
-include_lib("lib/yaws/include/yaws_api.hrl").

-define(MILLISECONDS_IN_HOUR, 3600000).

createRow(Name, Value) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [], utils:toStringForScalars(Value)}
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

createItemsTable(Items) ->
	{table, [], lists:map(
		fun(Item) ->
            Class = dbItem:getClass(Item#item.itemClassID),
			createRow(Class#itemClass.name, Item#item.durability)
		end,
		Items
	)}.
	
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
         		{input, [{type, button}, {value, "Найти"}, {onclick, "this.parentNode.submit()"}], []}
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
  {Inventory, Equipment} = dbItem:getUserInventoryAndEquipment(Id),
	UserProgress = dbUserProgress:getOrCreate(Id),
    {table, [], [
        createSearch("Найти по имени", Rec#user.name, helper:urlFor(user, searchByName)),
        createSearch("Найти по vkontakteId", Rec#user.vkontakteID, helper:urlFor(user, searchByVkontakteId)),
        createBlankRow(),
		createRow("Id", {a, [ {href, helper:urlFor(user, show, Id)}], utils:toStringForScalars(Id)}),
		createInput("Имя", Rec#user.name, Rec#user.id, helper:urlFor(user, changeNickname), "точно изменить ник?"),
		createRow("Город", Rec#user.homeCity),
		createInput("Рублей", utils:toString(utils:trunc(Rec#user.money, 2)), Rec#user.id, helper:urlFor(user, addMoney), "точно изменить рубли?"),
		createInput("Золотых", utils:toString(utils:trunc(Rec#user.realMoney, 2)), Rec#user.id, helper:urlFor(user, addRealMoney), "точно изменить золотые (ЗОЛОТЫЕ!!) ?"),
		createRow("Зарегистрировался", utils:timestampToHumanString(Rec#user.date)),
		createRow("Бензина", Car#car.fuel),
		createRow("Уровень", Rec#user.level),
		createRow("Рейтинг", Rec#user.rating),
		createRow("Опыт", Rec#user.experience),
		createRow("Нитро", Car#car.nitroCount),
		createRow("Время онлайн", UserProgress#userProgress.onlineTime / ?MILLISECONDS_IN_HOUR),
		createRow("Проехал", UserProgress#userProgress.kilometers),
		createRow("Работал", UserProgress#userProgress.worksCounter),
        createRow("Триггеры", showTriggerList(Rec#user.triggers)),
        createRow("Роли", showAtomList(Rec#user.roles) ),
		createRow("Инвентарь", createItemsTable(Inventory)),
		createRow("Надето", createItemsTable(Equipment)),
		createRowLink("Vkontakte", Rec#user.vkontakteID, "http://vkontakte.ru/id" ++ integer_to_list(Rec#user.vkontakteID)),
        createInputGiveCar("Выдать авто",Rec#user.id,helper:urlFor(user, give), "точно выдать авто?"),
        createRowLink("Activity", "Activity", "activity.yaws?id=" ++ integer_to_list(Id))
	]}.

createInputGiveCar(Name, Id, Url, Question) ->
	{tr, [], [
		{td, [{align, right}], Name ++ ":&nbsp;"},
		{td, [],
		    {form, [ {action, Url}, {method, post}, {style, "margin: 0px"} ], [
         		{input, [{type, hidden}, {name, id}, {value, Id}], []},
         		{input, [{type, text}, {name, carClassID}, {value, carClassID}], []},
            {input, [{type, text}, {name, color}, {value, color}], []},
         		{input, [{type, button}, {value, "выдать"}, {onclick, "try_submit(this.parentNode, '"++Question++"'); return false;"}], []}
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
  io_lib:format("buyItem <a href='~s'>~b</a>: вещь стоила ~pруб, ~pз; было ~pруб, ~pз; стало ~pруб, ~pз",
      [helper:urlFor(itemClass, edit, ItemClassID), ItemClassID, ItemPrice, ItemRealPrice, OldUserMoney, OldUserRealMoney, NewUserMoney, NewUserRealMoney]
  );
formatAction(
  {sellItem, ItemClassID, ItemPrice, OldUserMoney, NewUserMoney}
) ->
  io_lib:format("sellItem <a href='~s'>~b</a>: вещь стоила ~pруб; было ~pруб; стало ~pруб",
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
  io_lib:format("buyFuel: price ~pруб, было топлива ~p, стало ~p; было ~pруб, стало ~pруб",
      [Price, OldFuel, NewFuel, OldMoney, NewMoney]
  );
formatAction(
  {consumeFuel, OldFuel, NewFuel}
) ->
  io_lib:format("consumeFuel: было топлива ~p, стало ~p",
      [OldFuel, NewFuel]
  );
formatAction(
  {workFuel, OldFuel, NewFuel}
) ->
  io_lib:format("workFuel: было топлива ~p, стало ~p",
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
  io_lib:format("repair: ~b; было ~pруб, стало ~pруб",
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
      {a, [{href, io_lib:format("?id=~w&offset=~w&limit=~w", [UserId, NewOffset, Limit])}], "Пред."};
    true ->
       {span, [], "Пред."}
    end,
   
    NextHtml = {a, [{href, io_lib:format("?id=~w&offset=~w&limit=~w", [UserId, Offset + Limit, Limit])}], "След."},
  
  {table, [ {cellpadding, 4} ], [
      helper:createRow(th, [
          "Когда",
          "Действие",
          "Результат"
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
        		{a, [ {href, helper:urlFor(user, show, mults, dbUser:getVkontakteId(Id))} ], "мульты"},
        		tableEditorView:createButtonForm(
        			"назначить админом",
        			helper:urlFor(user, addAdminRole),
        			"точно назначить id "++utils:toStringForScalars(Id)++" админом?",
        			Id,0
        		),
                tableEditorView:createButtonForm(
        			"разжаловать из админов",
        			helper:urlFor(user, removeAdminRole),
        			"точно разжаловать id "++utils:toStringForScalars(Id)++" из админов?",
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
        	        {p, [], "Я ваще не понял че щас произошло"}
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
