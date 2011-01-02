-module(dbMessage).
-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([
	getLatest/2,
	add/4,
	getAllPost/1,
	processPost/3
]).

-define(TIME_FOR_LAST_CHAT_IN_MINUTES, 15).
-define(MESSAGES_FOR_LAST_CHAT, 10).

getLatest(type, Type)->
	{atomic, Result} = mnesia:transaction(        
		fun() ->
            After = utils:now() - ?TIME_FOR_LAST_CHAT_IN_MINUTES*60*1000,
			FilteredHandle = qlc:q([Message || Message <- mnesia:table(message),
															Message#message.receiver =:= Type,
															Message#message.date > After]),

			SortedHandle = qlc:keysort(#message.date,  FilteredHandle, {order ,descending}),

			Cursor = qlc:cursor(SortedHandle),
			TopRecords = qlc:next_answers(Cursor, ?MESSAGES_FOR_LAST_CHAT),
			qlc:delete_cursor(Cursor),
			lists:reverse(TopRecords)
		end),
	Result.

add(From, To, Body, TimeStamp)->
	Message = #message{id=dbUuid:get(message), date=TimeStamp, from=From, receiver=To, body=Body},
	mnesia:dirty_write(Message),
    {ok, Message}.

getAllPost_nt(UserID) ->
	PListWithItems = qlc:e(qlc:q([P || P <- mnesia:table(postMessage), P#postMessage.recepientID =:= UserID orelse P#postMessage.senderID =:= UserID, length(P#postMessage.item) > 0])),
	PListWithoutItems = qlc:e(qlc:q([P || P <- mnesia:table(postMessage), P#postMessage.recepientID =:= UserID orelse P#postMessage.senderID =:= UserID, length(P#postMessage.item) =:= 0])),

	PList2WithItems = qlc:e(qlc:q([
			#postMessageInfo{
			id=P#postMessage.id,
			fromNick=U#user.name,
			itemInfo=[I],
			timeStamp=P#postMessage.timeStamp,
            sellPrice=P#postMessage.sellPrice,
			money=P#postMessage.money,
			comment=P#postMessage.comment,
            senderID=P#postMessage.senderID
            } || P <- PListWithItems, U <- mnesia:table(user), I <- mnesia:table(item), U#user.id =:= P#postMessage.senderID, lists:nth(1, P#postMessage.item) =:= I#item.id])),

	PList2WithoutItems = qlc:e(qlc:q([
			#postMessageInfo{
			id=P#postMessage.id,
			fromNick=U#user.name,
			itemInfo=[],
			timeStamp=P#postMessage.timeStamp,
			money=P#postMessage.money,
            sellPrice=0,
			comment=P#postMessage.comment,
            senderID=P#postMessage.senderID
			} || P <- PListWithoutItems, U <- mnesia:table(user), U#user.id =:= P#postMessage.senderID])),

	lists:keysort(#postMessageInfo.timeStamp, PList2WithItems ++ PList2WithoutItems).

getAllPost(UserID) ->
	{atomic, Result} = mnesia:transaction(
        fun() ->
            getAllPost_nt(UserID)
        end),

	Result.

getPostMessage_nt(PostMessageID) ->
	Result = qlc:e(qlc:q([P || P <- mnesia:table(postMessage), P#postMessage.id =:= PostMessageID])),
	case Result of
		[PostMessage] ->
			PostMessage;
		[] ->
			noPostMessage
	end.

processPost(Action, PostMessageID, UserID) ->
    Result = mnesia:transaction(
       fun() ->
           PostMessage = getPostMessage_nt(PostMessageID),
           MessageItems = PostMessage#postMessage.item,
           MessageMoney = PostMessage#postMessage.money,
           SellPrice = PostMessage#postMessage.sellPrice,          
                      
           case Action of
                accept ->                        
                    UserDetails = dbUser:getDetails_nt(UserID),
                    User = dbUser:getRecord_nt(id, UserID),
                    OldUserMoney = User#user.money,

                    if PostMessage#postMessage.recepientID =/= UserID ->
                        mnesia:abort({error, "[[cantReceiveMailThatIsNotForYou]]"});
                    true -> ok end,
                    
                    InventorySize = dbGlobal:get_nt(inventorySize),

                    if MessageItems =/= [] andalso length(UserDetails#userDetails.inventory + length(MessageItems)) > InventorySize ->
                        mnesia:abort({error, "[[inventoryIsFull]]"});
                    true -> ok end,

                    if SellPrice > 0 ->
                        SenderUser = dbUser:getRecord_nt(id, PostMessage#postMessage.senderID),
                        OldSenderUserMoney = SenderUser#user.money,
                        NewSenderUserMoney = OldSenderUserMoney + SellPrice,
                        mnesia:write(SenderUser#user{money = NewSenderUserMoney}),
                        dbActivity:register_nt(SenderUser#user.id, {moneyReceived, postMessage, OldSenderUserMoney, NewSenderUserMoney}, ok);
                    true -> ok end,

                    UserInventory = UserDetails#userDetails.inventory ++ MessageItems,
                    mnesia:write(UserDetails#userDetails{inventory=UserInventory}),

                    NewUserMoney = OldUserMoney + MessageMoney - SellPrice,

                    if NewUserMoney < 0 ->
                        mnesia:abort({error, "[[notEnoughMoney]]"});
                    true -> ok end,
                    mnesia:write(User#user{money=NewUserMoney}),
                    mnesia:delete(postMessage, PostMessageID, write),
                    dbActivity:register_nt(UserID, {processPostMessage, PostMessageID, Action, MessageItems, MessageMoney, OldUserMoney, NewUserMoney}, ok);

                cancel ->
                    SenderUserID = PostMessage#postMessage.senderID,
                    SenderUser = dbUser:getRecord_nt(id, SenderUserID),
                    SenderUserDetails = dbUser:getDetails_nt(SenderUserID),
                    OldSenderUserMoney = SenderUser#user.money,

                    if PostMessage#postMessage.senderID =/= UserID andalso PostMessage#postMessage.recepientID =/= UserID ->
                        mnesia:abort({error, "[[cantCancelThisMail]]"});
                    true -> ok end,
                    
                    InventorySize = dbGlobal:get_nt(inventorySize),

                    if MessageItems =/= [] andalso length(SenderUserDetails#userDetails.inventory + length(MessageItems)) > InventorySize ->
                        mnesia:abort({error, "[[inventoryIsFull]]"});
                    true -> ok end,                                            

                    SenderUserInventory = SenderUserDetails#userDetails.inventory ++ MessageItems,
                    mnesia:write(SenderUserDetails#userDetails{inventory=SenderUserInventory}),

                    NewSenderUserMoney = OldSenderUserMoney + MessageMoney,

                    mnesia:write(SenderUser#user{money=NewSenderUserMoney}),
                    mnesia:delete(postMessage, PostMessageID, write),
                    dbActivity:register_nt(SenderUserID, {processPostMessage, PostMessageID, Action, MessageItems, MessageMoney, OldSenderUserMoney, NewSenderUserMoney}, ok);

                _Other ->
                    mnesia:abort({error, "Invalid command"})
            end,
                            
            getAllPost_nt(UserID)
        end
    ),

	mneser:transactionResult(Result, UserID, {processPostMessage, PostMessageID}).
