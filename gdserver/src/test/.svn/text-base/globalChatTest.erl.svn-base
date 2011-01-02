-module(globalChatTest).
-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").

addAndRemoveClient_test() ->
    {ok, ChatPID} = globalChat:start_link(noRegister),
    
    %empty
    Messages0 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(0, length(Messages0)),
    
    %one element
    ChatPID ! {addClient, global, #clientInfo{clientID=1}, self()},
    Messages1 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(1, length(Messages1)),
    
    %two elements
    ChatPID ! {addClient, global, #clientInfo{clientID=2}, noPID},
    Messages2 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(2, length(Messages2)),
    Messages2_except = globalChat:getAllAddClientMessagesExceptOne(ChatPID, 1),
    ?assertMatch(1, length(Messages2_except)),
    
    %remove 2nd
    ChatPID ! {removeClient, global, 2},
    Messages1_2 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(1, length(Messages1_2)),
    Messages1_except = globalChat:getAllAddClientMessagesExceptOne(ChatPID, 1),
    ?assertMatch(0, length(Messages1_except)),
    
    %remove 1st
    ChatPID ! {removeClient, global, 1},
    Messages0_2 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(0, length(Messages0_2)),
    
    %remove unexistent
    ChatPID ! {removeClient, global, 1000},
    Messages0_3 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    %chat process must not fall and be responsible
    ?assertMatch(0, length(Messages0_3)),
    
    exit(ChatPID, kill).

chatMessage_test() ->
    {ok, ChatPID} = globalChat:start_link(noRegister),
    
    %add client
    ChatPID ! {addClient, global, #clientInfo{clientID=1}, self()},
    %receive broadcast
%    ChatPID ! {chatMessage, global, "Text", 0},
%    receive
%        {otherChatMessage, global, "Text", 0} ->
%            ?assert(true)
%    end,
%    
%    %don't receive broadcast from yourself
%    ChatPID ! {chatMessage, global, "Text", 1},
%    receive
%        after 1000 ->
%            ?assert(true)
%    end,
%    
    exit(ChatPID, kill).
    

%    %receive broadcast
%    ChatPID ! {chatMessage, global, "Text", 0},
%    receive
%        {otherChatMessage, global, "Text", 0} ->
%            ?assert(true)
%    end,
%    
%    %don't receive broadcast from yourself
%    ChatPID ! {chatMessage, global, "Text", 1},
%    receive
%        after 1000 ->
%            ?assert(true)
%    end,
%    
%    exit(ChatPID, kill).
%    
%
addRemoveBroadcast_test() ->
    {ok, ChatPID} = globalChat:start_link(noRegister),
    
    %empty
    Messages0 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(0, length(Messages0)),
    
    %one element
    ChatPID ! {addClient, global, #clientInfo{clientID=1}, self()},
    Messages1 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(1, length(Messages1)),
       
    %two elements
    ChatPID ! {addClient, global, #clientInfo{clientID=2}, noPID},
    Messages2 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(2, length(Messages2)),
    receive
        {addClient, global, #clientInfo{clientID=2}} ->
            ?assert(true)
        after 1000 ->
            ?assert(false)
    end,
    
    %remove
    ChatPID ! {removeClient, global, 2},
    Messages1_2 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    ?assertMatch(1, length(Messages1_2)),
    receive
        {removeClient, global, 2} ->
            ?assert(true)
        after 1000 ->
            ?assert(false)
    end,
    
    %remove unexistent
    ChatPID ! {removeClient, global, 1000},
    Messages0_2 = globalChat:getAllAddClientMessagesExceptOne(ChatPID, -1),
    %chat process must not fall and be responsible
    ?assertMatch(1, length(Messages0_2)),
    %no messages for removeing unexistent client
    receive
        after 1000 ->
            ?assert(true)
    end,
    
    exit(ChatPID, kill).
    
getClientPID_test() ->
    {ok, ChatPID} = globalChat:start_link(noRegister),

    ClientPID = self(),

    ChatPID ! {addClient, global, #clientInfo{clientID=1}, ClientPID},
    
    ?assertMatch({error, _Reason}, globalChat:getClientPID(ChatPID, 2)),
    ?assertMatch({ok, ClientPID}, globalChat:getClientPID(ChatPID, 1)),

    exit(ChatPID, kill).
    