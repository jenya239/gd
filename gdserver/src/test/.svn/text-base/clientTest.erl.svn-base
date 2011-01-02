-module(clientTest).
-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").

getClientInfo_logged_out_test() ->
    {ok, ClientPID} = client:start(nosocket, 1),
    ?assertMatch(error, element(1, client:getClientInfo(ClientPID))),
    exit(ClientPID, kill).

getClientInfo_logged_in_test() ->
    {ok, GlobalChatPID} = globalChat:start_link(),

    allTest:createTestUsers(),

    {ok, ClientPID} = client:start(nosocket, 1),
    ClientPID ! {authorizeVkontakte, 105739, "a53cf681affa7bc4fb6e23f8b8a866a2", "Test test"},
    ?assert(is_record(client:getClientInfo(ClientPID), clientInfo)),
    
    exit(GlobalChatPID, kill),
    exit(ClientPID, kill),
    mnesia:clear_table(user).