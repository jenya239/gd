-module(allTest).
-export([main/0, createTestUsers/0]).
-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").
main() ->
    
    {StartMegaseconds, StartSeconds, StartMicroseconds} = erlang:now(),
    mneser:start(),
    createTestData(),
    test(),
    {EndMegaseconds, EndSeconds, EndMicroseconds} = erlang:now(),
    {TestMegaseconds, TestSeconds, TestMicroseconds} =
        {EndMegaseconds - StartMegaseconds, EndSeconds - StartSeconds, EndMicroseconds - StartMicroseconds},
    io:format("  Test run time: ~B.~6..0B seconds.~n", [TestMegaseconds * 1000 + TestSeconds, TestMicroseconds]).

createTestUsers() ->
    %% create test users
    {ok, User1} = registration:createUser(109830, "test" ,0, 1),
    {ok, User2} = registration:createUser(109831, "test1",0, 1),
    {ok, User3} = registration:createUser(109832, "test2",0, 1),
    createTestUser(User1, test),
    createTestUser(User2, test1),
    createTestUser(User3, test2).

createTestUser(User,Id) ->
    UserT = User#user{id = Id},
    UserDetails = mneser:getRecord(userDetails, User#user.id),
    UserDetailsT = UserDetails#userDetails{id = Id},
    mneser:writeRecord(UserT),
    mneser:writeRecord(UserDetailsT).

createTestData() ->
    createTestUsers(),
    {atomic, ok} = mnesia:transaction(
        fun() ->
            Car = registration:createCar(1, [], 1, 5),
            mnesia:write(Car),
            mnesia:write(#route{id=1, displayName="TestRoute", fileName="testRoute", length=1})
        end
    ).

all_test_() ->
    [
          {module, utilsTest}
        , {module, globalChatTest}
        , {module, mneserTest}
        , {module, clientTest}
        , {module, lobbyManagerTest}
     %   , {module, lobbyTest}
        , {module, statsTest}
        , {module, usersTest}
      
  ].
