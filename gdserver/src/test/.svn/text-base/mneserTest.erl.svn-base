-module(mneserTest).
-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").

add_user_test()->
    UserCount = length(dbUser:getAll()),
    ?assertMatch({ok, #user{name="add_user_test1"}}, dbUser:add(#user{id=dbUuid:get(user), name="add_user_test1"})),
    ?assertMatch({userAlreadyExsists, "add_user_test1" }, dbUser:add(#user{id=dbUuid:get(user), name="add_user_test1"})),
    ?assertMatch({ok, #user{name="add_user_test2"}}, dbUser:add(#user{id=dbUuid:get(user), name="add_user_test2"})),
    ?assertMatch(2, length(dbUser:getAll()) - UserCount),
    
    mnesia:clear_table(user).

check_date_test() ->
    ?assertMatch(true,mneser:checkDate(0,erlang:now())).

get_no_user_test() ->
    UserID = dbUuid:get(user),
    ?assertMatch({error, noUserID, UserID}, dbUser:getRecord(id, UserID)).

get_user_test() ->
    UserID = dbUuid:get(user),
    dbUser:add(#user{id=UserID, name="get_user_test"}),
    ?assertMatch(#user{name="get_user_test"}, dbUser:getRecord(id, UserID)),
    
    mnesia:clear_table(user).

getuuid_test()->
    ?assertMatch(1, dbUuid:get("qwerty")),
    ?assertMatch(2, dbUuid:get("qwerty")),   
    ?assertMatch(1, dbUuid:get("qwerty2")),
    ?assertMatch(3, dbUuid:get("qwerty")).
    
getRatings_test() ->    

    dbUser:add(#user{id=dbUuid:get(user), name="getRatings_test1", rating=1, homeCity=1}),
    dbUser:add(#user{id=dbUuid:get(user), name="getRatings_test2", rating=3, homeCity=1}),
    dbUser:add(#user{id=dbUuid:get(user), name="getRatings_test3", rating=2, homeCity=1}),
    dbUser:add(#user{id=dbUuid:get(user), name="getRatings_test4", rating=2, homeCity=2}),
      
    Ratings2 = dbUser:getRatings(1),
    ?assertEqual(3, length(Ratings2)),
    
    ?assertEqual(3, (lists:nth(1, Ratings2))#ratingInfo.rating),
    ?assertEqual(2, (lists:nth(2, Ratings2))#ratingInfo.rating),
    ?assertEqual(1, (lists:nth(3, Ratings2))#ratingInfo.rating),
    
    Ratings3 = dbUser:getRatings(2),
    ?assertEqual(1, length(Ratings3)),
    
    mnesia:clear_table(user).
    
findAndUpdate_test() ->
    Fun = fun(Item) -> {Item#item{itemClassID=3}, anyValue} end,
    ?assertMatch(false, dbItem:findAndUpdate(1, Fun, [])),
    ?assertMatch(false, dbItem:findAndUpdate(1, Fun, [#item{id=2}])),
    ?assertMatch({ok, [#item{id=2, itemClassID=3}], anyValue}, dbItem:findAndUpdate(2, Fun, [#item{id=2, itemClassID=2}])).


