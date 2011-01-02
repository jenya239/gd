-module(statsTest).

-include("data.hrl").
-include("lib/eunit/include/eunit.hrl").

aggregate_test() ->
    List = [
        #activity{userid = 1, date = 1}, 
        #activity{userid = 1, date = 3},
        #activity{userid = 1, date = 11},
        #activity{userid = 2, date = 5}
    ],
    
    Expected = [
        {1, 0, 2},
        {1, 1, 1},
        {2, 0, 1}
    ],
    
    {_, _, Value} = lists:foldl(fun stats:aggregate/2, {0, 10, []}, List),
    
    ?assertEqual(Expected, lists:reverse(Value)).

groupByElement_test() ->
    List = [
        {1, 0, 2},
        {1, 1, 1},
        {2, 0, 1}
    ],
    
    Expected = [2, 1],
    {_, Value} = lists:foldl(fun stats:groupByElement/2, {1, []}, List),
    
    ?assertEqual(Expected, Value).
        