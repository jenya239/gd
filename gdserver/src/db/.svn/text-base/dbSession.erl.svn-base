-module(dbSession).

-include_lib("stdlib/include/qlc.hrl").
-include("data.hrl").

-export([create/1, write/1]).

create(UserID) ->
    Result = mnesia:transaction(fun() -> 
        Session = #session{id=dbUuid:get_nt(session), userID=UserID, startTime=utils:now()},
        mnesia:write(Session),
        Session
    end),
    
    case Result of
        {atomic, NewSession} ->
            NewSession;
        Other ->
            Other
    end.
    
write(Session) ->
    {atomic, ok} = mnesia:transaction(fun() -> 
        mnesia:write(Session)
    end).