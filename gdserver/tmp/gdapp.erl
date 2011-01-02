-module(gdapp).

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-record(state, {gdsup}).

%%-----------------------------------------------------------------------------
%% Application callbacks
%%-----------------------------------------------------------------------------
start(_Type, [ConfigPath | _StartArgs]) ->
    case server:start_link(ConfigPath) of
        {ok, Pid} -> 
            {ok, Pid, #state{gdsup=Pid}};
        Error ->
            Error
    end.

prep_stop(State) ->
    serverSocket:stop(),
    State.

stop(_State) ->
    ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

