-module(status).

-include("config.hrl").

-export([start/0,run/1,try_connect/2]).

-record(state, {host, port, email, period, mailed}).

start()->
	Config = utils:readConfig(?CONFIG_PATH),
	Host = get(Config, host),
	Port = get(Config, port),
	Email = get(Config, email),
	Period = get(Config, period),
	spawn(status,run,[#state{host = Host, email = Email, port = Port, period = Period, mailed = false }]).

run(State)->
	case try_connect(State#state.host, State#state.port) of
		ok->
            if
                State#state.mailed == true -> 
                    log:write(error, ?MODULE_STRING, "Server become alive: ~s~n", [State#state.host]),
                    smtp:sendMail(State#state.email, "Server become alive: " ++ State#state.host, "server monitor report");
                true ->
                    ok
            end,
                
            NewState = State#state{mailed = false},
            ok;
        Error-> 
            if  
                State#state.mailed == false -> 
                    ErrorStr = io_lib:format("~10000p", [Error]),
                    log:write(error, ?MODULE_STRING, "Server died: ~s ~s~n", [State#state.host, ErrorStr]),
                    Message = "Server on host " ++ State#state.host ++ " is died. Reason " ++ ErrorStr,
                    smtp:sendMail(State#state.email, Message, "server monitor report"),
                    NewState = State#state{mailed = true};
                true->
                    NewState=State 
            end
	end,
	timer:sleep(State#state.period),
	run(NewState).

get(Config,What) ->
	{_Some, {What, Result}} = lists:keysearch(What, 1, Config),
	Result.

try_connect(Host, Port)->
	try connect(Host,Port) of 	
		ok -> ok
	catch
        _Error:X -> {error, X}	
	end.
		
-define(TCP_OPTIONS, [list, {active, false}]).    
    
connect(Host,Port)->
    %%log:write(info, "Status connecting to ~s:~w~n", [ Host, Port]),	
    {ok, Socket} = gen_tcp:connect(Host, Port, ?TCP_OPTIONS),
    % receive server time
	{ok, _Result} = gen_tcp:recv(Socket, 0),
	ok.

