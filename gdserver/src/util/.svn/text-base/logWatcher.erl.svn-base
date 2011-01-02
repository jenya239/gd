-module(logWatcher).

-include("data.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
start_link(Mail) ->    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Mail], []).
    
%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([Mail | _Args]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    
    mneser:start(),
    {_,_,Day} =  erlang:date(),
    timer:send_after(60*60*1000, self(), timer),
    {ok, {Mail,Day}}.

%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle cast
%%-----------------------------------------------------------------------------
handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle info
%%-----------------------------------------------------------------------------
handle_info(timer, {Mail, Day}) ->
    {_,_,Now} = erlang:date(),
    NewState = if 
       Now =:= Day ->
           {Mail,Day};
       true ->
          processInfo(Mail),
          {Mail,Now}
    end,
    
    {noreply, NewState}.

%%-----------------------------------------------------------------------------
%% Code change
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%% Terminate
%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
processInfo(Mail)->
  smtp:sendMail(Mail, prepareMailBody(), "Global chat daily log").

prepareMailBody()->
    After = utils:now() - 24*60*60*1000,
    Messages = dbMessage:getLatest(type, global, 10000, After),
    MailBody = lists:foldl(fun(Message, Acc) ->
        User = dbUser:getRecord(id,Message#message.from),
        
        Result= 
            User#user.name ++ ": " ++ Message#message.body ++ [10],
        Acc++Result
        
    end, "", Messages),
    
   lists:flatten(MailBody).