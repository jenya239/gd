-module(vkontakte).

-behaviour(gen_server).

-include("data.hrl").
-include("config.hrl").

-export([start_link/5]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([getTransactionHistory/0, 
        authorize/4, 
        setStatus/2, 
        getVkontakteInfo/1, 
        exchangeVkontakteVotes/3, 
        addVkontakteVotes/4,
        startSendingMessage/3,
        startSendingMessage/2, 
        addMessagesCount/1, 
        isSendingMessages/0, 
        getLastResponse/0, 
        stopSendingMessage/0, 
        buyVkontakteRating/4, 
        sendVkontakteRating/4,
        uploadLanguageFile/1,
        setVkontakteLanguageValue/2,
        updateWhiteList/0,
        getWhiteList/0,
        updateBlackList/0,
        getBlackList/0]).
        
-record(state, {appID, appSecret, vkAppID, vkAppSecret, checkAuth, messagingPID, lastResult="", messagesCount=0, whiteList, blackList}).

start_link(AppID, AppSecret, VkAppID, VkAppSecret, CheckAuth) ->    
    gen_server:start_link({local, ?MODULE}, ?MODULE, [AppID, AppSecret, VkAppID, VkAppSecret, CheckAuth], []).
    
getTransactionHistory() ->
    gen_server:call(?MODULE, getTransactionHistory).

authorize(VkontakteID, AuthKey, FirstLastName, VkontakteOwnerID) ->
    gen_server:cast(?MODULE, {authorize, VkontakteID, AuthKey, FirstLastName, VkontakteOwnerID, self()}).
    
setStatus(VkontakteID, Status) ->
    gen_server:cast(?MODULE, {setStatus, VkontakteID, Status}).
    
getVkontakteInfo(VkontakteID) ->
    gen_server:cast(?MODULE, {getVkontakteInfo, VkontakteID, self()}).
    
exchangeVkontakteVotes(UserID, VkontakteID, Votes) ->
    gen_server:cast(?MODULE, {exchangeVkontakteVotes, UserID, VkontakteID, Votes, self()}).
    
startSendingMessage(VkontakteID, Message) ->
    gen_server:cast(?MODULE, {startSendingMessage, VkontakteID, Message}).
    
startSendingMessage(UserID1, UserID2, Message) ->
    gen_server:cast(?MODULE, {startSendingMessage, UserID1, UserID2, Message}).
    
addMessagesCount(Count) ->
    gen_server:cast(?MODULE, {addMessagesCount, Count}).
    
isSendingMessages() ->
    gen_server:call(?MODULE, isSendingMessages).
    
getLastResponse() ->
    gen_server:call(?MODULE, getLastResponse).
    
stopSendingMessage() ->
    gen_server:cast(?MODULE, stopSendingMessage).
    
buyVkontakteRating(FromVkontakteID, ToVkontakteID, Rating, Message) ->
    gen_server:cast(?MODULE, {buyVkontakteRating, FromVkontakteID, ToVkontakteID, Rating, Message, self()}).
    
sendVkontakteRating(ToVkontakteID, Rating, Message, VkRatingApp) ->
    gen_server:cast(?MODULE, {sendVkontakteRating, ToVkontakteID, Rating, Message, VkRatingApp, self()}).

setVkontakteLanguageValue(Key, Value) ->
    gen_server:call(?MODULE, {setVkontakteLanguageValue, Key, Value}, 60000).

updateWhiteList() ->
    gen_server:call(?MODULE, updateWhiteList).

getWhiteList() ->
    gen_server:call(?MODULE, getWhiteList).

updateBlackList() ->
    gen_server:call(?MODULE, updateBlackList).

getBlackList() ->
    gen_server:call(?MODULE, getBlackList).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([AppID, AppSecret, VkAppID, VkAppSecret, CheckAuth]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    process_flag(trap_exit, true),
    inets:start(),
    {ok, #state{appID=AppID, appSecret=AppSecret, vkAppID=VkAppID, vkAppSecret=VkAppSecret, checkAuth=CheckAuth, whiteList=takeWhiteList(), blackList=takeBlackList()}}.
    
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(getTransactionsHistory, _From, State) ->
    Params = getCommonParams(State#state.appID) ++ [{"method", "secure.getTransactionsHistory"}],
    Sig = utils:calculateSignature(Params, State#state.appSecret),
    URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(Params) ++ "&sig=" ++ Sig,
    Reply = case http:request(get, {URL, []}, [{relaxed, true}], []) of
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            if
                StatusCode =/= 200 ->
                    {error, {httpError, "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
                true ->
                    file:write_file("transactionsHistory.xml", Body),
					ok
            end;
        {error, _} ->
            {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
    end,
    
    {reply, Reply, State};
    
handle_call(isSendingMessages, _From, State) ->
    Reply = if State#state.messagingPID =/= undefined ->
        {true, State#state.messagesCount};
    true ->
        false
    end,
    
    {reply, Reply, State};
    
handle_call(getLastResponse, _From, State) ->
    {reply, State#state.lastResult, State};


handle_call(updateWhiteList, _From, State) ->
    WhiteList = takeWhiteList(),
    {reply, WhiteList, State#state{whiteList=WhiteList}};
    
handle_call(getWhiteList, _From, State) ->
    {reply, State#state.whiteList, State};

handle_call(updateBlackList, _From, State) ->
    BlackList = takeBlackList(),
    {reply, BlackList, State#state{blackList=BlackList}};
    
handle_call(getBlackList, _From, State) ->
    {reply, State#state.blackList, State};

%%for service proposes only
handle_call({setVkontakteLanguageValue, Key, Value}, _From, State) ->
    Reply = setVkontakteLanguageValueInternal(Key, Value, State),
    {reply, Reply, State};

handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast({authorize, VkontakteID, AuthKey, FirstLastName, VkontakteOwnerID, From}, State) ->
    spawn(fun() ->
        CalcAuthKey = utils:calculateAuthKey(VkontakteID, State#state.appID, State#state.appSecret),

        Response= if
            not State#state.checkAuth orelse CalcAuthKey =:= AuthKey ->
                case dbUser:getRecord(vkontakteID, VkontakteID) of
                    {error, {noVkontakteID, VkontakteID1}} ->
                       {ok,User} = registration:createUser(VkontakteID1, FirstLastName, VkontakteOwnerID, 0),
                       {authorizeVkontakte, ok, User};
                    {error, {Reason, Message}} ->
                        {authorizeVkontakte, error, Reason, Message, VkontakteID, AuthKey, FirstLastName};
                    User ->
                        {authorizeVkontakte, ok, User}
                end;
            true ->
                {authorizeVkontakte, error, authKeyMismatch, "[[authError]]", VkontakteID, AuthKey, FirstLastName}
        end,
    
        From ! Response
    end),
    {noreply, State};
    
handle_cast({setStatus, VkontakteID, Status}, State) ->
    spawn(fun() ->
        Rand = integer_to_list(random:uniform(1000000000)),
        TimeSt =  integer_to_list(utils:now()),
        Params = [{"api_id", State#state.appID}, {"method", "secure.saveAppStatus"}, {"timestamp", TimeSt }, {"random", Rand},{"status",Status}, {"uid", integer_to_list(VkontakteID)}, {"v", "2.0"}],
        Sig = utils:calculateSignature(Params, State#state.appSecret),
    
        ParamsEncoded = [{"api_id", State#state.appID}, {"method", "secure.saveAppStatus"}, {"timestamp", TimeSt}, {"random", Rand}, {"status",yaws_api:url_encode(Status)}, {"uid", integer_to_list(VkontakteID)}, {"v", "2.0"}],
        URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(ParamsEncoded) ++ "&sig=" ++ Sig,
        _Response = case http:request(get, {URL, []}, [{relaxed, true}], []) of
            {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
                if
                    StatusCode =/= 200 ->
                        {error, {httpError, StatusCode, ReasonPhrase,  "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
                    true ->
                        case utils:parseXml(Body) of
                            {response, _ResponseTags} ->
                                ok;
                            {error, ErrorTags} ->
                                {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                                {error, {apiError, ErrorMsg}}
                        end
                end;
            {error, _} ->
                {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
        end
    end),
        
    {noreply, State};
    
handle_cast({getVkontakteInfo, VkontakteID, From}, State) ->
    spawn(fun() ->
        log:write("getVkontakteInfo: ~p, invoked~n", [VkontakteID]),
        Params = getCommonParams(State#state.appID) ++ [{"method", "secure.getBalance"},  {"uid", integer_to_list(VkontakteID)}],
        Sig = utils:calculateSignature(Params, State#state.appSecret),
        URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(Params) ++ "&sig=" ++ Sig,
        log:write("getVkontakteInfo: ~p, sending request ~p~n", [VkontakteID, URL]),
        Response = case http:request(get, {URL, []}, [{relaxed, true}], []) of
            {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->            
                if 
                    StatusCode =/= 200 ->
                        log:write("getVkontakteInfo: ~p, error status code ~p~n", [VkontakteID, StatusCode]),
                        {get, vkontakteInfo, error, {httpError, "Ошибка HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
                    true ->
                        log:write("getVkontakteInfo: ~p, parsing xml ~p~n", [VkontakteID, Body]),
                        case utils:parseXml(Body) of
                            {response, ResponseTags} ->
                                {value, {_, Votes}} = lists:keysearch({response, balance}, 1, ResponseTags),
                                Votes2 = utils:trunc(list_to_integer(Votes)/100.0, 2),
                                log:write("getVkontakteInfo: ~p, got Votes2 ~p~n", [VkontakteID, Votes2]),
                                {get, vkontakteInfo, ok, #vkontakteInfo{userAppBalance=Votes2}};
                            {error, ErrorTags} ->
                                {value, {_, ErrorCode}} = lists:keysearch({error, error_code}, 1, ErrorTags),
                                {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                                log:write("getVkontakteInfo: ~p, error code = ~p, error msg = ~p~n", [VkontakteID, ErrorCode, ErrorMsg]),
                                {get, vkontakteInfo, error, {apiError, ErrorMsg}}
                        end
                end;
            {error, _} = Error ->
                log:write("getVkontakteInfo: got error response ~p, ~p~n", [VkontakteID, Error]),
                {get, vkontakteInfo, error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
        end,
        
        From ! Response
    end),
    
    {noreply, State};
    
handle_cast({buyVkontakteRating, FromVkontakteID, ToVkontakteID, Rating, Message, From}, State) ->
    spawn(fun() ->
        log:write("buyVkontakteRating: From = ~p, To = ~p, rating = ~p, Message=~pinvoked~n", [FromVkontakteID, ToVkontakteID, Rating, Message]),
        Votes = round(Rating*ratingRate(Rating)*100)/100,
        log:write("withdrawing votes: ~p ~p~n", [FromVkontakteID, Votes]),
        Response = case withdrawVkontakteVotes(FromVkontakteID, Votes, State#state.appID, State#state.appSecret) of
            ok ->
                Result = case catch addVkontakteRating(ToVkontakteID, Rating, Message, State#state.appID, State#state.appSecret) of
                    ok ->
                        {atomic, ok};
                    Error ->
                        log:write("buyVkontakteRating: From = ~p, To = ~p, error = ~1000p~n", [FromVkontakteID, ToVkontakteID, Error]),
                        log:write("returning votes: ~p ~p~n", [FromVkontakteID, Votes]),
                        addVkontakteVotes(FromVkontakteID, Votes, State#state.appID, State#state.appSecret),
                        {error, {error, "[[serverErrorNoVotesWithdrawn]]"}}
                end,
                %{buyVkontakteRating, Result};
                {buyVkontakteRating, mneser:transactionResult(Result, FromVkontakteID, {buyVkontakteRating, ToVkontakteID, Rating})};
            Error ->                
                {buyVkontakteRating, Error}
        end,

        From ! Response
    end),

    {noreply, State};
    
handle_cast({sendVkontakteRating, ToVkontakteID, Rating, Message, VkRatingApp, From}, State) ->
    spawn(fun() ->
        log:write("sendVkontakteRating: To = ~p, rating = ~p, Message=~pinvoked~n", [ToVkontakteID, Rating, Message]),

        {AppID, AppSecret} = case VkRatingApp of
            false ->
                {State#state.appID, State#state.appSecret};
            true ->
                {State#state.vkAppID, State#state.vkAppSecret}
        end,

        Result = case catch addVkontakteRating(ToVkontakteID, Rating, Message, AppID, AppSecret) of
            ok ->
                {atomic, ok};
            Error ->
                log:write("sendVkontakteRating: To = ~p, error = ~1000p~n", [ToVkontakteID, Error]),
                {error, {error, "[[serverError]]"}}
        end,

        From ! {sendVkontakteRating, mneser:transactionResult(Result, ToVkontakteID, {sendVkontakteRating, Rating})}
    end),

    {noreply, State};

handle_cast({exchangeVkontakteVotes, UserID, VkontakteID, Votes, From}, State) ->
    spawn(fun() ->
        log:write("exchangeVkontakteVotes: ~p, ~p, votes = ~p, invoked~n", [UserID, VkontakteID, Votes]),
        Response = case withdrawVkontakteVotes(VkontakteID, Votes, State#state.appID, State#state.appSecret) of
            ok ->          
                Result = try
                    mnesia:transaction(fun() ->
                        User = dbUser:getRecord_nt(id, UserID),
                        OldRealMoney = User#user.realMoney,
                        Rate = dbGlobal:get_nt(vkontakteExchangeRate),
                        NewRealMoney = OldRealMoney + Votes*Rate,
                        mnesia:write(User#user{realMoney=NewRealMoney}),
                        dbActivity:register_nt(User#user.id, {exchangeVkontakteVotes, Votes, Rate, OldRealMoney, NewRealMoney}, ok),
						Votes
                    end)
                catch throw: Throw ->
                    Throw
                end,
                case Result of
                    {atomic, _R} ->
                        ok;
                    Error ->
                        log:write("exchangeVkontakteVotes: ~p, ~p, error = ~1000p~n", [UserID, VkontakteID, Error]),
                        addVkontakteVotes(VkontakteID, Votes, State#state.appID, State#state.appSecret)
                end,
                {exchangeVkontakteVotes, mneser:transactionResult(Result, UserID, exchangeVkontakteVotes)};
            Error ->
                {exchangeVkontakteVotes, Error}
        end,

        From ! Response
    end),
        
    {noreply, State};
    
handle_cast({startSendingMessage, VkontakteID, Message}, State) ->
    NewState = if State#state.messagingPID =:= undefined ->
        PID = spawn_link(fun() -> 
            sendVkontakteMessage(integer_to_list(VkontakteID), Message, State)
        end),
        State#state{messagingPID=PID, messagesCount=0, lastResult=""};
    true ->
        State
    end,
    
    {noreply, NewState};
    
handle_cast({startSendingMessage, UserID1, UserID2, Message}, State) ->
    NewState = if State#state.messagingPID =:= undefined ->
        PID = spawn_link(fun() -> 
            sendVkontakteMessageLoop(UserID1, UserID2, Message, [], State)
        end),
        State#state{messagingPID=PID, messagesCount=0, lastResult=""};
    true ->
        State
    end,
    
    {noreply, NewState};
    
handle_cast({addMessagesCount, Count}, State) ->
    OldCount = State#state.messagesCount,
    {noreply, State#state{messagesCount=OldCount + Count}};
    
handle_cast(stopSendingMessage, State) ->
    if State#state.messagingPID =/= undefined ->
        exit(State#state.messagingPID, stopped);
    true ->
        ok
    end,
    
    {noreply, State};
    
handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info({'EXIT', From, Reason}, State) ->
    NewState = if From =:= State#state.messagingPID ->
        State#state{messagingPID=undefined, lastResult=Reason};
    true ->
        State
    end,
    
    {noreply, NewState};

handle_info(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Code change
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, erlang:append_element(State, takeBlackList())}.

%%-----------------------------------------------------------------------------
%% Terminate
%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

withdrawVkontakteVotes(VkontakteUserID, Votes, AppID, AppSecret) ->
    log:write("withdrawVkontakteVotes: ~p, votes = ~p, invoked~n", [VkontakteUserID, Votes]),
    Params = getCommonParams(AppID) ++ [{"method", "secure.withdrawVotes"}, {"uid", integer_to_list(VkontakteUserID)}, {"votes", integer_to_list(Votes*100)}],
    Sig = utils:calculateSignature(Params, AppSecret),
    URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(Params) ++ "&sig=" ++ Sig,
    log:write("withdrawVkontakteVotes: ~p, sending request ~p~n", [VkontakteUserID, URL]),
    case http:request(get, {URL, []}, [{relaxed, true}], []) of 
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            if StatusCode =/= 200 ->
                log:write("withdrawVkontakteVotes: ~p, error status code ~p~n", [VkontakteUserID, StatusCode]),
                {error, {httpError, "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
            true ->
                log:write("withdrawVkontakteVotes: ~p, parsing xml ~p~n", [VkontakteUserID, Body]),
                case utils:parseXml(Body) of
                    {response, ResponseTags} ->
                        log:write("withdrawVkontakteVotes: ~p, got ok~n", [VkontakteUserID]),
                        {value, {_, _}} = lists:keysearch({response, transferred}, 1, ResponseTags),
                        ok;
                    {error, ErrorTags} ->
                        {value, {_, ErrorCode}} = lists:keysearch({error, error_code}, 1, ErrorTags),
                        {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                        log:write("withdrawVkontakteVotes: ~p, error code = ~p, error msg = ~p~n", [VkontakteUserID, ErrorCode, ErrorMsg]),
                        {error, {apiError, ErrorMsg}}
                end
            end;
        {error, _Error} = Error ->
            log:write("withdrawVkontakteVotes: got error response ~p, ~p~n", [VkontakteUserID, Error]),
            {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
    end.
    
addVkontakteRating(VkontakteID, Rating, Message, AppID, AppSecret) ->
    Rand = integer_to_list(random:uniform(1000000000)),
    TimeSt =  integer_to_list(utils:now()),
    Params = [{"api_id", AppID}, {"message", Message}, {"method", "secure.addRating"}, {"timestamp", TimeSt }, {"random", Rand}, {"rate", integer_to_list(Rating)}, {"uid", integer_to_list(VkontakteID)}, {"v", "2.0"}],
    Sig = utils:calculateSignature(Params, AppSecret),

    ParamsEncoded = [{"api_id", AppID}, {"message",yaws_api:url_encode(Message)}, {"method", "secure.addRating"}, {"timestamp", TimeSt}, {"random", Rand}, {"rate", integer_to_list(Rating)}, {"uid", integer_to_list(VkontakteID)}, {"v", "2.0"}],
    URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(ParamsEncoded) ++ "&sig=" ++ Sig,
    Response = case http:request(get, {URL, []}, [{relaxed, true}], []) of
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            if
                StatusCode =/= 200 ->
                    {error, {httpError, StatusCode, ReasonPhrase,  "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
                true ->
                    case utils:parseXml(Body) of
                        {response, _ResponseTags} ->
                            ok;
                        {error, ErrorTags} ->
                            {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                            {error, {apiError, ErrorMsg}}
                    end
            end;
        {error, _} ->
            {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
    end, 
    Response.

addVkontakteVotes(VkontakteUserID, Votes, AppID, AppSecret) ->
    log:write("addVkontakteVotes: ~p, votes = ~p, invoked~n", [VkontakteUserID, Votes]),
    Params = getCommonParams(AppID) ++ [{"method", "secure.addVotes"}, {"uid", integer_to_list(VkontakteUserID)}, {"votes", integer_to_list(Votes*100)}],
    Sig = utils:calculateSignature(Params, AppSecret),
    URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(Params) ++ "&sig=" ++ Sig,    
    log:write("addVkontakteVotes: ~p, sending request ~p~n", [VkontakteUserID, URL]),
    case http:request(get, {URL, []}, [{relaxed, true}], []) of 
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            if StatusCode =/= 200 ->
                log:write("addVkontakteVotes: ~p, error status code ~p~n", [VkontakteUserID, StatusCode]),
                {error, {httpError, "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
            true ->
                log:write("addVkontakteVotes: ~p, parsing xml ~p~n", [VkontakteUserID, Body]),
                case utils:parseXml(Body) of
                    {response, ResponseTags} ->
                        {value, {_, _}} = lists:keysearch({response, transferred}, 1, ResponseTags),
                        log:write("addVkontakteVotes: ~p, got ok~n", [VkontakteUserID]),
                        ok;
                    {error, ErrorTags} ->
                        {value, {_, ErrorCode}} = lists:keysearch({error, error_code}, 1, ErrorTags),
                        {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                        log:write("addVkontakteVotes: ~p, error code = ~p, error msg = ~p~n", [VkontakteUserID, ErrorCode, ErrorMsg]),
                        {error, {apiError, ErrorMsg}}
                end
            end;
        {error, _Error} = Error ->
            log:write("addVkontakteVotes: got error response ~p, ~p~n", [VkontakteUserID, Error]),
            {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
    end.

getCommonParams(AppID) ->
    [{"api_id", AppID}, {"timestamp", integer_to_list(utils:now())}, {"random", integer_to_list(random:uniform(1000000000))}, {"v", "2.0"}].

paramsToString(Params) ->
    lists:foldl(
        fun({Name, Value}, Result) ->
            if Result /= "" ->
                Result ++ "&";
            true ->
                Result
            end ++ Name ++ "=" ++ Value
        end, "", Params).
        
sendVkontakteMessage(VkontakteIDs, Message, State) ->
    VkontakteIDsStr = lists:foldl(fun(ID, Acc) ->
        if length(Acc) > 0 ->
            Acc ++ "," ++ integer_to_list(ID);
        true ->
            integer_to_list(ID)
        end
    end, "", VkontakteIDs),
    
    Rand = integer_to_list(random:uniform(1000000000)),
    TimeSt =  integer_to_list(utils:now()),
    Params = [{"api_id", State#state.appID}, {"method", "secure.sendNotification"}, {"timestamp", TimeSt}, {"random", Rand},{"message", Message}, {"uids", VkontakteIDsStr}, {"v", "2.0"}],
    Sig = utils:calculateSignature(Params, State#state.appSecret),

    ParamsEncoded = [{"api_id", State#state.appID}, {"method", "secure.sendNotification"}, {"timestamp", TimeSt}, {"random", Rand},{"message", yaws_api:url_encode(Message)}, {"uids", VkontakteIDsStr}, {"v", "2.0"}],
    URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(ParamsEncoded) ++ "&sig=" ++ Sig,
    _Response = case http:request(get, {URL, []}, [{relaxed, true}], []) of
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            if
                StatusCode =/= 200 ->
                    {error, {httpError, StatusCode, ReasonPhrase,  "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
                true ->
                    case utils:parseXml(Body) of
                        {response, _ResponseTags} ->
                            ok;
                        {error, ErrorTags} ->
                            {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                            {error, {apiError, ErrorMsg}}
                    end
            end;
        {error, _} ->
            {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
    end.

setVkontakteLanguageValueInternal(Key, Value, State) ->  
    Rand = integer_to_list(random:uniform(1000000000)),
    TimeSt =  integer_to_list(utils:now()),
    Params = [{"api_id", State#state.appID}, {"method", "secure.setLanguageValue"}, {"timestamp", TimeSt}, {"random", Rand},{"key", Key}, {"value", Value}, {"v", "2.0"}],
    Sig = utils:calculateSignature(Params, State#state.appSecret),

    ParamsEncoded = [{"api_id", State#state.appID}, {"method", "secure.setLanguageValue"}, {"timestamp", TimeSt}, {"random", Rand},{"key", Key}, {"value", yaws_api:url_encode(Value)}, {"v", "2.0"}],
    URL = "http://api.vkontakte.ru/api.php?" ++ paramsToString(ParamsEncoded) ++ "&sig=" ++ Sig,
    _Response = case http:request(get, {URL, []}, [{relaxed, true}], []) of
        {ok, {{_HttpVersion, StatusCode, ReasonPhrase}, _Headers, Body}} ->
            if
                StatusCode =/= 200 ->
                    {error, {httpError, StatusCode, ReasonPhrase,  "[[error]] HTTP: " ++ integer_to_list(StatusCode) ++ " " ++ ReasonPhrase}};
                true ->
                    case utils:parseXml(Body) of
                        {response, _ResponseTags} ->
                            ok;
                        {error, ErrorTags} ->
                            {value, {_, ErrorMsg}} = lists:keysearch({error, error_msg}, 1, ErrorTags),
                            {error, {apiError, ErrorMsg}}
                    end
            end;
        {error, _} ->
            {error, {httpError, "[[serviceIsTemporaryUnavaiable]]"}}
    end.

trySendVkontakteMessage(VkontakteIDList, Message, Counter, State) ->
    case sendVkontakteMessage(VkontakteIDList, Message, State) of
        {error, _} = Error ->
            if Counter > 0 ->
                timer:sleep(1000),
                trySendVkontakteMessage(VkontakteIDList, Message, Counter - 1, State);
            true ->
                Error
            end;
        _Other ->
            ok
    end.
    
sendVkontakteMessageLoop(UserID, LastUserID, Message, VkontakteIDList, State) ->
    NewVkontakteIDList = case mnesia:dirty_read({user, UserID}) of
        [] ->
            VkontakteIDList;
        [User] ->
            [User#user.vkontakteID | VkontakteIDList]
    end,
    
    NewVkontakteIDList2 = if length(NewVkontakteIDList) >= 100 ->
        case trySendVkontakteMessage(NewVkontakteIDList, Message, 3, State) of
            ok ->
                vkontakte:addMessagesCount(100);
            Error ->
                exit(Error)
        end,
        [];
    true ->
        NewVkontakteIDList
    end,    

    NextUserID = UserID + 1,

    if NextUserID > LastUserID ->
        if length(NewVkontakteIDList2) > 0 ->
            case trySendVkontakteMessage(NewVkontakteIDList2, Message, 3, State) of
                ok ->
                    vkontakte:addMessagesCount(100);
                Error1 ->
                    exit(Error1)
            end;
        true -> ok end;
    true ->
        sendVkontakteMessageLoop(NextUserID,LastUserID, Message, NewVkontakteIDList2, State)
    end.
    
ratingRate(Rating) ->
    if 
        Rating < 50 -> ?RATING_RATE1;
        Rating < 100 -> ?RATING_RATE50;
        true -> ?RATING_RATE100
    end.

uploadLanguageFile(Path) ->
    {ok, Bin} = file:read_file(Path),
    Text = binary_to_list(Bin),
    Lines = string:tokens(Text, [13, 10]),
    lists:foreach(fun(Line) ->
        case Line of
            [$# | _Tail] ->
                ok;
            _Other ->
                [Key, Value] = string:tokens(Line, [$=]),
                %%log:write("~p = ~p~n", [Key, Value])
                setVkontakteLanguageValue(Key, Value)
        end
    end, Lines).

takeWhiteList() ->
    {ok, {_, _, Res}} = httpc:request("http://62.109.8.9/whitelist.txt"),
    lists:map( 
        fun( S ) ->  
            {In, _} = string:to_integer( 
                re:replace( 
                    S, 
                    "\\D+", 
                    "", 
                    [global, {return, list}] 
                ) 
            ), 
            In 
        end, 
        string:tokens( Res, "\n" ) 
    ).

takeBlackList() ->
    {ok, {_, _, Res}} = httpc:request("http://62.109.8.9/blacklist.txt"),
    lists:map( 
        fun( S ) ->  
            {In, _} = string:to_integer( 
                re:replace( 
                    S, 
                    "\\D+", 
                    "", 
                    [global, {return, list}] 
                ) 
            ), 
            In 
        end, 
        string:tokens( Res, "\n" ) 
    ).

