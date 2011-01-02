-module(queryCache).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([invalidate/1,
        requestRatings/2,
        requestRoutes/1,
        requestCars/1,
        requestCities/1,
        requestTopUserDailyScores/1]).

-include("data.hrl").
-include("config.hrl").
        
-record(state, {results}).
        
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:cast(?MODULE, stop).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([]) ->
    log:write(debug, ?MODULE_STRING, "process started ~n", []),
    {ok, #state{results=dict:new()}}.

invalidate(Key) ->
    gen_server:cast(?MODULE, {invalidate, Key}).

requestRatings(CityID, From) ->
    gen_server:cast(?MODULE, {requestRatings, CityID, From}).

requestRoutes(From) ->
    gen_server:cast(?MODULE, {requestRoutes, From}).

requestCars(From) ->
    gen_server:cast(?MODULE, {requestCars, From}).

requestCities(From) ->
    gen_server:cast(?MODULE, {requestCities, From}).

requestTopUserDailyScores(From) ->
    gen_server:cast(?MODULE, {requestTopUserDailyScores, From}).    

%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
  
handle_call(_Message, _From, State) ->    
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------

handle_cast({invalidate, Key}, State) ->
    NewResults = dict:erase(Key, State#state.results),
    {noreply, State#state{results=NewResults}};

handle_cast({requestRatings, CityID, From}, State) ->
    {Ratings, NewResults} = getResult({ratings, CityID}, fun()->
        dbUser:getRatings(CityID)
    end, ?RATING_EXPIRES_AFTER, State),
    From ! {requestRatings, Ratings},
    {noreply, State#state{results=NewResults}};

handle_cast({requestRoutes, From}, State) ->
    {Routes, NewResults} = getResult(routes, fun() ->
        mneser:getAllRoutes()
    end, 0, State),
    From ! {requestRoutes, Routes},
    {noreply, State#state{results=NewResults}};

handle_cast({requestCars, From}, State) ->
    {Cars, NewResults} = getResult(cars, fun() ->
        dbCar:getCars(1)
    end, 0, State),
    From ! {requestCars, Cars},
    {noreply, State#state{results=NewResults}};

handle_cast({requestCities, From}, State) ->
    {Cities, NewResults} = getResult(cities, fun() ->
        dbCity:getCities()
    end, 0, State),
    From ! {requestCities, Cities},
    {noreply, State#state{results=NewResults}};

handle_cast({requestTopUserDailyScores, From}, State) ->    
    {Scores, NewResults} = getResult(userTopDailyScores, fun() ->
        dbUser:getTopUserDailyScores()
    end, 1, State),
    %?DAILY_SCORES_EXPIRES_AFTER
    From ! {requestTopUserDailyScores, Scores},
    {noreply, State#state{results=NewResults}};

handle_cast(stop, State) ->
    {stop, shutdown, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
    
handle_info(_Message, State) ->
    {noreply, State}.

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

%getResult(Key, QueryFun, State) ->
%    getResult(Key, QueryFun, ?QUERY_DEFAULT_EXPIRES_AFTER, State).

getResult(Key, QueryFun, ExpiresAfter, State) ->
    case dict:find(Key, State#state.results) of
        {ok, {ExpiresTimestamp, CachedResult}} ->
            Now = utils:now(),
            if ExpiresTimestamp =:= 0 ->
                {CachedResult, State#state.results};
            ExpiresTimestamp > Now ->
                {CachedResult, State#state.results};
            true ->
                NewExpiresTimestamp = if ExpiresAfter =:= 0 ->
                    0;
                true ->
                    utils:now()+ExpiresAfter
                end,
                NewResult = QueryFun(),
                {NewResult, dict:store(Key, {NewExpiresTimestamp, NewResult}, State#state.results)}
            end;
        error ->
            NewExpiresTimestamp = if ExpiresAfter =:= 0 ->
                0;
            true ->
                utils:now()+ExpiresAfter
            end,
            NewResult = QueryFun(),
            {NewResult, dict:store(Key, {NewExpiresTimestamp, NewResult}, State#state.results)}            
    end.
