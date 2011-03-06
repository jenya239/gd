-define(CARSTATE_DROP_TIMEOUT, 1000).
-define(MILLISECONDS_IN_HOUR, (60*60*1000)).
-define(RATING_EXPIRES_AFTER, (15*60*1000)).
-define(DAILY_SCORES_EXPIRES_AFTER, (1*60*1000)).
-define(QUERY_DEFAULT_EXPIRES_AFTER, (60*60*1000)).
-define(IDLE_TIMEOUT, 120000).
-define(SYNC_REQUEST_TIMEOUT, 1000).
-define(MAX_ON_RACE, 100).
-define(CLIENT_SOCKET_TIMEOUT, (1*60*1000)).
-define(SOCKET_SEND_TIMEOUT, (10*1000)).
%-define(RACING_IDLE_TIMEOUT, 5000).
-define(MESSAGE_FILTER, [carState, otherCarState, loadingProgress, {get, lobbies}, {get, cars}, {get, routes}, {get, gameTypes}, {get, ratings}, {ping}]).
%-define(MESSAGE_FILTER, []).
%-define(XML_FILTER, ["<event name=\"get\" property=\"lobbies\"", "<event name=\"loadingProgress\"", "<event name=\"ping\""]).
-define(XML_FILTER, []).
-define(MAX_LOG_SIZE, (1024*1024*1024)).
-define(HTTP_VERBOSE, false).
-define(MAX_NICKNAME_LENGTH, 35).
-define(APP_ID, "468346").
-define(CONFIG_PATH, "cfg/server.cfg").
-define(SECURE_CFG_PATH, "cfg/secure.cfg").
-define(RATING_RATE1, 0.7).
-define(RATING_RATE50, 0.65).
-define(RATING_RATE100, 0.55).
-define(LOG_WRITE(_Fun), ok).
%-define(LOG_WRITE(Fun), Fun).

%user to import:
%-include("config.hrl").

% critical - что-то очень важное, что всегда надо писать (server start/shutdown? racemanager error?)
% error - что-то упало
% warning - что-то не так, но не упало
% info - все ок
% debug - все ок - в деталях
% trace - для быстрой отладки - регулрно надо искать log:write(trace и удалять из кода

-define(LOG_LEVEL, trace).
