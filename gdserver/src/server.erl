-module(server).

-behaviour(supervisor).

-include_lib("lib/yaws/include/yaws.hrl").
-include("config.hrl").

-export([start_link/0, start_link/1]).

-export([init/1]).

-export([get/2, upgrade/1, start/0, stop/0, start_web_server/1]).

start() ->
    application:load(gdserver),
    application:start(gdserver).
    
stop() ->
    application:stop(gdserver).

start_link() ->
    ?MODULE:start_link("./cfg/").
    
start_link(ConfigPath) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConfigPath]).

%%-----------------------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------------------
init([ConfigPath | _Args]) ->
    Config = utils:readConfig(ConfigPath++"server.cfg"),
    SecureConfig = utils:readConfig(?SECURE_CFG_PATH),
    
    mneser:start(),
    ?MODULE:upgrade(Config),
    
    ?MODULE:start_web_server(Config),

    inets:start(),
    OSMonResult = case application:start(sasl) of
        {error, Error} ->
            error_logger:warning_msg("Couldn't start sasl and os_mon: ~p~n", [Error]),
            error;
        ok ->
            case application:start(os_mon) of
                {error, Error} ->
                    error_logger:warning_msg("Couldn't start os_mon: ~p~n", [Error]),
                    error;
                ok ->
                    ok
            end
    end,

    Log = {log, {log, start_link, [get(Config, logBasePath)]},
            permanent, 2000, worker, [log]},
                    
    QueryCache = {queryCache, {queryCache, start_link, []},
            permanent, 2000, worker, [queryCache]},
            
    SystemStats = {systemStats, {systemStats, start_link, []},
            permanent, 2000, worker, [systemStats]},
            
    Vkontakte = {vkontakte, {vkontakte, start_link, [get(SecureConfig, appID), get(SecureConfig, appSecret), get(SecureConfig, vkAppID), get(SecureConfig, vkAppSecret), get(Config, checkAuthKey)]}, 
            permanent, 2000, worker, [vkontakte]},
                    
    CityManager = {cityManager, {cityManager, start_link, []},
            permanent, 2000, worker, [cityManager]},    
    
    CitySupervisor1 = {citySupervisor1, {citySupervisor, start_link, [1]},
            permanent, 2000, supervisor, [citySupervisor]},
    
    CitySupervisor2 = {citySupervisor2, {citySupervisor, start_link, [2]},
            permanent, 2000, supervisor, [citySupervisor]},
            
    CitySupervisor3 = {citySupervisor3, {citySupervisor, start_link, [3]},
            permanent, 2000, supervisor, [citySupervisor]},
                                    
    Backuper = {backuper, {backuper, start_link, [get(Config, backupPeriod)]},
            permanent, 2000 , worker, [backuper]},
            
    ServerSocket = {serverSocket, {serverSocket, start_link, [get(Config, gamePort)]},
            permanent, 2000, worker, [serverSocket]},
            
    FlashSecurity1 = {flashSecurity1, {flashSecurity, start_link, [get(Config, flashSecurePort)]},
            permanent, 2000, worker, [flashSecurity]},
            
    FlashSecurity2 = {flashSecurity2, {flashSecurity, start_link, [get(Config, flashSecurePort2)]},
            permanent, 2000, worker, [flashSecurity]}, 
        
    Scheduler = {scheduler, {scheduler, start_link, []},
            permanent, 2000, worker, [scheduler]}, 

    Pre = [Log, QueryCache, Vkontakte, CityManager, CitySupervisor1, CitySupervisor2, CitySupervisor3, Backuper, ServerSocket, FlashSecurity1, FlashSecurity2, Scheduler],
    Children = if OSMonResult =:= ok ->
        Pre ++ [SystemStats];
    true ->
        Pre
    end,
        
    {ok, {{one_for_one, 3, 10}, Children}}.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
get(Config, What) ->
    utils:getValue(Config, What).
    
upgrade(_Config)->
    migrator:migrateToCurrentVersion().
    
start_web_server(Config)-> 
    Users = [{"admin", "mnogoeuro"}], 

    AuthDirs = [#auth{dir = ["/editor", "/stats", "/admin", "/showdb"],
                      realm = "The realm",
                      users = Users}], 
    yaws:start_embedded(get(Config, webRoot), 
                        [{servername, get(Config, webServerName)},
                         {port, get(Config, webPort)},
                         {listen, get(Config, webIP)}, {authdirs, AuthDirs},
                         {appmods, [{"showdb", ymnesia},{"editdb",tableEditorView}]},
                         {arg_rewrite_mod, helper},
                         {incude_dir, "src/forWeb"}],
                        [{auth_log, false},
                         {copy_errlog, false}]).
