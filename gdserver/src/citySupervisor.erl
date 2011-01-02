-module(citySupervisor).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(CityID) ->
    supervisor:start_link({local, list_to_atom(?MODULE_STRING ++ integer_to_list(CityID))}, ?MODULE, [CityID]).

%%-----------------------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------------------
init([CityID]) ->
    LobbyManagerName = list_to_atom("lobbyManager" ++ integer_to_list(CityID)),
    LobbyManager = {LobbyManagerName, {lobbyManager, start_link, [LobbyManagerName, CityID]},
                    permanent, 2000, worker, [lobbyManager]},
                    
    GlobalChatName = list_to_atom("globalChat" ++ integer_to_list(CityID)),
    GlobalChat = {GlobalChatName, {globalChat, start_link, [CityID]},
            permanent, 2000, worker, [globalChat]},

    {ok, {{one_for_one, 0, 1}, [LobbyManager, GlobalChat]}}.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
