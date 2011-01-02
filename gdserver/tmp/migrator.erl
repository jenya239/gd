-module(migrator).
-export([migrateToCurrentVersion/0]).
-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("config.hrl").

writeVersion(Number) ->
	io:format("Writing version... ~n", []),
	mnesia:clear_table(version),
	Result = mnesia:transaction(fun() -> mnesia:write(#version{number=Number, result=ok}) end),
	io:format(" ~p~n", [Result]).
    
migrate(VersionNumber)->
	Module = list_to_atom("migration_" ++ integer_to_list(VersionNumber)),
    io:format("Trying to apply migration ~b... ~n", [VersionNumber]),
	Res = try apply(Module, migrate, [])
	catch
		error: undef -> {migration, no_exists}
	end,
	case Res of
		{migrated, VersionNumber} -> 
			writeVersion(VersionNumber),
			io:format("Successfully migrated to database version ~b ~n", [VersionNumber]);
		{migration, no_exists} -> 
			io:format("Migration ~b doesn't exist. Skipped ~n", [VersionNumber])
	end.

migrateTo(CurrentVersion, TargetVersion) ->
	if
		CurrentVersion < TargetVersion ->
			migrate(CurrentVersion + 1),
			migrateTo(CurrentVersion + 1, TargetVersion);
		CurrentVersion > TargetVersion ->
			io:format("DB version in cfg (~p) is less than actualy version of DB (~p)~n", [TargetVersion, CurrentVersion]);
		true -> ok
	end.

migrateToCurrentVersion() ->
	mnesia:stop(),
	case mnesia:create_schema([node()]) of
		ok -> io:format("Schema doesn't exist. Successfully created new schema~n", []);
		{error, {Name, {already_exists, _}}} -> io:format("Schema ~s already exists~n", [Name])
	end,
	mnesia:start(),
	mneser:waitForTables(100000),
	CurrentVersion = utils:getValue(utils:readConfig(?CONFIG_PATH), dbVersion),
	DBVersion = mneser:dbVersion(),
	io:format("Check data versions: ~n data schema version in code = ~b (read from config) ~n database version = ~b~n",
		[CurrentVersion, DBVersion]),
	migrateTo(DBVersion, CurrentVersion).
    
