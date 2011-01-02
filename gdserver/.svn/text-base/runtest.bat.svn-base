call ant
del /Q db\test
erl -noshell -pa ebin -pa lib/eunit/ebin -mnesia dir '"db/test"' -s mneser deleteDatabase -s migrator migrateToCurrentVersion -s allTest main -s init stop
del /Q db\test
