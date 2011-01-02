del /Q db\dev\*.*
copy db_3\dev\*.* db\dev
erl -pa ebin -mnesia dir '"db/dev"' -s server upgrade -s init stop
