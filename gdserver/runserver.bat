call ant
erl +A 128 -sname dev@cs2536 -pa ebin -mnesia dir '"db/dev-remote"' -s mneser start -s server start
@rem erl -sname dev -pa ebin -mnesia dir '"db/test_new"' -s mneser start -s server start
