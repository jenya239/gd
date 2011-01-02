call ant
call erl -noshell -pa ebin -s log start_link "logs/fuel.txt" -s fuelTest start
call erl -noshell -pa ebin -s log start_link "logs/authorizationTest.txt" -s authorizationTest start
call erl -noshell -pa ebin -s log start_link "logs/notConnectedTest.txt" -s notConnectedTest start
call erl -noshell -pa ebin -s log start_link "logs/racingTest.txt" -s racingTest start
call erl -noshell -pa ebin -s log start_link "logs/lobbyTestF.txt" -s lobbyTestF start


@rem call erl -noshell -pa ebin -s stressTest start
@rem call erl -pa ebin -s authorizationTest start -s notConnectedTest start -s racingTest start -s stressTest start

