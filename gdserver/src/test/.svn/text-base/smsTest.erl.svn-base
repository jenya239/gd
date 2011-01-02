-module(smsTest).
-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").

getCode_test ()->
	Code1 = sms:getCode("02"),
	Code2 = sms:getCode("02"),
	?assert(Code1 /= Code2),
	?assertMatch(ok, sms:checkCode(Code1,"Me")),
	?assertMatch(ok, sms:checkCode(Code2,"Me")),
	?assertMatch({error,alreadyUsed},sms:checkCode(Code1,"Me")),
	?assertMatch({error,noSuchCode},sms:checkCode(12345,"Me")).
	