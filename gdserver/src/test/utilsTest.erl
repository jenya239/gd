-module(utilsTest).

-include("lib/eunit/include/eunit.hrl").

count_elem_test()->
    Lst = ["a","b","a","c","a","b"],
    Expected = [{"a",3},{"b",2},{"c",1}],
    ?assertMatch(Expected, utils:countElem(Lst)),
    ?assertMatch([], utils:countElem([])).

generateFriendlyPassword_test() ->
    Password62 = utils:generateFriendlyPassword(6, 2),
    ?assert(is_list(Password62)),
    ?assertMatch(6+2, length(Password62)),
    
    Password00 = utils:generateFriendlyPassword(0, 0),
    ?assertMatch(0, length(Password00)),
    
    Password60 = utils:generateFriendlyPassword(6, 0),
    ?assertMatch(6+0, length(Password60)),
    
    Password02 = utils:generateFriendlyPassword(0, 2),
    ?assertMatch(0+2, length(Password02)),
       
    Password61 = utils:generateFriendlyPassword(6, 1),
    ?assertMatch(6+1, length(Password61)),
    
    Password12 = utils:generateFriendlyPassword(1, 2),
    ?assertMatch(1+2, length(Password12)),
    
    Password2010 = utils:generateFriendlyPassword(20, 10),
    ?assertMatch(20+10, length(Password2010)).

cp1251_to_utf8_test() ->
    %% "привет hello in" win1251
    Cp1251 = [16#ef, 16#f0, 16#E8, 16#E2, 16#E5, 16#F2, 16#20, 16#68, 16#65, 16#6C, 16#6C, 16#6F],
    %% "привет hello in" unicode
    Utf8 = [16#D0, 16#BF, 16#D1, 16#80, 16#D0, 16#B8, 16#D0, 16#B2, 16#D0, 16#B5, 16#D1, 16#82, 16#20, 16#68, 16#65, 16#6C, 16#6C, 16#6F],
    New = utils:cp1251_to_utf8(Cp1251),
    ?assertMatch(Utf8, New).

replace_test() ->
    ?assertMatch("1a.", utils:replace("1a.", $b, $c)),
    ?assertMatch("11&lr;bb", utils:replace("11<bb", $<, "&lr;")),
    ?assertMatch("11&lr;&lr;b&lr;b", utils:replace("11<<b<b", $<, "&lr;")),
    ?assertMatch("11&quot;bb", utils:replace("11\"bb", $", "&quot;")).

unicode_to_utf8_escaped_test() ->
    ?assertMatch("11&quot;b&gt;b&amp;lt;", utils:unicode_to_utf8_escaped("11\"b>b<")).

utf8_to_unicode_test() ->
    ?assertMatch([1055,1088,1080,1074,1077,1090], utils:utf8_to_unicode("Привет")).

callSyncHelper() ->
    receive 
        {Pid, a} -> Pid ! {aResponse, p1};
        {Pid, b} -> Pid ! {bResponse, p1, p2};
        {Pid, c} -> Pid ! {cResponse}
    end,
    callSyncHelper().    
    
callSync_test() ->    
    Pid = spawn(fun() -> callSyncHelper() end),
    ?assertMatch(p1, utils:callSync(Pid, a, aResponse)),
    ?assertMatch({p1, p2}, utils:callSync(Pid, b, bResponse)),
    ?assertMatch(ok, utils:callSync(Pid, c, cResponse)),
    exit(Pid, kill).
    
inList_test() ->
    ?assertEqual(false, utils:inList(anything, [])),
    ?assertEqual(true, utils:inList(anything, [anything])),
    ?assertEqual(true, utils:inList(anything, [anything, something])), 
    ?assertEqual(false, utils:inList(other, [anything, something])).

containsPrefix_test() ->
    ?assertEqual(false, utils:containsPrefix("anything", [])),
    ?assertEqual(true, utils:containsPrefix("anything", ["anything"])),
    ?assertEqual(false, utils:containsPrefix("anything", ["anythingmore"])),
    ?assertEqual(true, utils:containsPrefix("anything", ["any", "some"])), 
    ?assertEqual(true, utils:containsPrefix("anything", ["some", "any"])), 
    ?assertEqual(false, utils:containsPrefix("other", ["any", "some"])).
    
listsDiffTail_test() ->
    ?assertMatch({[], []}, utils:listsDiffTail("", "")),
    ?assertMatch({[], []}, utils:listsDiffTail("anything", "anything")),
    ?assertMatch({"thing", "something"}, utils:listsDiffTail("anything", "anysomething")).
    
nextMonth_test() ->
    ?assertMatch({1999, 12}, utils:nextMonth({1999, 11})),
    ?assertMatch({2000, 1}, utils:nextMonth({1999, 12})).
    
prevMonth_test() ->
    ?assertMatch({1999, 11}, utils:prevMonth({1999, 12})),
    ?assertMatch({1999, 12}, utils:prevMonth({2000, 1})).
    
nextDay_test() ->
    ?assertMatch({1999, 12, 31}, utils:nextDay({1999, 12, 30})),
    ?assertMatch({2000, 1, 1}, utils:nextDay({1999, 12, 31})).

prevDay_test() ->
    ?assertMatch({1999, 11, 19}, utils:prevDay({1999, 11, 20})),
    ?assertMatch({1999, 12, 31}, utils:prevDay({2000, 1, 1})).
    
dateToErlangTime_test() ->
    ?assertMatch(0, utils:dateToErlangTime({1970, 1, 1})),
    ?assertMatch(365*24*3600*1000000, utils:dateToErlangTime({1971, 1, 1})).

isInt_test() ->
		?assertMatch(true, utils:isInt("3")),
		?assertMatch(true, utils:isInt("34")),
		?assertMatch(true, utils:isInt("-13")),
		?assertMatch(false, utils:isInt("2.3")),
		?assertMatch(false, utils:isInt("3,4")),
		?assertMatch(false, utils:isInt("fsadfasd")),
		?assertMatch(false, utils:isInt("sdfsd33")).

trunc_test() ->
    ?assertMatch(0.0, utils:trunc(0.33, 0)),
    ?assertMatch(2.0, utils:trunc(2.33, 0)),
    ?assertMatch(2.3, utils:trunc(2.33, 1)),
    ?assertMatch(2.33, utils:trunc(2.33, 2)),
    ?assertMatch(2.33, utils:trunc(2.33, 3)).

parseXmerl_test() ->
    ?assertMatch({a, [{{a}, ""}]}, utils:parseXml("<a></a>")),
    ?assertMatch({a, [{{a}, "test1"}]}, utils:parseXml("<a>test1</a>")),
    ?assertMatch({a, [{{a, b}, "test3"},{{a, b}, "test3_"}]}, utils:parseXml("<a><b>test3</b><b>test3_</b></a>")),
    ?assertMatch({a, [{{a, b, c}, "test4"}, {{a, b, c}, "test4_"}]}, utils:parseXml("<a><b><c>test4</c></b><b><c>test4_</c></b></a>")).
    
% isLoginAllowed_test() ->
%     ?assertMatch(true, utils:isLoginAllowed("LoginName123")),
%     ?assertMatch(false, utils:isLoginAllowed("Логин123")).

to_string_test() ->
    ?assertMatch("", utils:toString("")),
    ?assertMatch("123", utils:toString(123)),
    ?assertMatch("[1234,1044]", utils:toString([1234, 1044])),
    ?assertMatch("atom", utils:toString(atom)),
    ?assertMatch("'ATOM'", utils:toString('ATOM')),
    ?assertMatch("ПРИВЕТutf8", utils:toString("ПРИВЕТutf8")).

%toLower_test() ->
%    ?assertMatch("abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()=абвгдеёжзийклмнопрстуфхцчшщъыьэюя", utils:toLower("ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()=АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")),
%    ?assertMatch("яaъb-.юzъ", utils:toLower("ЯAЪB-.ЮZЪ")).

utf8length_test() ->
    ?assertMatch(0, utils:utf8length("")),
    ?assertMatch(5, utils:utf8length("Снаки")).
    
cutNickname_test() ->
    ?assertMatch("", utils:cutNickname("")),
    ?assertMatch("Снаки", utils:cutNickname("Снаки")),
    ?assertMatch("Снаки123456789012345678901234567890", utils:cutNickname("Снаки1234567890123456789012345678901234567890")).