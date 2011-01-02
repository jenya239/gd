-module(utils).

-compile(export_all).

-include("data.hrl").
-include("config.hrl").
-include("lib/eunit/include/eunit.hrl").

-define(SECONDS_FROM_YEAR0_TO_YEAR1970, 719528 * 86400).

max(A,B) ->
    case A < B of
        true ->
            B;
         _E -> A
    end.

min(A,B) ->
    case A < B of
        true ->
            A;
         _E -> B
    end.

sec_to_string(Time)->   
    Hour = Time div 3600,
    Min = (Time rem 3600) div 60,
    Sec = Time rem 60,
    lists:flatten(io_lib:format("~p:~p:~p", [Hour,Min,Sec])).

%% ["1","2","1","3"] -> [("1",2),("2",1),("3",1)]
countElem([]) -> [];
countElem(Lst)->
   SortedLst = lists:sort(Lst),
   Fun = fun(Elem,{LastElem,CurrCount,Res}) ->
            case LastElem of
                [] ->    {Elem,1,Res};
                Elem ->  {Elem,CurrCount+1,Res};
                _    ->  {Elem,1,[{LastElem,CurrCount}|Res]}
            end
         end,
   {Elem,Count,Res} = lists:foldr(Fun,{[],0,[]},SortedLst),
   [{Elem,Count}|Res].

%% microseconds
erlangTime(ErlangTime)->
    timer:now_diff(ErlangTime, {0, 0, 0}).

%% maicroseconds
erlangTime()->
    utils:erlangTime(erlang:now()).

%% seconds
unixSeconds()->
    utils:now() div 1000.

%% milliseconds
now() ->
    timer:now_diff(erlang:now(), {0, 0, 0}) div 1000.    
        
nowString() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(erlang:now()),
    lists:flatten(io_lib:fwrite("~2B/~2B/~4..0B ~2B:~2.10.0B:~2.10.0B",
              [Month, Day, Year, Hour, Min, Sec])).

nub(Lst) ->
   sets:to_list(lists:foldl( fun(Elem,Set) ->
                                sets:add_element(Elem, Set)
                             end,
                           sets:new(),
                           Lst)).
                       
changeRecordName(Record, NewRecordName) ->
    [_RecordName | Fields] = tuple_to_list(Record),
    list_to_tuple([NewRecordName | Fields]).

unicode_to_utf8([undefined]) -> 
    [];

unicode_to_utf8(undefined) -> 
    [];

unicode_to_utf8(Atom) when is_atom(Atom) -> 
    unicode_to_utf8(atom_to_list(Atom));

unicode_to_utf8(Str) ->
    binary_to_list(unicode_to_binary(Str)).

%%
%% utf8_to_binary and encode_utf32 got from http://langweenie.blogspot.com/2007/06/utf8-for-erlang.html
%%
%% Given a list of unicode code points, return a binary of UTF-8
%% encoded text.

unicode_to_binary(Str) ->
    encode_utf32(Str, []).

encode_utf32([], Utf8) ->
    list_to_binary(lists:reverse(Utf8));

encode_utf32([U32| Str], Utf8) ->
    if
        %% 0-7F  0zzzzzzz
        U32 < 16#80 ->
            encode_utf32(Str, [U32|Utf8]);

        %% 110yyyyy 10zzzzzz
        U32 < 16#800 ->
            Y = 2#11000000 bor ((U32 band 16#7c0) bsr 6),
            Z = 2#10000000 bor  (U32 band 16#3f),
            encode_utf32(Str, [Z|[Y|Utf8]]);

        %% 1110xxxx 10yyyyyy 10zzzzzz
        U32 < 16#10000  ->
            X = 2#11100000 bor ((U32 band 16#f000) bsr 12),
            Y = 2#10000000 bor ((U32 band 16#fc0) bsr 6),
            Z = 2#10000000 bor  (U32 band 16#3f),
            encode_utf32(Str, [Z|[Y|[X|Utf8]]]);

        %% 11110www 10xxxxxx 10yyyyyy 10zzzzzz
        U32 < 16#110000 ->
            W = 2#11110000 bor ((U32 band 16#1c0000) bsr 18),
            X = 2#10000000 bor ((U32 band 16#3f000) bsr 12),
            Y = 2#10000000 bor ((U32 band 16#fc0) bsr 6),
            Z = 2#10000000 bor  (U32 band 16#3f),
            encode_utf32(Str, [Z|[Y|[X|[W|Utf8]]]]);

        %% past allocated code points
        %% bad code point
        true ->
            {error, U32}
    end.
    
utf8_to_unicode(UTF8) ->
    Xml = "<a atr='" ++ UTF8 ++ "'/>",
    {{xmlElement,a,a,[],
             {xmlNamespace,[],[]},
             [],1,
             [{xmlAttribute,atr,[],[],[],[],1,[],
                Unicode,
                false}],
             [],[],[],undeclared}, []} = xmerl_scan:string(Xml, [{xmlbase, ""}]),
         Unicode.    

loop(Fun, Count, Acc) ->
    loop(Fun, Count, Acc, 0).

loop(_Fun, 0, Acc, _Index) ->
    Acc;

loop(Fun, 1, Acc, Index) ->
    Acc2 = Fun(Index, Acc),
    Acc2;

loop(Fun, Count, Acc, Index) ->
    Acc2 = Fun(Index, Acc),
    loop(Fun, Count-1, Acc2, Index+1).

generateFriendlyPassword() ->
    generateFriendlyPassword(6, 2).

generateFriendlyPassword(LetterCount, DigitCount) ->
    Letters = generateLetters(LetterCount),
    addDigits(Letters, DigitCount).

addDigits(Letters, DigitCount) ->
    loop
        (
        fun(_Index, Acc) ->
                Acc ++ integer_to_list(random:uniform(9))
        end,
            DigitCount,
            Letters
        ).

generateLetters(LetterCount) ->
    
    {A1, A2, A3} = erlang:now(),
    random:seed(A1, A2, A3),    

    Charset1 = ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"],
    Charset2 = ["a", "e", "i", "o", "u"],
    Charsets = [Charset1, Charset2],

    StartCharsetIndex = random:uniform(2),
    
    Letters = loop
        (
        fun(Index, Acc) ->
                Charset = lists:nth(1 + (StartCharsetIndex + Index) rem 2, Charsets),
                Char = lists:nth(random:uniform(length(Charset)), Charset),
                Acc ++ Char
        end,
                    LetterCount,
                    ""
        ),
    
    DirtyWords = [ "huj", "hui", "huy", "hyi", "hyj", "xui", "xuj", "xuy", "xyi", "xyj", "bix", "bob", "con", "cum", "fod", "fuc", "fud", "fuk", "gal", "gat", "gay", "mal", "mam", "mar", "mec", "pat", "peg", "per", "pic", "pil", "pit", "put", "rab", "sex", "tar", "tes", "tet", "tol", "vac", "xup"],

    ContainDirtyWord = lists:any
        (
        fun(DirtyWord) ->
                string:str(Letters, DirtyWord) > 0
        end,
                    DirtyWords
        ),

    if 
        ContainDirtyWord ->
            generateLetters(LetterCount);
        true ->
            Letters
    end.

%N = erlang:now(), M = utils:erlangTime(N), R = utils:microsecsToTimestamp(M), {N, M, R}.
%{Mega, Secs, Micro}
microsecsToTimestamp(M) ->

	Mic = M rem 1000000,
	Sec = (M rem 1000000000000) div 1000000,
	Meg = M div 1000000000000,
	{ Meg, Sec, Mic}.
%	Mega = Microsecs div 1000000000000,
%	OstMega = Microsecs rem 1000000000000,
%	Secs = OstMega div math:pow(10, 6),
%	OstSecs = OstMega rem math:pow(10, 6),
%	{ Mega, Secs, OstSecs }.

timestampToString(Timestamp) ->
    {{Year, Month, Day}, {_Hour, _Min, _Sec}} = calendar:now_to_local_time(Timestamp),
    lists:flatten(io_lib:fwrite("~2B/~2B/~4..0B", [Day, Month, Year])).


timestampToDateTimeString(Timestamp) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(Timestamp),
  lists:flatten(io_lib:fwrite("~4..0B_~2..0B_~2..0B_~2..0B_~2.10.0B_~2.10.0B",
            [Year, Month, Day, Hour, Min, Sec])).

timestampToHumanString(Timestamp) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(Timestamp),
  lists:flatten(io_lib:fwrite("~4..0B.~2..0B.~2..0B ~2..0B:~2.10.0B:~2.10.0B",
            [Year, Month, Day, Hour, Min, Sec])).

timestampString() ->
  timestampToDateTimeString(erlang:now()).

cp1251_to_utf8(String) ->
    lists:foldl(
        fun(Ch, NewString) ->
            if 
                Ch < 16#80 -> NewString ++ [Ch];
                Ch >= 16#C0 ->
                    if 
                        Ch < 16#F0 ->
                            NewString ++ [16#D0, 16#90 + Ch - 16#C0]; %% А-Я, а-п (A-YA, a-p) 
                        true ->
                            NewString ++ [16#D1, 16#80 + Ch - 16#F0] %% р-я (r-ya)
                    end;
                true ->
                    case (Ch) of
                        16#A8 -> NewString ++ [16#D0, 16#81]; % YO
                        16#B8 -> NewString ++ [16#D1, 16#91]; % yo
                % ukrainian
                        16#A1 -> NewString ++ [16#D0, 16#8E]; % Ў (U)
                        16#A2 -> NewString ++ [16#D1, 16#9E]; % ў (u)
                        16#AA -> NewString ++ [16#D0, 16#84]; % Є (e)
                        16#AF -> NewString ++ [16#D0, 16#87]; % Ї (I..)
                        16#B2 -> NewString ++ [16#D0, 16#86]; % I (I)
                        16#B3 -> NewString ++ [16#D1, 16#96]; % i (i)
                        16#BA -> NewString ++ [16#D1, 16#94]; % є (e)
                        16#BF -> NewString ++ [16#D1, 16#97]; % ї (i..)
                % chuvashian
                        16#8C -> NewString ++ [16#D3, 16#90]; % Ӑ (A)
                        16#8D -> NewString ++ [16#D3, 16#96]; % Ӗ (E)
                        16#8E -> NewString ++ [16#D2, 16#AA]; % Ҫ (SCH)
                        16#8F -> NewString ++ [16#D3, 16#B2]; % Ӳ (U)
                        16#9C -> NewString ++ [16#D3, 16#91]; % ӑ (a)
                        16#9D -> NewString ++ [16#D3, 16#97]; % ӗ (e)
                        16#9E -> NewString ++ [16#D2, 16#AB]; % ҫ (sch)
                        16#9F -> NewString ++ [16#D3, 16#B3] % ӳ (u)
                    end
            end
        end,
        "",
        String).

replace(Str, Char, Seq) ->
    lists:foldr
        (
            fun(CurrChar,Acc) ->
                if CurrChar == Char ->
                    Seq ++ Acc;
                        true -> [CurrChar | Acc]
                end
            end,[], Str
        ).

escapeText(Text) ->
    utils:replace(
      utils:replace(
        utils:replace(
          utils:replace(
            utils:replace(
                Text,
                    $" ,"&quot;"), 
                $<, "&lt;"), 
            $>, "&gt;"),
        $', "#quot;"),
    $&, "#and;").

unicode_to_utf8_escaped(Text) ->
    UT8 = utils:unicode_to_utf8(lists:flatten(Text)),
    escapeText(UT8).

callSync(TargetPID, Message) ->
    callSync(TargetPID, Message, Message).

callSync(TargetPID, Message, FirstResponseElement) ->
    TargetPID ! {self(), Message},
    receive
        ResponseMessage when element(1, ResponseMessage) =:= FirstResponseElement ->
            [_ | ResponseTail] = tuple_to_list(ResponseMessage),
            case ResponseTail of
                [] -> ok;
                [SingleElement] -> SingleElement;
                MultipleElements -> list_to_tuple(MultipleElements)
            end            
        after ?SYNC_REQUEST_TIMEOUT ->
            {error, {timeout, "callSync timeout"}}
    end.

clearMessageQueue() ->
    receive
        _Anything ->
            clearMessageQueue()
        after 1000 ->
            ok
    end.

inList(X, List) ->
    lists:any(fun(Y) -> Y =:= X end, List).

containsPrefix(String, Prefixes) ->
    lists:any(
        fun(Elem) ->
            string:str(String, Elem) =:= 1                
        end, Prefixes).

listsDiffTail([], []) ->
    {[], []};

listsDiffTail(L1, []) ->
    {L1, []};

listsDiffTail([], L2) ->
    {[], L2};

listsDiffTail([H1 | T1] = L1, [H2 | T2] = L2) ->
    if 
        H1 =:= H2 ->
            listsDiffTail(T1, T2);
        true ->
            {L1, L2}
    end.
    
sendToAll(Message, Clients) ->
    sendToAllExceptOne(Message, Clients, -1).

version() ->
    dict_to_list_fixed.

sendToAllExceptOne(Message, Clients, ExceptClientID) ->
    Tuples = dict:to_list(Clients),
    lists:foreach(
        fun({ClientID, {ClientPID, _ClientInfo}}) ->
                if is_number(ClientID) andalso ClientID =/= ExceptClientID andalso is_pid(ClientPID) ->
                    ClientPID ! Message;
                true -> ok end
        end, Tuples).

%% Fun(Message,UserID) -> MessageRes
sendToAllDifferent(Message, Clients, Fun) ->
    lists:foreach(
        fun({_ClientID, {ClientPID, ClientInfo}}) ->
            if is_pid(ClientPID) ->
                ClientPID ! Fun(Message, ClientInfo#clientInfo.userID);
            true ->
                ok
            end
        end, dict:to_list( Clients) ).
        
sendMessageToClient(ClientID, Message, Clients) ->
    case dict:find(ClientID, Clients) of
        {ok, {undefined, _}} ->
            ok;
        {ok, {ClientPID, _}} ->
            ClientPID ! Message;
        error ->
            ok
    end.
    
sendMessages(Pid, Messages) ->
    lists:foreach( 
        fun(Message) -> 
            Pid ! Message
        end, Messages).    

nextMonth({Year, Month}) ->
    case Month + 1 > 12 of
        true ->
            {Year+1, 1};
        false ->
            {Year, Month+1}
    end.

prevMonth({Year, Month}) ->
    case Month - 1 =< 0 of
        true ->
            {Year-1, 12};
        false ->
            {Year, Month-1}
    end.
    
nextDay(Date) ->
    ErlangTime = dateToErlangTime(Date),
    NextErlangTime = ErlangTime + 24 * 60 * 60 * 1000000,
    Timestamp = microsecsToTimestamp(NextErlangTime),
    {NextDate, _} = calendar:now_to_universal_time(Timestamp),
    NextDate.
    
prevDay(Date) ->
    ErlangTime = dateToErlangTime(Date),
    PrevErlangTime = ErlangTime - 24 * 60 * 60 * 1000000,
    Timestamp = microsecsToTimestamp(PrevErlangTime),
    {PrevDate, _} = calendar:now_to_universal_time(Timestamp),
    PrevDate.

dateToErlangTime({_Year, _Month, _Day} = Date, Time) ->
    (calendar:datetime_to_gregorian_seconds({Date, Time}) - ?SECONDS_FROM_YEAR0_TO_YEAR1970) * 1000000.

dateToErlangTime({_Year, _Month, _Day} = Date) ->
    dateToErlangTime(Date, {0, 0, 0}).
    
dateToUniversalErlangTime({_Year, _Month, _Day} = Date, Time) ->
    {UDate, UTime} = case calendar:local_time_to_universal_time_dst({Date, Time}) of
        [] ->
            {Date, Time};
        [{UD, UT}, _] ->
            {UD, UT};
        [{UD, UT}] ->
            {UD, UT}
    end,
    (calendar:datetime_to_gregorian_seconds({UDate, UTime}) - ?SECONDS_FROM_YEAR0_TO_YEAR1970) * 1000000.

dateToUniversalErlangTime({_Year, _Month, _Day} = Date) ->
    dateToUniversalErlangTime(Date, {0, 0, 0}).

fmt(Text, Params) ->
    lists:flatten(io_lib:format(Text, Params)).

isInt(Str) ->
	case (catch list_to_integer(Str)) of
		{'EXIT', _} -> false;
		_Integer -> true
	end.

isFloat(Str) ->
	case (catch list_to_float(Str)) of
		{'EXIT', _} -> false;
		_Float -> true
	end.

readConfig(FilePath) ->
    {ok, List} = file:consult(FilePath),
    List.

getValue(Map, Key) ->
    Search = lists:keysearch(Key, 1, Map),
    {_Some, {Key, Value}} = Search,
    Value.
    
trunc(FloatValue, Digits) ->
    Power = math:pow(10, Digits), 
    erlang:trunc(FloatValue*Power)/Power.
    
floor(X) ->
    case trunc(X) of 
        Y when Y > X -> Y - 1;
        Z -> Z
    end.

toStringForScalars(Var) ->
    if
		is_atom(Var) -> atom_to_list(Var);
		is_integer(Var) -> integer_to_list(Var);
		is_float(Var) -> io_lib:format("~8.2f", [Var]);
		true -> Var
	end.

toString(Val) ->
    if is_list(Val) ->
        Val2 = lists:flatten(Val),
        IsAscii = lists:all(fun(Char) -> is_integer(Char) andalso Char >=0 andalso Char =< 255 end, Val2),
        if IsAscii ->
            Val2;
        true ->
            lists:flatten(io_lib:format("~p", [Val2]))
        end;
    true ->
        lists:flatten(io_lib:format("~p", [Val]))
    end.

convertParentsList(ParentsList) ->
    lists:reverse(lists:map(
        fun({ParentName, _}) ->
            ParentName
        end, ParentsList)).

parseXml(Xml) ->
    {Xmerl, []} = xmerl_scan:string(Xml, [{xmlbase, ""}, {space, normalize}]),
    {element(2, Xmerl), parseXmerl(Xmerl)}.

parseXmerl(Xmerl) when element(1, Xmerl) =:= xmlElement ->
    {xmlElement,TagName,TagName,[],{xmlNamespace,[],[]},ParentsList,_Index,_AttributeList,TagsList,[],_,undeclared} = Xmerl,
    case TagsList of 
        [] ->
            {xmlElement,TagName,TagName,[],{xmlNamespace,[],[]},ParentsList,_Index,_AttributeList,[],[],_,undeclared} = Xmerl,
            ParentsTuple = list_to_tuple(convertParentsList(ParentsList) ++ [TagName]),
            [{ParentsTuple, ""}];
        List ->
            lists:foldl(
                fun(InnerXmerl, ResultList) ->
                    ResultList ++ parseXmerl(InnerXmerl)
                end, [], List)
    end;

parseXmerl(Xmerl) when element(1, Xmerl) =:= xmlText ->
    {xmlText,ParentsList,_Index,_AttributeList,Value,text} = Xmerl,
    ParentsTuple = list_to_tuple(convertParentsList(ParentsList)),
    [{ParentsTuple, Value}].

calculateSignature(Params, AppSecret) ->
    Concat = lists:foldl(
        fun({Name, Value}, Result) ->
            Result ++ Name ++ "=" ++ Value
        end, "", lists:keysort(1, Params)) ++ AppSecret,
    MD5 = md5_hexdigest(Concat),
    MD5.

calculateAuthKey(VkontakteID, AppID, AppSecret) ->
    Concat = AppID ++ "_" ++ integer_to_list(VkontakteID) ++ "_" ++ AppSecret,
    md5_hexdigest(Concat).

md5_hexdigest(String) ->
   string:to_lower(
     lists:flatten(
       lists:map(
         fun(V) ->
                 case httpd_util:integer_to_hexlist(V) of
                     [A, B] -> [A, B];
                     [B] -> [$0, B]
                 end
         end,
         binary_to_list(erlang:md5(String))
        ))).

stringToTerm(String) ->
	element(2, erl_parse:parse_term(element(2, erl_scan:string( String )))).

toLower(String) ->
    string:to_lower(String).
    
utf8length(String) ->
    Unicode = utf8_to_unicode(String),
    length(Unicode).
    
cutNickname(Nickname) ->
    Unicode = utf8_to_unicode(Nickname),
    Unicode1 = lists:sublist(Unicode, ?MAX_NICKNAME_LENGTH),
    unicode_to_utf8(Unicode1).
    
formatTime(Time) ->
    Seconds = Time div 1000,
    Minutes = Seconds div 60,
    Hours = Minutes div 60,
        
    if Hours < 1 ->
        Ending = if Minutes >= 10 andalso Minutes =< 20 ->
            "минут";
        true ->
            R = Minutes rem 10,
            if R == 0 orelse R == 5 orelse R == 6 orelse R == 7 orelse R == 8 orelse R == 9 ->
                "минут";
            true ->
                if R == 1 ->
                    "минуту";
                true ->
                    "минуты"
                end
            end
        end,
        
        integer_to_list(Minutes) ++ " " ++ Ending;
    true ->
        Ending = if Hours >= 10 andalso Hours =< 20 ->
            "часов";
        true ->
            R = Hours rem 10,
            if R == 0 orelse R == 5 orelse R == 6 orelse R == 7 orelse R == 8 orelse R == 9 ->
                "часов";
            true ->
                if R == 1 ->
                    "час";
                true ->
                    "часа"
                end
            end
        end,
        integer_to_list(Hours) ++ " " ++ Ending
    end.
    
monthToString(Month) ->
    case Month of
        1 ->
            "Январь";
        2 ->
            "Февраль";
        3 ->
            "Март";
        4 ->
            "Апрель";
        5 ->
            "Май";
        6 ->
            "Июнь";
        7 ->
            "Июль";
        8 ->
            "Август";
        9 ->
            "Сентябрь";
        10 ->
            "Октябрь";
        11 ->
            "Ноябрь";
        12 ->
            "Декабрь"
    end.

extractNamedList(What, Terms) ->
	{value, Res0} = lists:keysearch( What, 1, Terms ),
  element( 2, Res0 ).

termToJson(X) when is_integer(X) -> integer_to_list(X);
termToJson(X) when is_float(X) -> io_lib:format("~.2f", [X]);
termToJson(X) when is_atom(X) -> "\"" ++ atom_to_list(X) ++ "\"";
termToJson(X) when is_tuple(X) -> termToJson(tuple_to_list(X));
termToJson(L) when is_list(L) ->
	A = lists:append(
		lists:map(
			fun(E) -> [termToJson(E), ", "] end,
			L
		)
	),
	lists:append(lists:append([
		["["],
		case length(A) == 0 of
			true -> [""];
			false -> lists:sublist( A, length(A) - 1 )
		end,
		["]\n"]
	])).

dateTimeToErlangMacroseconds(DateTime) ->
    (calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0,0,0}}))*1000000.

subtractDays(DateTime, Days) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime)- 24*60*60*Days).
