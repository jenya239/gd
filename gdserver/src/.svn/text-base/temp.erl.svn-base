-module(temp).

-export([test/0]).

-include("data.hrl").

test() ->
    io:format("~w~n", [utf8_to_unicode("Привет")]).

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
    
