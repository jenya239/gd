-define(receiveXmlWithTimeout(ExpectedHead, ClientID, State, Timeout),
    ((fun () ->
        receive
                {tcp, _, [0, 0, 0, 128] ++ ExpectedHead ++ Tail___} ->
                    log:write(debug, "<~3..0b> <~4.._s> <-- ~10000p~n", [ClientID, atom_to_list(State), lists:sublist(ExpectedHead ++ Tail___, 60)]),
                    {ok, ExpectedHead ++ Tail___};
                {tcp_error, _, Reason } ->
                    {error, Reason};
                {tcp_closed, _} ->
                    {error, closed}
                after Timeout ->
                    {error, timeout}
            end
    end)())).

-define(receiveXml(ExpectedHead, ClientID, State),
    (?receiveXmlWithTimeout(ExpectedHead, ClientID, State, 1000))).

-define(receiveXmlTail(ExpectedHead, ClientID, State),
	((fun () ->
	    receive
                {tcp, _, [0, 0, 0, 128] ++ ExpectedHead ++ Tail} -> 
                    Message = ExpectedHead ++ Tail,
                    log:write(debug, "<~3..0b> <~4.._s> <-- ~10000p~n", [ClientID, atom_to_list(State), lists:sublist(Message, 60)]),
                    {ok, Tail};
                {tcp_error, _, Reason } ->
                    {error, Reason};
                {tcp_closed, _} ->
                    {error, closed}
                after 1000 ->
                    {error, timeout}
            end
	end)())).

-define(receiveAnyXml(ClientID, State),
	((fun () ->
	    receive
                {tcp, _, [0, 0, 0, 128] ++ Message} ->                     
                    log:write(debug, "<~3..0b> <~4.._s> <-- ~10000p~n", [ClientID, atom_to_list(State), lists:sublist(Message, 60)]),
                    {ok, Message};
                {tcp_error, _, Reason } ->
                    {error, Reason};
                {tcp_closed, _} ->
                    {error, closed}            
                after 1000 ->
                    {error, timeout}
            end
	end)())).
        
-define(receiveMessage(ClientID, State),
	((fun () ->
	    receive
                {tcp, _, [0, 0, 0, 128] ++ Xml} ->                    
                    log:write(debug, "<~3..0b> <~4.._s> <-- ~10000p~n", [ClientID, atom_to_list(State), lists:sublist(Xml, 60)]),
                    {ok, {xml, Xml}};
                {tcp, _, [0, 0, 0, 64] ++ Bin} ->
                    log:write(debug, "<~3..0b> <~4.._s> <-- ~-60w~n", [ClientID, atom_to_list(State), Bin]),
                    {ok, {bin, Bin}};
                {tcp_error, _, Reason } ->
                    {error, Reason};
                {tcp_closed, _} ->
                    {error, closed}                
                after 1000 ->
                    {error, timeout}
            end
	end)())).        
        