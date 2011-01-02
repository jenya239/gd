-module(fileUpload).
-include_lib("lib/yaws/include/yaws_api.hrl").
-export([receive_file/1]).

%%% Michael Leonhard (http://tamale.net/)
%%% Based on upload.yaws from YAWS 1.57 by Claes Wikstrom

-record(upload,			% represents state for a partially processed upload
				{filename			% name of the file
						,last				% indicates that the last part is being processed
						,rlist				% reversed order list of binaries comprising file data
						,data				% binary of file data
				}).

receive_file(Arg) ->
	case handle_post(Arg) of
		{upload, Upload} -> {ok, Upload#upload.filename, Upload#upload.data};
		Other -> Other
	end.

%%% Process POST data from client, state=#upload
%%% returns Ehtml | {get_more, Continuation, NewState}
handle_post(A) when is_record(A#arg.state, upload) ->
	%io:fwrite("fileUpload:handle_post/2 State=~s~n", [upload_to_string(A#arg.state)]),
	multipart(A, A#arg.state);
handle_post(A) ->
	%io:fwrite("fileUpload:handle_post/2 creating state~n"),
	State = #upload{},
	multipart(A, State).

%%% Process part of a multi-part form post
%%% returns Ehtml | {get_more, Continuation, NewState}
multipart(A, State) when is_record(State, upload) ->
	%io:fwrite("fileUpload:multipart/3 State=~s~n", [upload_to_string(State)]),
	case yaws_api:parse_multipart_post(A) of
		{cont, Cont, Part} ->
			%io:fwrite("fileUpload:multipart/3 cont~n"),
			case process_part(Part, State) of
				{done, Result} ->
					%io:fwrite("fileUpload:multipart/3 done~n"),
					Result;
				{cont, NewState} ->
					%io:fwrite("fileUpload:multipart/3 get_more NewState=~s~n", [upload_to_string(NewState)]),
					{get_more, Cont, NewState}
			end;
		{result, Part} ->
			%io:fwrite("fileUpload:multipart/3 result~n"),
			case process_part(Part, State#upload{last=true}) of
				{done, Result} ->
					%io:fwrite("fileUpload:multipart/3 done~n"),
					%{upload, Upload} = Result,
					%file:write_file("images/"++Upload#upload.filename, Upload#upload.data),
					Result;
				{cont, _} ->
					%io:fwrite("fileUpload:multipart/3 error~n"),
					{error, "Error During Upload"}
			end;
		[] -> {error,"You must select a file to upload."}
	end.

%% Converts path into a safe htmlized filename, with directories removed
%% returns non-empty String
sanitize_filename(Input) ->
	FilePath =
			case Input of
				List when is_list(List) -> List;
				_ -> "unnamed"
			end,
	StripWindowsDirsFun = fun(L) -> lists:last(string:tokens(L,"\\")) end,
	StripUnixDirsFun = fun(L) -> lists:last(string:tokens(L,"/")) end,
	EmptyToUnnamedFun = fun(L) -> case L of [] -> "unnamed"; _-> L end end,
	yaws_api:htmlize(
			EmptyToUnnamedFun(
			StripUnixDirsFun(
			StripWindowsDirsFun(FilePath)))).

%%% Processes a part of the multipart post
%%% returns {done,{upload,#upload}} | {done,{error,Reason}} | {cont,#upload}
%%%
%%% Process data_part and data identically
process_part([{part_body, Data}|Tail], State) ->
	%io:fwrite("fileUpload:process_part/4 part_body~n"),
	process_part([{body, Data}|Tail], State);

%%% Final part list has been processed
process_part([], State) when State#upload.last==true,State#upload.filename /= undefined ->
	%io:fwrite("fileUpload:process_part/4a State=~s~n", [upload_to_string(State)]),
	Data = iolist_to_binary(lists:reverse(State#upload.rlist)),
	{done, {upload, State#upload{rlist=undefined,data=Data}}};

%%% Final part list has been processed but filename was not processed
process_part([], State) when State#upload.last==true ->
	%io:fwrite("fileUpload:process_part/4b State=~s~n", [upload_to_string(State)]),
	{done, {error, "Error: did not receive header with upload."}};

%%% Part list was processed
process_part([], State) ->
	%io:fwrite("fileUpload:process_part/4c State=~s~n", [upload_to_string(State)]),
	{cont, State};

%%% Process header
process_part([{head, {"file", Opts}}|Tail], State ) ->
	%io:fwrite("fileUpload:process_part/4d State=~s~n", [upload_to_string(State)]),
	case lists:keysearch(filename, 1, Opts) of
		{value, {_, UncheckedFileName}} ->
			%io:fwrite("fileUpload:process_part/4d UncheckedFileName=~s~n", [UncheckedFileName]),
			FileName = sanitize_filename(UncheckedFileName),
			%io:fwrite("fileUpload:process_part/4d FileName=~s~n", [FileName]),
			process_part(Tail, State#upload{filename=FileName,rlist=[]});
		false ->
			{done, {error, "Error: filename not found in header."}}
	end;

%%% Process data
process_part([{body, Data}|Tail], State) when State#upload.filename /= undefined ->
	%io:fwrite("fileUpload:process_part/4e State=~s~n", [upload_to_string(State)]),
	NewRList = [list_to_binary(Data) | State#upload.rlist],
	%io:fwrite("fileUpload:process_part/4e data part=<<~w bytes>>~n", [length(Data)]),
	process_part(Tail, State#upload{rlist=NewRList}).
