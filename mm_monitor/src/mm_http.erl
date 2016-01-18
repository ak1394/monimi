-module(mm_http).

%%
%% Include files
%%

-define(CRLF, <<"\r\n">>).
-define(TIMEOUT, 30000).
-define(DEFAULT_MAX_REDIRECTS, 3).
-define(MAX_BODY_SIZE, 4096).

%%
%% Exported Functions
%%

-export([http_probe/2, https_probe/2]).

%%
%% API Functions
%%

http_probe(Url, OptionList) ->
    probe(http, Url, OptionList).

https_probe(Url, OptionList) ->
    probe(https, Url, OptionList).

probe(Transport, Url, OptionList) ->
    case http_uri:parse(Url) of
        %% url parsed ok
    	{_Scheme, _UserInfo, _Host, _Port, _Path, _Query}=ParsedUrl ->
            probe_parsed(Transport, ParsedUrl, OptionList);
        %% failed to parse url
		{error, Reason} ->
            {error, Reason}
    end.

probe_parsed(Transport, {_Scheme, _UserInfo, Host, _Port, _Path, _Query}=ParsedUrl, OptionList) ->
	case inet:getaddr(Host, inet) of
    	{ok, HostAddr} ->
        	connect(Transport, HostAddr, ParsedUrl, OptionList);
        {error, _Reason} ->
        	{failed, dns_lookup_failed}
    end.

probe_redirect(Transport, Location, OldParsedUrl,  OptionList) ->
    case http_uri:parse(Location) of
        %% url parsed ok
    	{_Scheme, _UserInfo, _Host, _Port, _Path, _Query}=ParsedUrl ->
            probe_parsed(Transport, ParsedUrl, OptionList);
        %% no scheme likely got relative url
        {error, no_scheme} ->
            probe_parsed(Transport, fix_relative_uri(OldParsedUrl, Location), OptionList);
        %% failed to parse url
		{error, Reason} ->
            {error, Reason}
    end.

connect(Transport, HostAddr, ParsedUrl, OptionList) ->
    {_Scheme, _UserInfo, _Host, Port, _Path, _Query} = ParsedUrl,
    case  transport_connect(Transport, HostAddr, Port, OptionList) of
        {ok, Socket} ->
            send_request(Transport, Socket, ParsedUrl, OptionList);
        {error, _Reason} ->
            {failed, connect_failed}
    end.

send_request(Transport, Socket, ParsedUrl, OptionList) ->
    HttpRequest = construct_request(ParsedUrl),
    KeywordCheck = proplists:get_value(keyword, OptionList, false),
    case transport_send(Transport, Socket, HttpRequest) of
        ok -> 
            case read_status(Transport, Socket) of
                ok when KeywordCheck == false ->
                    Result = {ok, erlang:now()},
                    transport_close(Transport, Socket),
                    Result;
                ok ->
                    Result = check_keyword(Transport, Socket, KeywordCheck),
                    transport_close(Transport, Socket),
                    Result;
                redirect -> 
                    follow_redirect(Transport, Socket, ParsedUrl, OptionList);
                {invalid, Code} ->
                    transport_close(Transport, Socket),
                    {failed, {invalid_http_code, Code}};
                {failed, _Reason}=Result ->
                    transport_close(Transport, Socket),
                    Result
            end;
        {error, _Reason} ->
            transport_close(Transport, Socket),
            {failed, send_request_failed}
    end.

read_status(Transport, Socket) ->
    case transport_recv(Transport, Socket) of
        {ok, {http_response, _Version, Code, _Status}} ->
            case Code of
                200 ->
                    ok;
                N when (N == 303) or (N == 302) or (N == 301) ->
                    redirect;
                _   ->
                    {invalid, Code}
            end;
        {error, _Reason} ->
            {failed, get_http_status_failed}
    end.

follow_redirect(Transport, Socket, ParsedUrl, OptionList) -> 
    case transport_recv(Transport, Socket) of
        {ok, {http_header, _, 'Location', _, Location}} ->
            transport_close(Transport, Socket),
            CurrentRedirects = proplists:get_value(allowed_redirects, OptionList, ?DEFAULT_MAX_REDIRECTS),
            case CurrentRedirects > 0 of
                true ->
			        %% TODO decide what sort of transport (http, https, both) is allowed
                    NewOptionList = [{allowed_redirects, CurrentRedirects - 1} | proplists:delete(allowed_redirects, OptionList)],
                    probe_redirect(Transport, Location, ParsedUrl, NewOptionList);
                false ->
                    {failed, too_many_redirects}
            end;
        {ok, {http_header, _, _, _, _}} ->
            follow_redirect(Transport, Socket, ParsedUrl, OptionList);
        {ok, http_eoh} ->
            {failed, no_location_in_redirect};
        {error, _Reason} ->
            {failed, follow_redirect_failed}
    end.

check_keyword(Transport, Socket, Keyword) ->
	case read_content_length(Transport, Socket) of 
		{ok, ContentLength} ->
			check_keyword(Transport, Socket, Keyword, ContentLength);
		{error, Reason} ->
			{failed, Reason}
	end.

check_keyword(Transport, Socket, Keyword, ContentLength) when ContentLength > ?MAX_BODY_SIZE ->
	check_keyword(Transport, Socket, Keyword, ?MAX_BODY_SIZE);

check_keyword(Transport, Socket, Keyword, ContentLength) ->
	ok = inet:setopts(Socket, [{packet, raw}]), 
    case transport_recv(Transport, Socket, ContentLength) of
		{ok, Data} ->
			case string:str(erlang:binary_to_list(Data), Keyword) of
				0 ->
					{failed, keyword_not_found};
				_ ->
					%% success, keyword found
					{ok, erlang:now()}
			end;
		{error, _Reason} ->
            {failed, failed_to_read_response}
	end.

read_content_length(Transport, Socket) ->
	case read_content_length(Transport, Socket, "0") of
		{ok, ContentLength} ->
			{ok, erlang:list_to_integer(ContentLength)};
		Else ->
            Else
        end.

read_content_length(Transport, Socket, ContentLength0) ->
    case transport_recv(Transport, Socket) of
        {ok, {http_header, _, 'Content-Length', _, ContentLength}} ->
            read_content_length(Transport, Socket, ContentLength);
        {ok, {http_header, _, _, _, _}} ->
            read_content_length(Transport, Socket, ContentLength0);
        {ok, http_eoh} ->
            {ok, ContentLength0};
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Internal functions
%%

% TODO? pass port in host header? 
construct_request({_Scheme, _UserInfo, Host, _Port, Path, Query}) ->
    [<<"GET ">>, Path ++ Query, <<" HTTP/1.1">>, ?CRLF,
     <<"Host: ">>, Host, ?CRLF,
     <<"User-Agent: Mozilla/5.0 (compatible; MoniBot/1.0; http://monimi.net/monibot)">>, ?CRLF,
     <<"Referer: http://monimi.net/monibot">>, ?CRLF,
     <<"Connection: close">>, ?CRLF,
     ?CRLF].

transport_connect(http, HostAddr, Port, OptionList) ->
    case proplists:is_defined(keyword, OptionList) of
        true ->
            RecBuf = ?MAX_BODY_SIZE;
        false ->
            RecBuf = 512
    end,
    gen_tcp:connect(HostAddr, Port, [binary,
                                    {packet, http},
                                    {recbuf, RecBuf},
                                    {active, false},
                                    {send_timeout, ?TIMEOUT}], ?TIMEOUT);

transport_connect(https, HostAddr, Port, _OptionList) ->
    ssl:connect(HostAddr, Port, [binary,
                                {packet, http},
                                {active, false}], ?TIMEOUT).

transport_send(http, Socket, HttpRequest) ->
   gen_tcp:send(Socket, HttpRequest);

transport_send(https, Socket, HttpRequest) ->
    ssl:send(Socket, HttpRequest).

transport_recv(http, Socket) ->
    gen_tcp:recv(Socket, 0, ?TIMEOUT);

transport_recv(https, Socket) ->
    ssl:recv(Socket, 0, ?TIMEOUT).

transport_recv(http, Socket, Length) ->
    gen_tcp:recv(Socket, Length, ?TIMEOUT);

transport_recv(https, Socket, Length) ->
    ssl:recv(Socket, Length, ?TIMEOUT).

transport_close(http, Socket) ->
    gen_tcp:close(Socket);

transport_close(https, Socket) ->
     ssl:close(Socket).

fix_relative_uri({Scheme, _UserInfo, Host, Port, Path, _Query}, Location) ->
    %% check if the location is absolute path
    case  string:chr(Location, $/) of
        1 ->
        	{Scheme, [], Host, Port, Location, []};
        _ ->
			{Scheme, [], Host, Port, Path ++ Location, []}            
        end.