%% Google Cloud Messaging for Ejabberd
%% Created: 02/08/2015 by mrDoctorWho
%% License: MIT/X11

-module(mod_gcm).
-author("mrDoctorWho").

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-behaviour(gen_mod).

-record(gcm_users, {user, gcm_key, last_seen}).


-define(NS_GCM, "https://android.googleapis.com/gcm"). %% I hope Google doesn't mind.
-define(GCM_URL, ?NS_GCM ++ "/send").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded;charset=UTF-8").


-export([start/2, stop/1, message/3, iq/3, mod_opt_type/1, depends/2]).

%% http://stackoverflow.com/questions/114196/url-encode-in-erlang
-spec(url_encode(string()) -> string()).

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].


url_encode(Data) ->
    url_encode(Data,"").

url_encode([],Acc) ->
    Acc;
url_encode([{Key,Value}|R],"") ->
    url_encode(R, escape_uri(Key) ++ "=" ++ escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ escape_uri(Key) ++ "=" ++ escape_uri(Value)).


%% Send an HTTP request to Google APIs and handle the response
send([{Key, Value}|R], API_KEY) ->
	Header = [{"Authorization", url_encode([{"key", API_KEY}])}],
	Body = url_encode([{Key, Value}|R]),
	{ok, RawResponse} = httpc:request(post, {?GCM_URL, Header, ?CONTENT_TYPE, Body}, [], []),
	%% {{"HTTP/1.1",200,"OK"} ..}
	{{_, SCode, Status}, ResponseBody} = {element(1, RawResponse), element(3, RawResponse)},
	%% TODO: Errors 5xx
	case SCode of
		200 -> ?DEBUG("mod_gcm: t(he message was sent", []);
		401 -> ?ERROR_MSG("mod_gcm: error! Code ~B (~s)", [SCode, Status]);
		_ -> ?ERROR_MSG("mod_gcm: error! Code ~B (~s), response: \"~s\"", [SCode, Status, ResponseBody])
	end.


%% TODO: Define some kind of a shaper to prevent floods and the GCM API to burn out :/
%% Or this could be the limits, like 10 messages/user, 10 messages/hour, etc
message(From, To, Packet) ->
	Type = fxml:get_tag_attr_s(<<"type">>, Packet),
	?DEBUG("mod_gcm: got offline message", []),
	case Type of 
		"normal" -> ok;
		_ ->
			%% Strings
			JFrom = jlib:jid_to_string(From#jid{user = From#jid.user, server = From#jid.server, resource = <<"">>}),
			JTo = jlib:jid_to_string(To#jid{user = To#jid.user, server = To#jid.server, resource = <<"">>}),
			ToUser = To#jid.user,
			ToServer = To#jid.server,
			ServerKey = gen_mod:get_module_opt(ToServer, ?MODULE, gcm_api_key, fun(V) -> V end, undefined),
			Body = fxml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),

			%% Checking subscription
			{Subscription, _Groups} = 
				ejabberd_hooks:run_fold(roster_get_jid_info, ToServer, {none, []}, [ToUser, ToServer, From]),
			case Subscription of
				both ->
					case Body of
						<<>> -> ok; %% There is no body
						_ ->
							Result = mnesia:dirty_read(gcm_users, {ToUser, ToServer}),
							case Result of 
								[] -> ?DEBUG("mod_gcm: no record found for ~s", [JTo]);
								[#gcm_users{gcm_key = API_KEY}] ->
									?DEBUG("mod_gcm: sending the message to GCM for user ~s", [JTo]),
									Args = [{"registration_id", API_KEY}, {"data.message", Body}, {"data.source", JFrom}, {"data.destination", JTo}],
									if ServerKey /=
										undefined -> send(Args, ServerKey);
										true ->
											?ERROR_MSG("mod_gcm: gcm_api_key is undefined!", []),
											ok
									end
							end
						end;
					_ -> ok
			end
	end.


iq(#jid{user = User, server = Server}, _To, #iq{sub_el = SubEl} = IQ) ->
	LUser = jlib:nodeprep(User),
	LServer = jlib:nameprep(Server),

	{MegaSecs, Secs, _MicroSecs} = now(),
	TimeStamp = MegaSecs * 1000000 + Secs,

	API_KEY = fxml:get_tag_cdata(fxml:get_subtag(SubEl, <<"key">>)),

	F = fun() -> mnesia:write(#gcm_users{user={LUser, LServer}, gcm_key=API_KEY, last_seen=TimeStamp}) end,

	case mnesia:dirty_read(gcm_users, {LUser, LServer}) of
		[] ->
			mnesia:transaction(F),
			?DEBUG("mod_gcm: new user registered ~s@~s", [LUser, LServer]);

		%% Record exists, the key is equal to the one we know
		[#gcm_users{user={LUser, LServer}, gcm_key=API_KEY}] ->
			mnesia:transaction(F),
			?DEBUG("mod_gcm: updating last_seen for user ~s@~s", [LUser, LServer]);

		%% Record for this key was found, but for another key
		[#gcm_users{user={LUser, LServer}, gcm_key=_KEY}] ->
			mnesia:transaction(F),
			?DEBUG("mod_gcm: updating gcm_key for user ~s@~s", [LUser, LServer])
		end,
	
	IQ#iq{type=result, sub_el=[]}. %% We don't need the result, but the handler have to send something.


start(Host, _Opts) -> 
	ssl:start(),
	application:start(inets),
	mnesia:create_table(gcm_users, [{disc_copies, [node()]}, {attributes, record_info(fields, gcm_users)}]),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, <<?NS_GCM>>, ?MODULE, iq, no_queue),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, message, 49),
	?INFO_MSG("mod_gcm has started successfully!", []),
	ok.


stop(_Host) -> ok.


depends(_Host, _Opts) ->
    [].


mod_opt_type(gcm_api_key) -> fun iolist_to_binary/1. %binary_to_list?

