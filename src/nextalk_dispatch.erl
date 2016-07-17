%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc HTTP request dispatch manager.
%% @author zhangsong
%% @since 0.1, 2016-7-8;
%% @version 0.1

-module(nextalk_dispatch).

-include("nextalk.hrl").

-define(H_AUTH, {"WWW-Authenticate", "Basic Realm=\"NexTalk\""}).
-define(H_PLAIN, {"Content-Type", "text/plain"}).
-define(H_JSON, {"Content-Type", "application/json"}).
-define(H_JSCRIPT, {"Content-Type", "application/x-javascript"}).
-define(API, "/api").
-define(API_WEBSOCKET, "/api/websocket").
-define(API_POLLING, "/api/polling").

-record(state, {docroot, dispatch}).

-export([http_handler/0, handle_request/2]).
-export([strftime/1]).

%%--------------------------------------------------------------------
%% HTTP Handler and Dispatcher
%%--------------------------------------------------------------------

http_handler() ->
    {ok, Modules} = application:get_key(?APP, modules),
    APIs = lists:append(lists:map(fun http_api/1, Modules)),
    State = #state{docroot = docroot(), dispatch = dispatcher(APIs)},
    {?MODULE, handle_request, [State]}.

http_api(Mod) ->
    Attrs = Mod:module_info(attributes),
    [{Name, {Mod, Fun, Args}} || {http_api, [{Name, Fun, Args}]} <- Attrs].

docroot() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv", "www"]).

dispatcher(APIs) ->
    fun(Req, Name, Params) ->
        case proplists:get_value(Name, APIs) of
            {Mod, Fun, ArgDefs} ->
                F = fun(Def) -> parse_arg(Def, Params) end,
                Args = lists:map(F, ArgDefs),
                case catch apply(Mod, Fun, Args) of
                    {ok, Data} ->
                        Format = proplists:get_value("format", Params),
                        CB = proplists:get_value("callback", Params),
                        respond(Req, Data, Format, CB);
                    {'EXIT', Reason} ->
                        lager:error("Badrequest API '~s' Error: ~p",
                                    [Name, Reason]),
                        Req:respond({404, [?H_PLAIN], Reason})
                end;
            undefined ->
                Req:respond({404, [?H_PLAIN],
                            <<"Resource file does not exist.">>})
        end
    end.

respond(Req, Data, Format, Callback) ->
    Data2 = case Format of
                "xml"  -> to_xml(Data);
                "json" -> to_json(Data);
                _Type  -> to_json(Data)
            end,
    case Callback of
        undefined -> Req:respond({200, [?H_JSON], Data2});
        []        -> Req:respond({200, [?H_JSON], Data2});
        _         ->
            JS = list_to_binary([Callback, "(", Data2, ")"]),
            Req:respond({200, [?H_JSCRIPT], JS})
    end.

parse_arg({Arg, Type}, Params) ->
    parse_arg({Arg, Type, undefined}, Params);
parse_arg({Arg, Type, Def}, Params) ->
    case proplists:get_value(Arg, Params) of
        undefined -> Def;
        Val       -> format(Type, Val)
    end.

%%--------------------------------------------------------------------
%% Handle HTTP Request
%%--------------------------------------------------------------------

handle_request(Req, State) ->
    Path = Req:get(path),
    Method = Req:get(method),
    lager:info("Execute path=~s, method=~s", [Path, Method]),
    handle_request(Path, Req, State).

handle_request(?API_POLLING, Req, #state{dispatch = Dispatch}) ->
    Params = params(Req),
    Dispatch(Req, ?API_POLLING, Params);
handle_request(?API_WEBSOCKET, Req, #state{dispatch = _Dispatch}) ->
    case is_websocket(Req) of
        true  ->
            nextalk_websocket:start_link(Req);
        false ->
            Req:respond(400, [?H_PLAIN], <<"Bad Request">>)
    end;
    %Params = params(Req),
    %Dispatch(Req, ?API_WEBSOCKET, Params);
handle_request(?API ++ Path, Req, #state{dispatch = Dispatch}) ->
    Params = params(Req),
    Fun = fun() -> Dispatch(Req, ?API ++ Path, Params) end,
    if_authorized(Req, Fun);
handle_request("/" ++ Rest, Req, #state{docroot = DocRoot}) ->
    mochiweb_request:serve_file(Rest, DocRoot, Req).

params(Req) ->
    case Req:get(method) of
        'GET'  -> Req:parse_qs();
        'POST' -> Req:parse_post()
    end.

is_websocket(Req) ->
    Upgrade = Req:get_header_value("Upgrade"),
    Upgrade =/= undefined andalso 
    string:to_lower(Upgrade) =:= "websocket".

%%--------------------------------------------------------------------
%% Basic Authorization
%%--------------------------------------------------------------------

if_authorized(Req, Fun) ->
    case authorized(Req) of
        true  -> Fun();
        false -> Req:respond({401, [?H_AUTH], []})
    end.

authorized(Req) ->
    Auth = Req:get_header_value("Authorization"),
    case Auth of
        "Basic " ++ BasicAuth ->
            {Key, Pwd} = tenant_app(BasicAuth),
            Result = nextalk_auth:check(bin(Key), bin(Pwd)),
            case Result of
                {ok,    _Tenant} -> true;
                {error, _Reason} -> false
            end;
        _Other -> false
    end.

tenant_app(BasicAuth) ->
    Subject = base64:decode(BasicAuth),
    list_to_tuple(binary:split(Subject, <<":">>)).

%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

to_json([])   -> <<"[]">>;
to_json(Data) -> iolist_to_binary(mochijson2:encode(Data)).

to_xml([])   -> <<"[]">>;
to_xml(Data) -> iolist_to_binary(mochijson2:encode(Data)).

format(string, S) -> S;
format(binary, S) -> list_to_binary(S);
format(int,    S) -> list_to_integer(S).


bin(S) when is_list(S)   -> list_to_binary(S);
bin(A) when is_atom(A)   -> bin(atom_to_list(A));
bin(B) when is_binary(B) -> B.


strftime({MegaSecs, Secs, _MicroSecs}) ->
    strftime(datetime(MegaSecs * 1000000 + Secs));
strftime({{Y,M,D}, {H,MM,S}}) ->
    lists:flatten(
        io_lib:format(
            "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
            [Y, M, D, H, MM, S])).

datetime(Timestamp) when is_integer(Timestamp) ->
    Universal = calendar:gregorian_seconds_to_datetime(Timestamp +
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    calendar:universal_time_to_local_time(Universal).
