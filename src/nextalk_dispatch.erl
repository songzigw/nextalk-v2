%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc HTTP request dispatch manager.
%% @author zhangsong
%% @since 0.1, 2016-7-8;
%% @version 0.1


-module(nextalk_dispatch).

-include("nextalk.hrl").

-import(proplists, [get_value/2]).
-export([http_handler/0, handle_request/2]).

-define(API, "/api").
-define(API_WEBSOCKET, "/api/websocket").
-define(API_LPOLLING, "/api/lpolling").
-record(state, {dispatch, websocket, lpolling}).

%%--------------------------------------------------------------------
%% HTTP Handler and Dispatcher
%%--------------------------------------------------------------------

http_handler() ->
    {ok, Modules} = application:get_key(?APP, modules),
    APIs = lists:append(lists:map(fun http_api/1, Modules)),
    State = #state{dispatch = dispatcher(APIs),
                   websocket = websocket(),
                   lpolling = long_polling()},
    {?MODULE, handle_request, [State]}.

http_api(Mod) ->
    [{Name, {Mod, Fun, Args}} || {http_api, [{Name, Fun, Args}]} <- Mod:module_info(attributes)].

dispatcher(APIs) ->
    fun(Req, Name, Params) ->
        case get_value(Name, APIs) of
            {Mod, Fun, ArgDefs} ->
                Args = lists:map(fun(Def) -> parse_arg(Def, Params) end, ArgDefs),
                case catch apply(Mod, Fun, Args) of
                    {ok, Data} ->
                        respond(Req, 200, Data);
                    {'EXIT', Reason} ->
                        lager:error("Execute API '~s' Error: ~p", [Name, Reason]),
                        respond(Req, 404, []);
                    _ ->
                        respond(Req, 404, [])
                end;
            undefined ->
                respond(Req, 404, [])
        end
    end.

websocket() ->
    fun(Req) -> nextalk_websocket:start_link(Req) end.

long_polling() ->
    fun(Req) -> nextalk_lpolling:loop(Req) end.

parse_arg({Arg, Type}, Params) ->
    parse_arg({Arg, Type, undefined}, Params);
parse_arg({Arg, Type, Def}, Params) ->
    case get_value(Arg, Params) of
        undefined -> Def;
        Val       -> format(Type, Val)
    end.

respond(Req, 401, Data) ->
    Req:respond({401, [{"WWW-Authenticate", "Basic Realm=\"NexTalk\""}], Data});
respond(Req, 404, Data) ->
    Req:respond({404, [{"Content-Type", "text/plain"}], Data});
respond(Req, 200, Data) ->
    Req:respond({200, [{"Content-Type", "application/json"}], to_json(Data)});
respond(Req, Code, Data) ->
    Req:respond({Code, [{"Content-Type", "text/plain"}], Data}).

%%--------------------------------------------------------------------
%% Handle HTTP Request
%%--------------------------------------------------------------------

handle_request(Req, State) ->
    Method = Req:get(method), 
    Path = Req:get(path),
    Fun = fun() -> handle_request(Method, Path, Req, State) end,
    if_authorized(Req, Fun).


is_websocket(Upgrade) -> 
    Upgrade =/= undefined andalso string:to_lower(Upgrade) =:= "websocket".

handle_request('GET', ?API_WEBSOCKET, Req, #state{websocket = Websocket}) ->
    Upgrade = Req:get_header_value("Upgrade"),
    case is_websocket(Upgrade) of
        true  -> catch Websocket(Req);
        false ->
            lager:error("Not WobSocket: Upgrade = ~s", [Upgrade]),
            respond(Req, 400, <<"Bad Request">>)
    end;

handle_request('GET', ?API_LPOLLING, Req, #state{lpolling = LPolling}) ->
    catch LPolling(Req);

handle_request('GET', ?API ++ Path, Req, #state{dispatch = Dispatch}) ->
    Dispatch(Req, Path, Req:parse_qs());

handle_request('POST', ?API ++ Path, Req, #state{dispatch = Dispatch}) ->
    Dispatch(Req, Path, Req:parse_post());

handle_request(Method, Path, Req, _State) ->
	lager:error("badrequest: method = ~p, path = ~s", [Method, Path]),
    respond(Req, 404, <<"Resource file does not exist">>).

%%--------------------------------------------------------------------
%% Basic Authorization
%%--------------------------------------------------------------------

if_authorized(Req, Fun) ->
    case authorized(Req) of
        true  -> Fun();
        false -> respond(Req, 401,  [])
    end.

authorized(Req) ->
    case Req:get_header_value("Authorization") of
        "Basic " ++ BasicAuth ->
            {Username, Password} = user_passwd(BasicAuth),
            case nextalk_auth:check(bin(Username), bin(Password)) of
                ok -> true;
                {error, Reason} ->
                    lager:error("HTTP Auth failure: username=~s, reason=~p",
                                [Username, Reason]),
                    false
            end;
         _Other -> false
    end.

user_passwd(BasicAuth) ->
    list_to_tuple(binary:split(base64:decode(BasicAuth), <<":">>)).

to_json([])   -> <<"[]">>;
to_json(Data) -> iolist_to_binary(mochijson2:encode(Data)).

format(string, S) -> S;
format(binary, S) -> list_to_binary(S);
format(int, S)    -> list_to_integer(S).

bin(S) when is_list(S)   -> list_to_binary(S);
bin(A) when is_atom(A)   -> bin(atom_to_list(A));
bin(B) when is_binary(B) -> B.

