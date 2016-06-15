%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @author zhangsong
%% @doc Authentication module.

-module(nextalk_auth).

-include("../../../include/emqttd.hrl").

-behaviour(emqttd_auth_mod).

-export([init/1, check/3, description/0]).

init(Opts) ->
    {ok, Opts}.

check(#mqtt_client{ws_initial_headers = undefined}, _Password, _Opts) ->
    ignore;

check(#mqtt_client{client_id = <<"dashboard_", _/binary>>,
                   username  = <<"dashboard">>,
                   ws_initial_headers = Headers}, _Password, [{_Proto, Port, _}]) ->
    Origin = proplists:get_value("Origin", Headers, ""),
    %%TODO: workaround first...
    case string:rstr(Origin, integer_to_list(Port)) of
        0  -> ignore;
        _I -> ok
    end;

check(_Client, _Password, _Opts) ->
    ignore.

description() ->
    "Authentication Module".
