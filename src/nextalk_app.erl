%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @author zhangsong
%% @doc NexTalk Application.


-module(nextalk_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT_MSG(Msg), io:format(Msg)).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

start(_Type, _Args) ->
    {ok, Sup} = nextalk_sup:start_link(),
    {ok, Listener} = application:get_env(nextalk, listener),
    ok = emqttd_access_control:register_mod(auth, nextalk_auth, [Listener], 9999),
    open_listener(Listener),
    {ok, Sup}.

stop(_State) ->
    emqttd_access_control:unregister_mod(auth, nextalk_auth),
    {ok, {_Proto, Port, _Opts}} = application:get_env(nextalk, listener),
    mochiweb:stop_http(Port).

%% ====================================================================
%% Internal functions
%% ====================================================================

open_listener({_Http, Port, Options}) ->
    mochiweb:start_http(Port, Options, nextalk:http_handler()).
