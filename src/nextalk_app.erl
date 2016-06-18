%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @author zhangsong
%% @doc NexTalk Application.

-module(nextalk_app).

-include("nextalk.hrl").

-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start(_Type, _Args) ->
    {ok, Sup} = nextalk_sup:start_link(),
    {ok, HTTP} = application:get_env(?APP, http),
    open_listener(HTTP),
    {ok, Sup}.

stop(_State) ->
    {ok, {Port, _Opts}} = application:get_env(?APP, http),
    mochiweb:stop_http(Port).

%% ====================================================================
%% Internal functions
%% ====================================================================

open_listener({Port, Options}) ->
    mochiweb:start_http(Port, Options, nextalk_dispatch:http_handler()).
