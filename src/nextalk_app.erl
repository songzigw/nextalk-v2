%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc NextTalk Application.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_app).

-include("nextalk.hrl").

-behaviour(application).

-export([start/2, stop/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start(_Type, _Args)
-spec start(Type, Args) ->
    {ok, pid()} when
    Type :: normal | {takeover, node()} | {failover, node()},
    Args :: term().
start(_Type, _Args) ->
    {ok, Sup} = nextalk_sup:start_link(),
    open_services(Sup),
    open_listeners(),
    {ok, Sup}.

stop(_State) ->
    {ok, {Port, _Opts}} = application:get_env(?APP, http),
    mochiweb:stop_http(Port).

%% ====================================================================
%% Internal functions
%% ====================================================================

open_listeners() ->
    {ok, {Port, Options}} = application:get_env(?APP, http),
    mochiweb:start_http(Port, Options, nextalk_dispatch:http_handler()).

open_services(Sup) ->
    start_child(Sup, nextalk_auth),
    start_child(Sup, {supervisor, nextalk_ticket_sup}).

start_child(Sup, {supervisor, Module}) ->
    supervisor:start_child(Sup, supervisor_spec(Module));

start_child(Sup, Module) when is_atom(Module) ->
    supervisor:start_child(Sup, worker_spec(Module)).

start_child(Sup, {supervisor, Module}, Opts) ->
    supervisor:start_child(Sup, supervisor_spec(Module, Opts));

start_child(Sup, Module, Opts) when is_atom(Module) ->
    supervisor:start_child(Sup, worker_spec(Module, Opts)).

supervisor_spec(Module) when is_atom(Module) ->
    supervisor_spec(Module, start_link, []).

supervisor_spec(Module, Opts) ->
    supervisor_spec(Module, start_link, [Opts]).

supervisor_spec(M, F, A) ->
    {M, {M, F, A}, permanent, infinity, supervisor, [M]}.

worker_spec(Module) when is_atom(Module) ->
    worker_spec(Module, start_link, []).

worker_spec(Module, Opts) when is_atom(Module) ->
    worker_spec(Module, start_link, [Opts]).

worker_spec(M, F, A) ->
    {M, {M, F, A}, permanent, 10000, worker, [M]}.
