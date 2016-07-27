%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc NexTalk application supervisor.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_sup).
-behaviour(supervisor).

-define(SUP_NAME, ?MODULE).
-define(CHILD(Mod, Type), {Mod, {Mod, start_link, []},
                           permanent, 5000, Type, [Mod]}).

-export([init/1]).
-export([start_link/0, start_child/2]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

start_child(Mod, Type) when is_atom(Mod), is_atom(Type) ->
    supervisor:start_child(?SUP_NAME, ?CHILD(Mod, Type)).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok, { {one_for_all, 10, 100}, [] } }.

%% ====================================================================
%% Internal functions
%% ====================================================================

