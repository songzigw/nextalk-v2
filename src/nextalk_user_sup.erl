%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc User manager.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_user_sup).
-behaviour(supervisor).
-include("nextalk.hrl").

-define(SUP_NAME, ?MODULE).

-export([init/1]).
-export([start_link/0, start_child/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

start_child(Uid) when is_atom(Uid) ->
    supervisor:start_child(?SUP_NAME,
        {Uid, {nextalk_user, start_link, []},
         permanent, 5000, worker, [nextalk_user]}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok, { {one_for_all, 10, 100}, [] } }.

%% ====================================================================
%% Internal functions
%% ====================================================================
