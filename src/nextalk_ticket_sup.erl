%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc User ticket supervisor.
%% @author zhangsong
%% @since 0.1, 2016-7-26
%% @version 0.1

-module(nextalk_ticket_sup).
-behaviour(supervisor).
-include("nextalk.hrl").

-define(SUP_NAME, ?MODULE).
-define(TAB, nextalk_ticket).

-export([init/1]).
-export([start_link/0, start_child/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, []).

start_child(Token) when is_atom(Token) ->
    supervisor:start_child(?SUP_NAME,
        {Token, {nextalk_ticket, start_link, []},
         permanent, 5000, worker, [nextalk_ticket]}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    %% Create ticket table
    create_ticket_tab(),
    {ok, { {one_for_all, 10, 100}, [] } }.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_ticket_tab() ->
    case ets:info(?TAB, name) of
        undefined ->
            ets:new(?TAB, [ordered_set, named_table, public,
                           {keypos, 2}, {write_concurrency, true}]);
        _ ->
            ok
    end.