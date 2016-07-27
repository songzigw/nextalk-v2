%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc User Information.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_user).
-behaviour(gen_server).
-include("nextalk.hrl").

-record(state, {}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================



%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


