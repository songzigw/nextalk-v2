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

-record(state, {user = #nextalk_user{}}).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Start link
-spec start_link(User) ->
        {ok, pid()} | ignore | {error, any()} when
        User :: nextalk_user().
start_link(User) ->
    #nextalk_user{uid = UID} = User,
    Uid = binary_to_atom(UID, utf8),
    gen_server:start_link({local, Uid}, ?MODULE, [User], []).

%% @doc User online
online(User) ->
    #nextalk_user{uid = UID} = User,
    Uid = binary_to_atom(UID, utf8),
    case erlang:whereis(Uid) of
        undefined -> nextalk_user_sup:start_child(User);
        _         -> ok
    end.

%% @doc User offline
offline(UID) ->
    Uid = binary_to_atom(UID, utf8),
    gen_server:call(Uid, offline).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([User]) ->
    {ok, #state{user = User}}.

handle_call(offline, _From, State) ->
    {stop, offline, State}.

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


