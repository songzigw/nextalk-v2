%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc User ticket.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_ticket).
-behaviour(gen_server).
-include("nextalk.hrl").

-define(INTERVAL, 1000 * 60).
-define(TIMEOUT, 60 * 10).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1,
         get_ticket/4,
         is_valid/1,
         stop/1,
         update_time/1]).

-http_api({"/api/get_ticket", get_ticket, [{uid,    binary},
                                           {nick,   binary},
                                           {avatar, binary},
                                           {tid,    binary}]}).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Get the ticket
-spec get_ticket(Uid, Nick, Avatar, TID) ->
        {ok, binary()} when
        Uid    :: binary(),
        Nick   :: binary(),
        Avatar :: binary(),
        TID    :: binary().
get_ticket(Uid, Nick, Avatar, TID) when ?EMPTY_KEY(TID) ->
    Ticket = #nextalk_ticket{tid = token(),
                             uid    = Uid,
                             nick   = Nick,
                             avatar = Avatar},
    nextalk_ticket_sup:start_child(Ticket),
    ets:insert(?TAB_TICKET, Ticket),
    {ok, Ticket#nextalk_ticket.tid}.
get_ticket(Uid, Nick, Avatar, TID) ->
    case is_valid(TID) of
        false ->
            get_ticket(Uid, Nick, Avatar, <<>>),
        true  ->
            update_user(TID, Nick, Avatar),
            {ok, TID}.
    end.

%% @doc Start link
-spec start_link(Ticket) ->
        {ok, pid()} | ignore | {error, any()} when
        Ticket :: nextalk_ticket().
start_link(Ticket) ->
    #nextalk_ticket{tid = TID} = Ticket,
    Tid = binary_to_atom(TID, utf8),
    gen_server:start_link({local, Tid}, ?MODULE, [Ticket], []).

%% @doc Stop ticket server
stop(TID) ->
    Tid = binary_to_atom(TID, utf8),
    ets:delete(?TAB_TICKET, Tid),
    gen_server:call(Tid, stop).

%% @doc Validity checking
checking() ->
    erlang:send_after(?INTERVAL, self(), checking).

%% @doc Update user information
update_user(TID, Nick, Avatar) ->
    Tid = binary_to_atom(TID, utf8),
    gen_server:call(Tid, {update_user, Nick, Avatar}),
    ets:update_element(?TAB_TICKET, Tid,
                       [{#nextalk_ticket.nick, Nick},
                        {#nextalk_ticket.avatar, Avatar}]).

%% @doc Update ticket time
update_time(TID) ->
    Tid = binary_to_atom(TID, utf8),
    Ts = nextalk_util:timestamp(),
    gen_server:call(Tid, {update_time, Ts}),
    ets:update_element(?TAB_TICKET, Tid,
                {#nextalk_ticket.time, Ts}).

%% @doc Get ticket user information
user(TID) ->
    Tid = binary_to_atom(TID, utf8),
    gen_server:call(Tid, user).

%% @doc Determine the ticket is valid
is_valid(TID) ->
    Tid = binary_to_atom(TID, utf8),
    case erlang:whereis(Tid) of
        undefined -> false;
        _         -> true
    end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([Ticket]) ->
    checking(), {ok, Ticket}.

handle_call({update_user, Nick, Avatar}, _From, State) ->
    NewState = State#nextalk_ticket{nick = Nick,
                                    avatar = Avatar},
    {reply, ok, NewState};
handle_call({update_time, Timestamp}, _From, State) ->
    NewState = State#nextalk_ticket{time = Timestamp},
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, logout, State};
handle_call(user, _From, State) ->
    #nextalk_ticket{uid = UID, nick = Nick, avatar = Avatar} = State,
    Reply = #nextalk_user{uid = UID, nick = Nick, avatar = Avatar},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(checking, #nextalk_ticket{time = Time, tid = TID} = State) ->
    Curr = nextalk_util:timestamp(),
    case (Curr - Time) > ?TIMEOUT of
        false ->
            checking(),
            {noreply, State};
        true  ->
            stop(TID),
            {stop, logout, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

token() ->
    %random:seed(now()),
    I1 = random:uniform(round(math:pow(2, 48))) - 1,
    I2 = random:uniform(round(math:pow(2, 32))) - 1,
    L = lists:flatten(io_lib:format("~12.16.0b~8.16.0b", [I1, I2])),
    list_to_binary(L).
