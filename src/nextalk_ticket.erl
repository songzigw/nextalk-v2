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

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([get_ticket/3]).

-http_api({"/api/get_ticket", get_ticket, [{uid,    binary},
                                           {nick,   binary},
                                           {avatar, binary},
                                           {tid,    binary}]}).

%% ====================================================================
%% API functions
%% ====================================================================

%% @doc Get the ticket
-spec get_ticket(Uid, Nick, Avatar, TID) ->
        {ok, nextalk_ticket()} when
        Uid    :: binary(),
        Nick   :: binary(),
        Avatar :: binary(),
        TID    :: binary().
get_ticket(Uid, Nick, Avatar, TID) ->
    Ticket = #nextalk_ticket{tid    = token(),
                             uid    = Uid,
                             nick   = Nick,
                             avatar = Avatar},
    nextalk_ticket_sup:start_child(Ticket),
    {ok, Ticket}.

-spec start_link(Ticket) -> 
        {ok, pid()} | ignore | {error, any()} when
        Ticket :: nextalk_ticket().
start_link(Ticket) ->
    #nextalk_ticket{tid = TID} = Ticket,
    gen_server:start_link({local, TID}, ?MODULE, [Ticket], []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([Ticket]) ->
    {ok, Ticket}.

handle_call(_Req, _From, State) ->
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

token() ->
    %random:seed(now()),
    I1 = random:uniform(round(math:pow(2, 48))) - 1,
    I2 = random:uniform(round(math:pow(2, 32))) - 1,
    L = lists:flatten(io_lib:format("~12.16.0b~8.16.0b", [I1, I2])),
    list_to_binary(L).
