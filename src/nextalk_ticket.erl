%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc User ticket.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1
%% @doc @todo Add description to nextalk_ticket.

-module(nextalk_ticket).

-include("nextalk.hrl").

-export([get_ticket/3]).

-http_api({"/api/get_ticket",
           get_ticket,
           [{uid,    binary},
            {nick,   binary},
            {avatar, binary}]}).

%% ====================================================================
%% API functions
%% ====================================================================

%% make(Uid, Nick, Avatar)
%% 获得令牌
-spec make(Uid, Nick, Avatar) ->
    {ok, nt_ticket()} when
    Uid    :: binary(),
    Nick   :: binary(),
    Avatar :: binary().
get_ticket(Uid, Nick, Avatar) ->
    Ticket = #nt_ticket{token  = token(),
                        uid    = Uid,
                        nick   = Nick,
                        avatar = Avatar},
    {ok, Ticket}.


%% ====================================================================
%% Internal functions
%% ====================================================================

token() ->
    random:seed(now()),
    I1 = random:uniform(round(math:pow(2, 48))) - 1,
    I2 = random:uniform(round(math:pow(2, 32))) - 1,
    L = lists:flatten(io_lib:format("~12.16.0b~8.16.0b", [I1, I2])),
    list_to_binary(L).
