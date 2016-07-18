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

get_ticket(Uid, Nick, Avatar) ->
    Ticket = 89987777,
    {ok, [Ticket, Uid, Nick, Avatar]}.


%% ====================================================================
%% Internal functions
%% ====================================================================


