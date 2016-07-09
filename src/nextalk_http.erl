%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc Business interface.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_http).
-include("nextalk.hrl").
-import(proplists, [get_value/2]).
-export([online/4]).

-http_api({"/online", online, [{ticket, binary},
                               {show,   binary, <<"available">>},
                               {nick,   binary},
                               {avatar, binary}]}).


online(Ticket, Show, Nick, Avatar) ->
    {ok, [Ticket, Show, Nick, Avatar]}.


bsplit(<<>>, _C) -> 
	[];
bsplit(B, C) -> 
	bsplit(B, C, <<>>, []).

bsplit(<<C, Rest/binary>>, C, Acc, Tokens) ->
    bsplit(Rest, C, <<>>, [Acc | Tokens]);
bsplit(<<C1, Rest/binary>>, C, Acc, Tokens) ->
    bsplit(Rest, C, <<Acc/binary, C1>>, Tokens);
bsplit(<<>>, _C, Acc, Tokens) ->
    lists:reverse([Acc | Tokens]).

b2a(Binary) ->
  list_to_atom(binary_to_list(Binary)).


