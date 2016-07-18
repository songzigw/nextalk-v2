%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc NextTalk loog polling server.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_polling).

-include("nextalk.hrl").

-http_api({"/api/polling",
           polling,
           [{ticket, binary},
            {session_id, binary}]}).

-export([polling/2, resume/0]).

%% ====================================================================
%% API functions
%% ====================================================================

polling(Ticket, SessionID) ->
    Pid = start(),
    rpc(Pid).


%% ====================================================================
%% Internal functions
%% ====================================================================

start() -> spawn_link(fun ?MODULE:resume/0).

rpc(Pid) ->
    From = self(),
    Pid ! {From, msg},
    receive
        {Pid, Data} -> Data
    after ?POLL_TIMEOUT
        {ok, {success, 0}}
    end.

resume() ->
    receive
        {From, msg} ->
            From ! {self(), {ok, {success, 1}}}
    end.
