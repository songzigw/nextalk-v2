%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc NextTalk loog polling server.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_polling).

-include("nextalk.hrl").

-http_api({"/api/polling", polling, [{ticket, binary},
                                     {session_id, binary}]}).

-export([polling/2, resume/0]).

%% ====================================================================
%% API functions
%% ====================================================================

polling(Ticket, SessionID) ->
    erlang:send_after(?POLL_TIMEOUT, self(), timeout),
    proc_lib:hibernate(?MODULE, resume, []).

resume() ->
    receive
        {ok  , _Packets} -> "{\"success\": 1}";
        {stop, _Reason}  -> "{\"success\": 0}";
        timeout          -> "{}"
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

