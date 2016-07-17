%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc NextTalk websocket server.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_websocket).

-behavior(gen_server).

-include("nextalk.hrl").

-record(client_state, {ws_pid, request, reply_channel, monitor, route_pid, ticket, user_pid}).

%% API function export
-export([start_link/1, ws_loop/3]).

%% gen_server function export
-export([init/1,
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link(Req) ->
    {ReentryWs, ReplyChannel} =
        mochiweb_websocket:upgrade_connection(Req, fun ?MODULE:ws_loop/3),
    Params = [self(), Req, ReplyChannel],
    {ok, UserPid} = gen_server:start_link(?MODULE, Params, []),
    ReentryWs(UserPid).

ws_loop(Payload, UserPid, _ReplyChannel) ->
    lager:info("Received data: ~p~n", [Payload]),
    UserPid.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([WsPid, Req, ReplyChannel]) ->
    MRef = erlang:monitor(process, WsPid),
    {ok, #client_state{ws_pid = WsPid,
                       request = Req,
                       reply_channel = ReplyChannel,
                       monitor = MRef}}.

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

