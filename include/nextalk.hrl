%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc NextTalk hrl.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-define(APP, nextalk).
-define(VERSION, "0.1").
-define(COPYRIGHT, "Copyright (C) 2016 NexTalk.IM").
-define(IDLE_TIMEOUT, 10000).
-define(POLL_TIMEOUT, 28000).

-define(TAB_TENANT_APP, nextalk_tenant_app).
-define(TAB_TICKET, nextalk_ticket).

-define(EMPTY_KEY(Key), ((Key == undefined) orelse (Key == <<>>))).

-record(mqtt_admin, {username, password, tags}).

-record(nextalk_tenant_app, {
        app_key          :: binary(),
        app_secret       :: binary(),
        status = pending :: atom()}).

-record(nextalk_user, {
        uid              :: binary(),
        nick             :: binary(),
        avatar           :: binary(),
        show = available :: available | away | chat | dnd | invisible | unavailable}).

-record(nextalk_ticket, {
        tid              :: binary(),
        uid              :: binary(),
        nick             :: binary(),
        avatar           :: binary(),
        time = nextalk_util:timestamp(),
        status = pending :: atom()}).

-record(nextalk_message, {
        from            :: binary(),
        nick            :: binary(),
        to              :: binary(),
        timestamp       :: float(),
        type = chat     :: chat | grpchat,
        body = <<>>     :: binary()}).

-type mqtt_admin() :: #mqtt_admin{}.
-type nextalk_user() :: #nextalk_user{}.
-type nextalk_ticket() :: #nextalk_ticket{}.
