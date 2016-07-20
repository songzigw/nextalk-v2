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

-define(EMPTY_KEY(Key), ((Key == undefined) orelse (Key == <<>>))).

-type mqtt_admin() :: #mqtt_admin{}.
-type nt_ticket() :: #nt_ticket{}.

-record(mqtt_admin, {username, password, tags}).

-record(nt_tenant_app, {
        app_key          :: binary(),
        app_secret       :: binary(),
        status = pending :: atom()}).

-record(nt_ticket, {
        t_id             :: binary(),
        uid              :: binary(),
        nick             :: binary(),
        avatar           :: binary(),
        status = pending :: atom()}).

-record(nt_message, {
        from            :: binary(),
        nick            :: binary(),
        to              :: binary(),
        timestamp       :: float(),
        type = chat     :: chat | grpchat,
        body = <<>>     :: binary()}).

-record(nt_user, {
        uid                 :: binary(),
        name                :: binary(),
        nick                :: binary(),
        domain              :: binary(),
        show = available    :: available | away | chat | dnd | invisible | unavailable,
        status = <<>>       :: binary()}).

