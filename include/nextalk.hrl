%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @author zhangsong

-define(APP, nextalk).
-define(VERSION, "0.1").
-define(COPYRIGHT, "Copyright (C) 2016 NexTalk.IM").
-define(IDLE_TIMEOUT, 10000).
-define(POLL_TIMEOUT, 28000).

-define(EMPTY_KEY(Key), ((Key == undefined) orelse (Key == <<>>))).

-record(mqtt_admin, {username, password, tags}).
-type mqtt_admin()  :: #mqtt_admin{}.
-type topic_class() :: uid | gid | atom().

-record(nextalk_tenant_app, {
        app_key          :: binary(),
        app_secret       :: binary(),
        status = pending :: atom()}).

%Example: {<<"/domain/$app_key/$class/$name">>, <<"$app_key">>, $class, <<"$name">>}
-record(nextalk_topic, {
        domain  :: atom(),
        class   :: topic_class(),
        name    :: binary()}).

-record(nextalk_message, {
        from            :: binary(),
        nick            :: binary(),
        to              :: binary(),
        timestamp       :: float(),
        type = chat     :: chat | grpchat,
        body = <<>>     :: binary()}).

-record(nextalk_endpoint, {
        uid                 :: binary(),
        name                :: binary(),
        nick                :: binary(),
        domain              :: binary(),
        show = available    :: available | away | chat | dnd | invisible | unavailable,
        status = <<>>       :: binary()}).

