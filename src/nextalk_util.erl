%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc User ticket supervisor.
%% @author zhangsong
%% @since 0.1, 2016-7-27
%% @version 0.1

-module(nextalk_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([datetime/1,timestamp/0]).


datetime(Timestamp) when is_integer(Timestamp) ->
    Universal = calendar:gregorian_seconds_to_datetime(Timestamp +
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
    calendar:universal_time_to_local_time(Universal).

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.