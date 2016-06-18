%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @author zhangsong
%% @doc Authentication module.

-module(nextalk_auth).
-include("nextalk.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, check/2]).

-spec(start_link() -> {ok, pid()} | ignore | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check(AppKey, AppSecret) when is_binary(AppKey) and is_binary(AppSecret) ->
    Secret = <<Salt:4/binary, Hash/binary>,
    TenantApp = #nextalk_tenant_app{app_secret = Secret>},
    case ets:lookup(nextalk_tenant_app, AppKey) of
        [TenantApp] ->
            case Hash =:= md5_hash(Salt, AppSecret) of
                true  -> {ok, TenantApp};
                false -> {error, <<"AppSecret Invalid">>}
            end;
        [_]         ->
            {error, <<"AppSecret Invalid">>};
        []          ->
            {error, <<"AppKey Not Found">>}
    end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    ets:new(nextalk_tenant_app, [set, protected, named_table, {keypos, 2}]),
    {ok, Apps} = application:get_env(?APP, auth_apps),
    [ets:insert(nextalk_tenant_app, tenant_app(AppKey, AppSecret)) || {AppKey, AppSecret} <- Apps],
    {ok, state}.

handle_call(Req, _From, State) ->
    {stop, {error, {badreq, Req}}, State}.

handle_cast(Msg, State) ->
    {stop, {error, {badmsg, Msg}}, State}.

handle_info(Info, State) ->
    {stop, {error, {badinfo, Info}}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
tenant_app(AppKey, AppSecret) ->
    #nextalk_tenant_app{app_key    = binary(AppKey),
                        app_secret = hash(binary(AppSecret)),
                        status     = binary(active)}.

hash(AppSecret) ->
    SaltBin = salt(),
    <<SaltBin/binary, (md5_hash(SaltBin, AppSecret))/binary>>.

md5_hash(SaltBin, AppSecret) ->
    erlang:md5(<<SaltBin/binary, AppSecret/binary>>).

salt() ->
    emqttd_time:seed(),
    Salt = random:uniform(16#ffffffff),
    <<Salt:32>>.

binary(S) when is_list(S)   -> list_to_binary(S);
binary(A) when is_atom(A)   -> binary(atom_to_list(A));
binary(B) when is_binary(B) -> B.
