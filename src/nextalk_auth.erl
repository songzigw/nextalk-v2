%%--------------------------------------------------------------------
%% Copyright (c) 2016 nextalk.im <feng@emqtt.io>.
%%--------------------------------------------------------------------

%% @doc Identification and authorization.
%% @author zhangsong
%% @since 0.1, 2016-7-8
%% @version 0.1

-module(nextalk_auth).
-behaviour(gen_server).
-include("nextalk.hrl").

-define(SERVER, ?MODULE).

%% gen_server function export
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0, check/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check(AppKey, AppSecret) when
    is_binary(AppKey) and is_binary(AppSecret) ->
    case ets:lookup(nt_tenant_app, AppKey) of
        [TenantApp = #nt_tenant_app{
            app_secret = <<Salt:4/binary, Hash/binary>>}] ->
            case Hash =:= md5_hash(Salt, AppSecret) of
                true  -> {ok, TenantApp};
                false -> {error, <<"AppSecret Invalid">>}
            end;
        []          ->
            {error, <<"AppKey Not Found">>};
        [_]         ->
            {error, <<"AppSecret Invalid">>}
    end.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    ets:new(nt_tenant_app, [set, protected, named_table, {keypos, 2}]),
    {ok, Apps} = application:get_env(?APP, auth_apps),
    Fun = fun({AppKey, AppSecret}) ->
              ets:insert(nt_tenant_app, tenant_app(AppKey, AppSecret))
          end,
    lists:foreach(Fun, Apps),
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
    #nt_tenant_app{app_key    = binary(AppKey),
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
