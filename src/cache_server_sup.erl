%%%-------------------------------------------------------------------
%% @doc cache_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cache_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(cache_server_sup, []).

init([]) ->
    {ok, TableName} = application:get_env(cache_server, table_name),
    {ok, TableNameDB} = application:get_env(cache_server, db_table_name),
    {ok, DropInterval} = application:get_env(cache_server, drop_interval),
    {ok, DefaultTTL} = application:get_env(cache_server, default_ttl),
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => cache_server,
                    start => {cache_server, start_link, [TableName, TableNameDB, [{drop_interval, DropInterval},{default_ttl, DefaultTTL}]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [cache_server]}],
    {ok, {SupFlags, ChildSpecs}}.