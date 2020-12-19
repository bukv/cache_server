%%%-------------------------------------------------------------------
%% @doc cache_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cache_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-include("../include/server_conf.hrl").

start_link() ->
    supervisor:start_link(cache_server_sup, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => cache_server,
                    start => {cache_server, start_link, [?TABLE_NAME, [{drop_interval, ?DROP_INTERVAL}]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [cache_server]}],
    {ok, {SupFlags, ChildSpecs}}.