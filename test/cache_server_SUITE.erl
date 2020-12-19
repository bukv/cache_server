-module(cache_server_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([
    all/0,
    init_per_suite/1, 
    end_per_suite/1,
    init_per_testcase/2, 
    end_per_testcase/2
    ]).

%% Test cases
-export([
    start_link/1,
    insert/1,
    insert_with_db/1,
    lookup/1,
    lookup_by_date/1,
    stop/1
    ]).


all() -> [
    start_link, 
    insert,
    lookup,
    lookup_by_date,
    stop
    ].

%% Common test callbacks
init_per_suite(Config) ->
    DropInterval = {drop_interval, 300},
    TableName = {table_name, test_table},
    Key1 = {key1, key_1},
    Val1 = {val1, val_1},
    TTL1 = {ttl1, 60},
    [DropInterval, TableName, Key1, Val1, TTL1|Config].

init_per_testcase(lookup_by_date, Config) ->
    DateFrom = {date_from,{{2015,1,1},{00,00,00}}},
    DateTo = {date_to,{{2015,1,10},{23,59,59}}},
    [DateFrom,DateTo|Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%% Test cases
start_link(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    {ok, Pid} = cache_server:start_link(TableName, [{drop_interval,DropInterval}]),
    true = is_pid(Pid).

insert(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key1 = proplists:get_value(key1, Config),
    Val1 = proplists:get_value(val1, Config),
    TTL1 = proplists:get_value(ttl1, Config),
    cache_server:start_link(TableName, [{drop_interval,DropInterval}]),
    ok = cache_server:insert(TableName, Key1, Val1, TTL1).

insert_with_db(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key1 = proplists:get_value(key1, Config),
    Val1 = proplists:get_value(val1, Config),
    TTL1 = proplists:get_value(ttl1, Config),
    cache_server:start_link(TableName, [{drop_interval,DropInterval}]),
    [{<<"ets">>,ok},{<<"db">>,ok}] = cache_server:insert_with_db(TableName, Key1, Val1, TTL1).

lookup(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key1 = proplists:get_value(key1, Config),
    Val1 = proplists:get_value(val1, Config),
    TTL1 = proplists:get_value(ttl1, Config),
    cache_server:start_link(TableName, [{drop_interval,DropInterval}]),
    cache_server:insert(TableName, Key1, Val1, TTL1), 
    {ok,Val1} = cache_server:lookup(TableName, Key1).

lookup_by_date(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    DateFrom = proplists:get_value(date_from, Config),
    DateTo = proplists:get_value(date_to, Config),
    cache_server:start_link(TableName, [{drop_interval,DropInterval}]),
    [] = cache_server:lookup_by_date(TableName, DateFrom, DateTo).

stop(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    cache_server:start_link(TableName, [{drop_interval,DropInterval}]),
    stopped = cache_server:stop().