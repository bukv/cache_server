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
    insert_with_ttl/1,
    insert_from_list/1,
    insert_from_list_with_ttl/1,
    insert_with_db/1,
    insert_with_db_and_ttl/1,
    lookup/1,
    db_lookup/1,
    lookup_by_date/1,
    db_lookup_by_date/1,
    db_delete_item/1,
    stop/1
    ]).


all() -> [
    start_link, 
    insert,
    insert_with_ttl,
    insert_from_list,
    insert_from_list_with_ttl,
    insert_with_db,
    insert_with_db_and_ttl,
    lookup,
    db_lookup,
    lookup_by_date,
    db_lookup_by_date,
    db_delete_item,
    stop
    ].

%% Common test callbacks
init_per_suite(Config) ->
    DropInterval = {drop_interval, 300},
    TableName = {table_name, test_table},
    TableNameDB = {db_table_name, db_test_table},
    Key = {key, key_1},
    Key2 = {key2, key_2},
    Val = {val, val_1},
    Val2 = {val2, val_2},
    TTL = {ttl, 60},
    TTL2 = {ttl2, 30},
    DateFrom = {date_from,{{2015,1,1},{00,00,00}}},
    DateTo = {date_to,{{2035,1,10},{23,59,59}}},
    [
        DateFrom, 
        DateTo, 
        DropInterval, 
        TableName, 
        TableNameDB, 
        Key, 
        Key2, 
        Val, 
        Val2, 
        TTL, 
        TTL2
        |Config
    ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%% Test cases
start_link(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    TTL = proplists:get_value(ttl, Config),
    {ok, Pid} = cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    true = is_pid(Pid).

insert(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    ok = cache_server:insert(Key, Val).

insert_with_ttl(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl2, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    ok = cache_server:insert(Key, Val, TTL).

insert_from_list(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    ok = cache_server:insert_from_list([{Key, Val}]).

insert_from_list_with_ttl(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl2, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    ok = cache_server:insert_from_list([{Key, Val}], TTL).

insert_with_db(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    [{<<"ets">>,ok},{<<"db">>,ok}] = cache_server:insert_with_db(Key, Val).

insert_with_db_and_ttl(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key2, Config),
    Val = proplists:get_value(val2, Config),
    TTL = proplists:get_value(ttl2, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    [{<<"ets">>,ok},{<<"db">>,ok}] = cache_server:insert_with_db(Key, Val, TTL).

lookup(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    cache_server:insert(Key, Val), 
    {ok,Val} = cache_server:lookup(Key).

db_lookup(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    {ok,Val} = cache_server:db_lookup(Key).

lookup_by_date(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DateFrom = proplists:get_value(date_from, Config),
    DateTo = proplists:get_value(date_to, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    cache_server:insert(Key, Val),
    [[{<<"key">>,Key},{<<"value">>,Val}]] = cache_server:lookup_by_date(DateFrom, DateTo).

db_lookup_by_date(Config) ->
    TableName = proplists:get_value(table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DateFrom = proplists:get_value(date_from, Config),
    DateTo = proplists:get_value(date_to, Config),
    Key = proplists:get_value(key, Config),
    Val = proplists:get_value(val, Config),
    Key2 = proplists:get_value(key2, Config),
    Val2 = proplists:get_value(val2, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    [{Key,Val},{Key2,Val2}] = cache_server:db_lookup_by_date(DateFrom, DateTo).

db_delete_item(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    Key = proplists:get_value(key, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    ok = cache_server:db_delete_item(Key).

stop(Config) ->
    TableName = proplists:get_value(table_name, Config),
    TableNameDB = proplists:get_value(db_table_name, Config),
    DropInterval = proplists:get_value(drop_interval, Config),
    TTL = proplists:get_value(ttl, Config),
    cache_server:start_link(TableName, TableNameDB, [{drop_interval,DropInterval},{default_ttl, TTL}]),
    stopped = cache_server:stop().