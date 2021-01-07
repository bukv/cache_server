-module(cache_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
    start_link/3, 
    stop/0,
    insert/3,
    insert/2,
    insert_from_list/2,
    insert_from_list/1,
    insert_with_db/2,
    insert_with_db/3,
    lookup/1,
    db_lookup/1,
    lookup_by_date/2,
    db_lookup_by_date/2,
    db_delete_item/1
    ]).

%% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    terminate/2, 
    code_change/3,
    handle_cast/2,
    handle_info/2
    ]).

-record(state, {
    table_name,
    db_table_name,
    drop_interval,
    default_ttl
}).

stop()  -> 
    gen_server:call(?MODULE, stop).

insert(Key, Value) -> 
    gen_server:call(?MODULE, {insert, Key, Value}).

insert(Key, Value, TTL) -> 
    gen_server:call(?MODULE, {insert_with_ttl, Key, Value, TTL}).

insert_from_list(PropList, TTL) -> 
    gen_server:call(?MODULE, {insert_from_list_with_ttl, PropList, TTL}).

insert_from_list(PropList) -> 
    gen_server:call(?MODULE, {insert_from_list, PropList}).

insert_with_db(Key, Value) -> 
    gen_server:call(?MODULE, {insert_with_db, Key, Value}).

insert_with_db(Key, Value, TTL) -> 
    gen_server:call(?MODULE, {insert_with_db_and_ttl, Key, Value, TTL}).

lookup(Key) -> 
    gen_server:call(?MODULE, {lookup,Key}).

db_lookup(Key) -> 
    gen_server:call(?MODULE, {db_lookup,Key}).

lookup_by_date(DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, DateFrom, DateTo}).

db_lookup_by_date(DateFrom, DateTo) ->
    gen_server:call(?MODULE, {db_lookup_by_date, DateFrom, DateTo}).

db_delete_item(Key) ->
    gen_server:call(?MODULE, {db_delete_item, Key}).

start_link(TableName, TableNameDB, ParamProp) -> 
    DropInterval = proplists:get_value(drop_interval, ParamProp),
    DefaultTTL = proplists:get_value(default_ttl, ParamProp),
    Params = #{table_name => TableName, drop_interval => DropInterval, db_table_name => TableNameDB, default_ttl => DefaultTTL},
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

init(Params) -> 
    TableName = maps:get(table_name, Params),
    TableNameDB = maps:get(db_table_name, Params),
    DropInterval = maps:get(drop_interval, Params),
    DefaultTTL = maps:get(default_ttl, Params),
    cache_ets:create(TableName),
    cache_db:create(TableNameDB),
    cache_db:start(TableNameDB),
    timer:apply_interval(DropInterval*1000, cache_ets, delete_obsolete, [TableName]),
    {ok, #state{
        table_name = TableName,
        db_table_name = TableNameDB,
        drop_interval = DropInterval,
        default_ttl = DefaultTTL
    }}.

handle_call({insert, Key, Value}, _From,  #state{table_name = TableName, default_ttl=TTL} = State) ->
    Reply = cache_ets:insert(TableName, Key, Value, TTL),
    {reply, Reply, State};
handle_call({insert_with_ttl, Key, Value, TTL}, _From,  #state{table_name = TableName} = State) ->
    Reply = cache_ets:insert(TableName, Key, Value, TTL),
    {reply, Reply, State};
handle_call({insert_from_list, PropList}, _From, #state{table_name = TableName, default_ttl=TTL} = State) ->
    Reply = cache_ets:insert_from_list(TableName, PropList, TTL),
    {reply, Reply, State};
handle_call({insert_from_list_with_ttl, PropList, TTL}, _From, #state{table_name = TableName} = State) ->
    Reply = cache_ets:insert_from_list(TableName, PropList, TTL),
    {reply, Reply, State};
handle_call({insert_with_db_and_ttl, Key, Value, TTL}, _From, #state{table_name = TableName, db_table_name = TableNameDB} = State) ->
    Reply = [
        {<<"ets">>,cache_ets:insert(TableName, Key, Value, TTL)},
        {<<"db">>,cache_db:insert(TableNameDB, Key, Value)}
    ],
    {reply, Reply, State};
handle_call({insert_with_db, Key, Value}, _From, #state{table_name = TableName, db_table_name = TableNameDB, default_ttl=TTL} = State) ->
    Reply = [
        {<<"ets">>,cache_ets:insert(TableName, Key, Value, TTL)},
        {<<"db">>,cache_db:insert(TableNameDB, Key, Value)}
    ],
    {reply, Reply, State};
handle_call({lookup, Key}, _From, #state{table_name = TableName} = State) ->
    Reply = cache_ets:lookup(TableName, Key),
    {reply, Reply, State};
handle_call({db_lookup, Key}, _From, #state{db_table_name = TableNameDB} = State) ->
    Reply = cache_db:lookup(TableNameDB, Key),
    {reply, Reply, State};
handle_call({lookup_by_date, DateFrom, DateTo}, _From, #state{table_name = TableName} = State) ->
    FromInSeconds = calendar:datetime_to_gregorian_seconds(DateFrom),
    ToInSeconds = calendar:datetime_to_gregorian_seconds(DateTo),
    Reply = cache_ets:lookup_by_date(TableName, FromInSeconds, ToInSeconds),
    {reply, Reply, State}; 
handle_call({db_lookup_by_date, DateFrom, DateTo}, _From,  #state{db_table_name = TableNameDB} = State) ->
    FromInSeconds = calendar:datetime_to_gregorian_seconds(DateFrom),
    ToInSeconds = calendar:datetime_to_gregorian_seconds(DateTo),
    Reply = cache_db:lookup_by_date(TableNameDB, FromInSeconds, ToInSeconds),
    {reply, Reply, State}; 
handle_call({db_delete_item, Key}, _From, #state{db_table_name = TableNameDB} = State) ->
    Reply = cache_db:delete_item(TableNameDB, Key),
    {reply, Reply, State};       
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason,  #state{table_name = TableName}) -> 
    cache_db:stop(),
    cache_ets:close_table(TableName).

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.