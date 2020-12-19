-module(cache_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
    start_link/2, 
    stop/0,
    insert/4,
    insert_with_db/4,
    lookup/2,
    lookup_by_date/3
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

-record(state, {}).

stop()  -> 
    gen_server:call(?MODULE, stop).

insert(TableName, Key, Value, TTL) -> 
    gen_server:call(?MODULE, {insert, TableName, Key, Value, TTL}).

insert_with_db(TableName, Key, Value, TTL) -> 
    gen_server:call(?MODULE, {insert_with_db, TableName, Key, Value, TTL}).

lookup(TableName, Key) -> 
    gen_server:call(?MODULE, {lookup, TableName, Key}).

lookup_by_date(TableName, DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, TableName, DateFrom, DateTo}).

start_link(TableName, ParamProp) -> 
    DropInterval = proplists:get_value(drop_interval, ParamProp),
    Params = #{table_name => TableName, drop_interval => DropInterval},
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).

init(Params) -> 
    TableName = maps:get(table_name, Params),
    DropInterval = maps:get(drop_interval, Params),
    cache_ets:create(TableName),
    cache_db:create(),
    cache_db:start(),
    timer:apply_interval(DropInterval*1000, cache_ets, delete_obsolete, [TableName]),
    {ok, #state{}}.

handle_call({insert, TableName, Key, Value, TTL}, _From, Tab) ->
    Reply = cache_ets:insert(TableName, Key, Value, TTL),
    {reply, Reply, Tab};
handle_call({insert_with_db, TableName, Key, Value, TTL}, _From, Tab) ->
    Reply = [
        {<<"ets">>,cache_ets:insert(TableName, Key, Value, TTL)},
        {<<"db">>,cache_db:insert(Key, Value)}
    ],
    {reply, Reply, Tab};
handle_call({lookup, TableName, Key}, _From, Tab) ->
    Reply = cache_ets:lookup(TableName, Key),
    {reply, Reply, Tab};
handle_call({lookup_by_date, TableName, DateFrom, DateTo}, _From, Tab) ->
    FromInSeconds = calendar:datetime_to_gregorian_seconds(DateFrom),
    ToInSeconds = calendar:datetime_to_gregorian_seconds(DateTo),
    Reply = cache_ets:lookup_by_date(TableName, FromInSeconds, ToInSeconds),
    {reply, Reply, Tab};        
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.