-module(cache_db).

-export([
    start/1,
    create/1,
    insert/2,
    lookup/2,
    lookup_by_date/3,
    delete_item/2
]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(db_cache, {key, value, creation_time}).

create(TableNameDB) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(TableNameDB,   [{attributes, record_info(fields, db_cache)},{disc_only_copies, [node()]}]),
    mnesia:stop().

start(TableNameDB) ->
    mnesia:start(),
    mnesia:wait_for_tables([TableNameDB], 20000).

insert(Key, Value) ->
    Row = #db_cache{key=Key, value=Value, creation_time=time_format:current_time()},
    WriteFun = fun() ->
        mnesia:write(Row)
        end,
    {atomic,Status} = mnesia:transaction(WriteFun),
    Status.

lookup(TableNameDB, Key) ->
    F = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(TableNameDB), 
        X#db_cache.key =:= Key])) 
    end,
    {atomic,Result} = mnesia:transaction(F),
    case Result of
        [{_Table, _Key, Value, _Time}] ->
            {ok,Value};
        _ -> 
            {ok,[]}
    end.

lookup_by_date(TableNameDB, From, To) ->
    F = fun() -> qlc:e(qlc:q([{X#db_cache.key,X#db_cache.value} || X <- mnesia:table(TableNameDB), 
        X#db_cache.creation_time >= From,
        X#db_cache.creation_time =< To])) 
    end,
    {atomic, ResultList} = mnesia:transaction(F),
    ResultList.

delete_item(TableNameDB, Key) ->
    Oid = {TableNameDB, Key},
    F = fun() ->
        mnesia:delete(Oid)
    end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic,ok} ->
            ok;
        _ ->
            false
    end.

%%%----------------------------------------------------------------------
%% Test
%%%----------------------------------------------------------------------
-ifdef(TEST).
    create_test_() -> [
        ?_assert(cache_db:create() =:= stopped)
    ].

    start_test_() -> [
        ?_assert(cache_db:start() =:= ok)
    ].

    insert_test_() -> [
        ?_assert(cache_db:insert(test_key, some_val) =:= ok),
        ?_assert(cache_db:insert(test_key2, some_val2) =:= ok) 
    ].

    lookup_test_() -> [
        ?_assert(cache_db:lookup(test_key) =:= {ok,some_val})
    ].
    
    lookup_by_date_test_() -> [
        ?_assert(cache_db:lookup_by_date(63775531972, 65984520571) =:= [{test_key,some_val},{test_key2,some_val2}])
    ].

    remove_item_test_() -> [
        ?_assert(cache_db:delete_item(test_key) =:= ok)
    ].
-endif.