-module(cache_db).

-export([
    start/0,
    create/0,
    insert/2,
    lookup/1,
    lookup_by_date/2,
    remove_item/1
]).

-include("../include/server_conf.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(?DB_TABLE, {key, value, creation_time}).

create() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(?DB_TABLE,   [{attributes, record_info(fields, ?DB_TABLE)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([?DB_TABLE], 20000).

insert(Key,Value) ->
    Row = #?DB_TABLE{key=Key, value=Value, creation_time=time_format:current_time()},
    WriteFun = fun() ->
        mnesia:write(Row)
        end,
    {atomic,Status} = mnesia:transaction(WriteFun),
    Status.

lookup(Key) ->
    F = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(?DB_TABLE), 
        X#?DB_TABLE.key =:= Key])) 
    end,
    {atomic,[{_Table, Key, Value, _Time}]} = mnesia:transaction(F),
    Value.

lookup_by_date(From,To) ->
    F = fun() -> qlc:e(qlc:q([{X#?DB_TABLE.key,X#?DB_TABLE.value} || X <- mnesia:table(?DB_TABLE), 
        X#?DB_TABLE.creation_time >= From,
        X#?DB_TABLE.creation_time =< To])) 
    end,
    {atomic, ResultList} = mnesia:transaction(F),
    ResultList.

remove_item(Key) ->
    Oid = {?DB_TABLE, Key},
    F = fun() ->
        mnesia:delete(Oid)
    end,
    mnesia:transaction(F).

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
        ?_assert(cache_db:lookup(test_key) =:= some_val)
    ].
    
    lookup_by_date_test_() -> [
        ?_assert(cache_db:lookup_by_date(63775531972, 65984520571) =:= [{test_key,some_val},{test_key2,some_val2}])
    ].

    remove_item_test_() -> [
        ?_assert(cache_db:remove_item(test_key) =:= {atomic,ok})
    ].
-endif.