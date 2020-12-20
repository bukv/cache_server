-module(cache_ets).

-export([
    create/1, 
    insert/4,
    insert_from_list/3, 
    lookup/2, 
    lookup_by_date/3, 
    delete_obsolete/1
    ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(cache_record, {key, value, creation_time, ttl}).

create(TableName) ->
    ets:new(TableName, [public, named_table, {keypos, 2}]),
    ok.

insert(TableName, Key, Value, TTL) ->
    CurrentTime = time_format:current_time(),
    ets:insert(TableName, #cache_record{key=Key, value=Value, creation_time=CurrentTime, ttl=TTL}),
    ok.


insert_from_list(TableName, [{Key,Value}|T], TTL) ->
    cache_ets:insert(TableName, Key, Value, TTL),
    insert_from_list(TableName, T, TTL);
insert_from_list(_TableName, [], _TTL) ->
    ok.

lookup(TableName, Key) ->
    DataFromTable = ets:lookup(TableName, Key),
    CurrentTime = time_format:current_time(),
    case DataFromTable of
        [#cache_record{value=Val, creation_time=CreationTime, ttl=TTL}] when CreationTime+TTL >= CurrentTime ->
            {ok,Val};
        _ ->
            {}
    end.

lookup_by_date(TableName, DateFrom, DateTo) ->
    MatchSpec = ets:fun2ms(fun(Record) when DateFrom =< Record#cache_record.creation_time, DateTo >= Record#cache_record.creation_time ->
        [{<<"key">>,Record#cache_record.key},{<<"value">>,Record#cache_record.value}]
    end),
    ets:select(TableName, MatchSpec).

delete_obsolete(TableName) ->
    CurrentTime = time_format:current_time(),
    MatchSpec = ets:fun2ms(fun(Record) when CurrentTime > Record#cache_record.creation_time + Record#cache_record.ttl->
        true
    end),
    ets:select_delete(TableName, MatchSpec),
    ok.

%%%----------------------------------------------------------------------
%% Test
%%%----------------------------------------------------------------------
-ifdef(TEST).
    create_test_() -> [
        ?_assert(cache_ets:create(test_table) =:= ok)
    ].

    insert_test_() -> [
        ?_assert(cache_ets:insert(test_table, key_test, val_test, 1) =:= ok),
        ?_assert(cache_ets:insert(test_table, key_test2, val_test2, 10) =:= ok)       
    ].

    insert_from_list_test_() -> [
            ?_assert(cache_ets:insert_from_list(test_table, [{<<"some_key_1">>,13},{<<"some_key_2">>,1}], 1) =:= ok)     
        ].

    lookup_test_() -> [
        ?_assert(cache_ets:lookup(test_table, key_test) =:= {ok,val_test}),
        ?_assert(
            receive
                 after (2000) -> cache_ets:lookup(test_table, key_test) =:= {} % Waiting 2 seconds to check obsolete data
            end  
        )
    ].

    lookup_by_date_test_() -> [
        ?_assert(cache_ets:lookup_by_date(test_table, {{2015,1,1},{00,00,00}}, {{2015,1,10},{23,59,59}}) =:= [])
    ].

    delete_obsolete_test_() -> [
        ?_assert(cache_ets:delete_obsolete(test_table) =:= ok),
        ?_assert(ets:prev(test_table, key_test2) =:= '$end_of_table')
    ].
-endif.