-module(cache_server_h).
-export([init/2]).

init(Req0=#{method := <<"POST">>}, State) ->
    {ok, Json, _R} = cowboy_req:read_body(Req0),
    DataFromJson = jsx:decode(Json,[return_maps]),
    ReqResult = jsx:encode(request_handler(DataFromJson)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, ReqResult, Req0),
    {ok, Req, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req, State}.

request_handler(#{<<"action">> := <<"insert">>,<<"key">> := Key, <<"value">> := Value} = ReqMap) ->
    case ReqMap of
        #{<<"ttl">> := TTL, <<"db">> := true} ->
            cache_server:insert_with_db(Key, Value, TTL);
        #{<<"ttl">> := TTL} ->
            cache_server:insert(Key, Value, TTL);
        #{<<"db">> := true} ->
            cache_server:insert_with_db(Key, Value);
        _ ->
            cache_server:insert(Key, Value)
    end,
    [{<<"result">>, <<"ok">>}];
request_handler(#{<<"action">> := <<"lookup">>, <<"key">> := Key}) ->
    {ok,Value} = cache_server:lookup(Key),
    [{<<"result">>,Value}];
request_handler(#{<<"action">> := <<"db_lookup">>, <<"key">> := Key}) ->
    {ok,Value} = cache_server:db_lookup(Key),
    ok = cache_server:insert(Key, Value),
    [{<<"result">>,Value}];
request_handler(#{<<"action">> := <<"lookup_by_date">>, <<"date_from">> := From, <<"date_to">> := To}) ->
    DateFrom = time_format:convert_date_and_time_to_tuple(From),
    DateTo = time_format:convert_date_and_time_to_tuple(To),
    Result = cache_server:lookup_by_date(DateFrom, DateTo),
    [{<<"result">>,Result}];
request_handler(#{<<"action">> := <<"db_lookup_by_date">>, <<"date_from">> := From, <<"date_to">> := To}) ->
    DateFrom = time_format:convert_date_and_time_to_tuple(From),
    DateTo = time_format:convert_date_and_time_to_tuple(To),
    PropList = cache_server:db_lookup_by_date(DateFrom, DateTo),
    ok = cache_server:insert_from_list(PropList),
    [{<<"result">>,PropList}];
request_handler(#{<<"action">> := <<"db_delete_item">>, <<"key">> := Key}) ->
    Result = cache_server:db_delete_item(Key),
    [{<<"result">>,Result}];
request_handler(_Map) ->
    [{<<"result">>,<<"bad request">>}].