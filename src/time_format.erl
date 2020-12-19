-module(time_format).

-include_lib("eunit/include/eunit.hrl").

-export([
        convert_date_and_time_to_tuple/1,
        current_time/0
    ]).

convert_date_and_time_to_tuple(BinaryDateTime) ->
    [Date, Time] = binary:split(BinaryDateTime, <<" ">>),
    [Year, Month, Day] = binary:split(Date, <<"/">>, [global]),
    [Hours, Minutes, Seconds] = binary:split(Time, <<":">>, [global]),
    {
        {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
        {binary_to_integer(Hours), binary_to_integer(Minutes), binary_to_integer(Seconds)}
    }.

current_time() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%%%----------------------------------------------------------------------
%% Test
%%%----------------------------------------------------------------------
-ifdef(TEST).
    convert_date_and_time_to_tuple_test_() -> [
        ?_assert(convert_date_and_time_to_tuple(<<"2015/1/10 23:59:59">>) =:= {{2015,1,10},{23,59,59}})
    ].
-endif.