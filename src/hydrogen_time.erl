%%%-------------------------------------------------------------------
%%% @doc hydrogen time
%%%
%%% Time and calendar library
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_time).

-include_lib("hydrogen.hrl").

-export([now_micro/0, now_milli/0, now/0,
         to_datetime/1, to_date/1, to_timestamp/1,
         iso8601_to_ts/1]).

-define(GREGORIAN_SECONDS_TO_UNIXTIMESTAMP_DIFFERENCE, 719528*24*3600).

-spec now_micro() -> non_neg_integer().
now_micro() ->
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    (MegaSeconds * 1000000 + Seconds) * 1000000 + MicroSeconds.

-spec now_milli() -> non_neg_integer().
now_milli() ->
    now_micro() div 1000.

-spec now() -> non_neg_integer().
now() ->
    now_micro() div 1000000.

-spec to_datetime(non_neg_integer()) -> calendar:datetime().
to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + 719528*24*3600).

-spec to_date(non_neg_integer()) -> calendar:date().
to_date(Timestamp) ->
    {{Y,M,D},_} = to_datetime(Timestamp),
    {Y,M,D}.

-spec to_timestamp(calendar:date() | calendar:datetime()) -> non_neg_integer().
to_timestamp({Year,Month,Day}) ->
    to_timestamp({{Year,Month,Day}, {0,0,0}});

to_timestamp(Datetime = {{_,_,_}, {_,_,_}}) ->
    calendar:datetime_to_gregorian_seconds(Datetime) -
    ?GREGORIAN_SECONDS_TO_UNIXTIMESTAMP_DIFFERENCE.


%% Converts ISO 8601 format to timestamp
%% Ex: 2013-07-15T09:45:49+0000 to 1373881549
-spec iso8601_to_ts(binary() | string()) -> non_neg_integer().
iso8601_to_ts(DateString) when is_binary(DateString) ->
    iso8601_to_ts(?TO_L(DateString));

iso8601_to_ts(DateString) when is_list(DateString) ->
    gregorian_seconds_to_timestamp(
        ymdhms_to_gregorian_seconds(
            parse_iso8601(DateString))).


%%-------------------------------------------------------------------
%% Internal helpers
%%-------------------------------------------------------------------
parse_iso8601(DateString) ->
    {ok,[Year, Month, Day, Hour, Min, Sec],_} =
    io_lib:fread("~d-~d-~dT~d:~d:~d+0000", DateString),
    {{Year,Month,Day},{Hour,Min,Sec}}.

ymdhms_to_gregorian_seconds({{Year,Month,Day},{Hour,Min,Sec}}) ->
    calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{Hour,Min,Sec}}).

gregorian_seconds_to_timestamp(Seconds) ->
    Seconds - ?GREGORIAN_SECONDS_TO_UNIXTIMESTAMP_DIFFERENCE.
