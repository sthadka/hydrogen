%% Time and calendar functions
-define(NOW(),            hydrogen_time:now()).
-define(NOW_MILLI(),      hydrogen_time:now_milli()).
-define(NOW_MICRO(),      hydrogen_time:now_micro()).
-define(ISO8601_TO_TS(B), hydrogen_time:iso8601_to_ts(B)).
-define(TO_DATETIME(TS),  hydrogen_time:to_datetime(TS)).
-define(TO_DATE(TS),      hydrogen_time:to_date(TS)).
-define(TO_TIMESTAMP(D),  hydrogen_time:to_timestamp(D)).

%% Helper functions for time conversion into milliseconds
-define(SECONDS(S),       hydrogen_time:seconds(S)).
-define(MINUTES(M),       hydrogen_time:minutes(M)).
-define(HOURS(H),         hydrogen_time:hours(H)).
-define(DAYS(D),          hydrogen_time:days(D)).
