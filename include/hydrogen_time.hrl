%% Time and calendar functions
-define(NOW(),            hydrogen_time:now()).
-define(ISO8601_TO_TS(B), hydrogen_time:iso8601_to_ts(B)).
-define(TO_DATETIME(TS),  hydrogen_time:to_datetime(TS)).
-define(TO_DATE(TS),      hydrogen_time:to_date(TS)).
-define(TO_TIMESTAMP(D),  hydrogen_time:to_timestamp(D)).
