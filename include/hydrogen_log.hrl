%% Helper functions for logging
-ifdef(USE_ERROR_LOGGER).

-define(LOGGER, error_logger).

-define(DEBUG_MSG,     info_msg).
-define(INFO_MSG,      info_msg).
-define(NOTICE_MSG,    warning_msg).
-define(WARNING_MSG,   warning_msg).
-define(ERROR_MSG,     error_msg).
-define(CRITICAL_MSG,  error_msg).
-define(ALERT_MSG,     error_msg).
-define(EMERGENCY_MSG, error_msg).

-else.

-define(LOGGER, lager).

-define(DEBUG_MSG,     debug).
-define(INFO_MSG,      info).
-define(NOTICE_MSG,    notice).
-define(WARNING_MSG,   warning).
-define(ERROR_MSG,     error).
-define(CRITICAL_MSG,  critical).
-define(ALERT_MSG,     alert).
-define(EMERGENCY_MSG, emergency).

-endif. %% USE_ERROR_LOGGER

%% Log only the message
-define(LOG_DEBUG(M),           ?LOG_DEBUG(M, [])).
-define(LOG_INFO(M),            ?LOG_INFO(M, [])).
-define(LOG_NOTICE(M),          ?LOG_NOTICE(M, [])).
-define(LOG_WARNING(M),         ?LOG_WARNING(M, [])).
-define(LOG_ERROR(M),           ?LOG_ERROR(M, [])).
-define(LOG_CRITICAL(M),        ?LOG_CRITICAL(M, [])).
-define(LOG_ALERT(M),           ?LOG_ALERT(M, [])).
-define(LOG_EMERGENCY(M),       ?LOG_EMERGENCY(M, [])).

%% Log message with arguments
-define(LOG_DEBUG(M, L),        ?LOG(?LOGGER, ?DEBUG_MSG,     M, L)).
-define(LOG_INFO(M, L),         ?LOG(?LOGGER, ?INFO_MSG,      M, L)).
-define(LOG_NOTICE(M, L),       ?LOG(?LOGGER, ?NOTICE_MSG,    M, L)).
-define(LOG_WARNING(M, L),      ?LOG(?LOGGER, ?WARNING_MSG,   M, L)).
-define(LOG_ERROR(M, L),        ?LOG(?LOGGER, ?ERROR_MSG,     M, L)).
-define(LOG_CRITICAL(M, L),     ?LOG(?LOGGER, ?CRITICAL_MSG,  M, L)).
-define(LOG_ALERT(M, L),        ?LOG(?LOGGER, ?ALERT_MSG,     M, L)).
-define(LOG_EMERGENCY(M, L),    ?LOG(?LOGGER, ?EMERGENCY_MSG, M, L)).

%% Log message with prefix
-define(LOG_DEBUG(P, M, L),     ?LOG(?LOGGER, ?DEBUG_MSG,     "[~ts] " ++ M, [P | L])).
-define(LOG_INFO(P, M, L),      ?LOG(?LOGGER, ?INFO_MSG,      "[~ts] " ++ M, [P | L])).
-define(LOG_NOTICE(P, M, L),    ?LOG(?LOGGER, ?NOTICE_MSG,    "[~ts] " ++ M, [P | L])).
-define(LOG_WARNING(P, M, L),   ?LOG(?LOGGER, ?WARNING_MSG,   "[~ts] " ++ M, [P | L])).
-define(LOG_ERROR(P, M, L),     ?LOG(?LOGGER, ?ERROR_MSG,     "[~ts] " ++ M, [P | L])).
-define(LOG_CRITICAL(P, M, L),  ?LOG(?LOGGER, ?CRITICAL_MSG,  "[~ts] " ++ M, [P | L])).
-define(LOG_ALERT(P, M, L),     ?LOG(?LOGGER, ?ALERT_MSG,     "[~ts] " ++ M, [P | L])).
-define(LOG_EMERGENCY(P, M, L), ?LOG(?LOGGER, ?EMERGENCY_MSG, "[~ts] " ++ M, [P | L])).

-define(LOG(Logger, Severity, Msg, Args), Logger:Severity(Msg, Args)).
