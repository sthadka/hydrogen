%% Hash table functions
-define(HT_NEW(),               hydrogen_ht:new()).
-define(HT_NEW(Ops),            hydrogen_ht:new(Ops)).
-define(HT_PUT(T, K, V),        hydrogen_ht:put(T, K, V)).
-define(HT_DEL(T),              hydrogen_ht:del(T)).
-define(HT_DEL(T, K),           hydrogen_ht:del(T, K)).
-define(HT_GET(T, K),           hydrogen_ht:get(T, K)).
-define(HT_RESET(T),            hydrogen_ht:reset(T)).
-define(HT_TO_LIST(T),          hydrogen_ht:to_list(T)).
