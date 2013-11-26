%% Hash table functions
-define(ht_new(),               hydrogen_ht:new()).
-define(ht_new(Ops),            hydrogen_ht:new(Ops)).
-define(ht_put(T, K, V),        hydrogen_ht:put(T, K, V)).
-define(ht_del(T),              hydrogen_ht:del(T)).
-define(ht_del(T, K),           hydrogen_ht:del(T, K)).
-define(ht_get(T, K),           hydrogen_ht:get(T, K)).
-define(ht_reset(T),            hydrogen_ht:reset(T)).
-define(ht_to_list(T),          hydrogen_ht:to_list(T)).
