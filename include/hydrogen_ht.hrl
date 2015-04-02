%% Hash table functions
-define(HT_NEW(Ops),       hydrogen_ht:new(Ops)).
-define(HT_NEW(Type, Ops), hydrogen_ht:new(Type, Ops)).
-define(HT_SET(T, K, V),   hydrogen_ht:set(T, K, V)).
-define(HT_DEL(T, K),      hydrogen_ht:del(T, K)).
-define(HT_GET(T, K),      hydrogen_ht:get(T, K)).
-define(HT_RESET(T),       hydrogen_ht:reset(T)).
-define(HT_TO_LIST(T),     hydrogen_ht:to_list(T)).
