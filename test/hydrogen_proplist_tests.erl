-module(hydrogen_proplist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hydrogen.hrl").

hydrogen_proplist_test_() ->
    [?_test(insert()),
     ?_test(append()),
     ?_test(update()),
     ?_test(add_update()),
     ?_test(delete()),
     ?_test(get_test()),
     ?_test(get_all()),
     ?_test(sort()),
     ?_test(apply()),
     ?_test(keys()),
     ?_test(member())].

insert() ->
    ?assertEqual([{k, v} | proplist()], ?kv_insert(proplist(), {k, v})).

append() ->
    ?assertEqual([{a, b}, {c, d}, {e, f}, {k, v}],
                 ?kv_append(proplist(), {k, v})).

update() ->
    ?assertEqual([{a, b}, {c, x}, {e, f}], ?kv_update(proplist(), {c, x})).

add_update() ->
    ?assertEqual([{a, b}, {c, x}, {e, f}], ?kv_add_update(proplist(), {c, x})),
    ?assertEqual([{a, b}, {c, d}, {e, f}, {k, v}],
                 ?kv_add_update(proplist(), {k, v})).

delete() ->
    ?assertEqual([{a, b}, {e, f}], ?kv_delete(proplist(), c)).

get_test() ->
    ?assertEqual(f, ?kv_get(proplist(), e)),
    ?assertEqual(default, ?kv_get(proplist(), k, default)).

get_all() ->
    ?assertEqual([v, b], ?kv_get_all(?kv_insert(proplist(), {a, v}), a)).

sort() ->
    ?assertEqual(proplist(), ?kv_sort([{c, d}, {e, f}, {a, b}])).

apply() ->
    Fs = [fun (L) -> [{K+1, V} || {K, V} <- L] end],
    ?assertEqual([{1, 1}, {2, 2}, {3, 3}],
                 ?kv_apply([{0, 1}, {1, 2}, {2, 3}], Fs)).

keys() ->
    ?assertEqual([a, c, e], ?kv_keys(proplist())).

member() ->
    ?assert(?kv_member(proplist(), a)),
    ?assertNot(?kv_member(proplist(), x)).











proplist() ->
    [{a, b}, {c, d}, {e, f}].
