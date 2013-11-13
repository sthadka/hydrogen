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
    ?assertEqual([{k, v} | proplist()], ?plist_insert(proplist(), {k, v})).

append() ->
    ?assertEqual([{a, b}, {c, d}, {e, f}, {k, v}],
                 ?plist_append(proplist(), {k, v})).

update() ->
    ?assertEqual([{a, b}, {c, x}, {e, f}], ?plist_update(proplist(), {c, x})).

add_update() ->
    ?assertEqual([{a, b}, {c, x}, {e, f}], ?plist_add_update(proplist(), {c, x})),
    ?assertEqual([{a, b}, {c, d}, {e, f}, {k, v}],
                 ?plist_add_update(proplist(), {k, v})).

delete() ->
    ?assertEqual([{a, b}, {e, f}], ?plist_delete(proplist(), c)).

get_test() ->
    ?assertEqual(f, ?plist_get(proplist(), e)),
    ?assertEqual(default, ?plist_get(proplist(), k, default)).

get_all() ->
    ?assertEqual([b, v], ?plist_get_all(?plist_insert(proplist(), {a, v}), a)).

sort() ->
    ?assertEqual(proplist(), ?plist_sort([{c, d}, {e, f}, {a, b}])).

apply() ->
    Fs = [fun (L) -> [{K+1, V} || {K, V} <- L] end],
    ?assertEqual([{1, 1}, {2, 2}, {3, 3}],
                 ?plist_apply([{0, 1}, {1, 2}, {2, 3}], Fs)).

keys() ->
    ?assertEqual([a, c, e], ?plist_keys(proplist())).

member() ->
    ?assert(?plist_member(proplist(), a)),
    ?assertNot(?plist_member(proplist(), x)).











proplist() ->
    [{a, b}, {c, d}, {e, f}].
