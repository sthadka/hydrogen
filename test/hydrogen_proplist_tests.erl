-module(hydrogen_proplist_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hydrogen.hrl").

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
    ?assertEqual([{k, v} | proplist()], ?PLIST_INSERT(proplist(), {k, v})).

append() ->
    ?assertEqual([{a, b}, {c, d}, {e, f}, {k, v}],
                 ?PLIST_APPEND(proplist(), {k, v})).

update() ->
    ?assertEqual([{a, b}, {c, x}, {e, f}], ?PLIST_UPDATE(proplist(), {c, x})).

add_update() ->
    ?assertEqual([{a, b}, {c, x}, {e, f}], ?PLIST_ADD_UPDATE(proplist(), {c, x})),
    ?assertEqual([{a, b}, {c, d}, {e, f}, {k, v}],
                 ?PLIST_ADD_UPDATE(proplist(), {k, v})).

delete() ->
    ?assertEqual([{a, b}, {e, f}], ?PLIST_DELETE(proplist(), c)).

get_test() ->
    ?assertEqual(f, ?PLIST_GET(proplist(), e)),
    ?assertEqual(default, ?PLIST_GET(proplist(), k, default)).

get_all() ->
    ?assertEqual([b, v], ?PLIST_GET_ALL(?PLIST_INSERT(proplist(), {a, v}), a)).

sort() ->
    ?assertEqual(proplist(), ?PLIST_SORT([{c, d}, {e, f}, {a, b}])).

apply() ->
    Fs = [fun (L) -> [{K+1, V} || {K, V} <- L] end],
    ?assertEqual([{1, 1}, {2, 2}, {3, 3}],
                 ?PLIST_APPLY([{0, 1}, {1, 2}, {2, 3}], Fs)).

keys() ->
    ?assertEqual([a, c, e], ?PLIST_KEYS(proplist())).

member() ->
    ?assert(?PLIST_MEMBER(proplist(), a)),
    ?assertNot(?PLIST_MEMBER(proplist(), x)).

proplist() ->
    [{a, b}, {c, d}, {e, f}].
