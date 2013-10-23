-module(hydrogen_convert_tests).
-include_lib("eunit/include/eunit.hrl").
-include("hydrogen.hrl").

hydrogen_convert_test_() ->
    [?_test(to_atom()),
     ?_test(to_existing_atom()),
     ?_test(to_integer()),
     ?_test(to_binary()),
     ?_test(to_list()),
     ?_test(to_float())
    ].

to_atom() ->
    ?assert(is_atom(?TO_A(test_atom))),
    ?assert(is_atom(?TO_A("test_atom"))),
    ?assert(is_atom(?TO_A(<<"test_atom">>))),
    ?assert(is_atom(hydrogen_convert:to_atom(<<"test_atom">>, latin1))).

to_existing_atom() ->
    ?assert(is_atom(?TO_E_A(test_atom))),
    ?assert(is_atom(?TO_E_A("test_atom"))),
    ?assert(is_atom(?TO_E_A(<<"test_atom">>))),

    ?assert(is_atom(hydrogen_convert:to_existing_atom(
                <<"test_atom">>, latin1))).

to_integer() ->
    ?assert(is_integer(?TO_I(42))),
    ?assert(is_integer(?TO_I(<<"42">>))),
    ?assert(is_integer(?TO_I("42"))),
    ?assertThrow({hydrogen, convert, not_a_valid_integer, _}, ?TO_I("a")).

to_binary() ->
    ?assert(is_binary(?TO_B(<<"binary">>))),
    ?assert(is_binary(?TO_B("binary"))),
    ?assert(is_binary(?TO_B(binary))),
    ?assert(is_binary(?TO_B(42))),
    ?assert(is_binary(?TO_B(4.2))).

to_list() ->
    ?assert(is_list(?TO_L("list"))),
    ?assert(is_list(?TO_L(<<"list">>))),
    ?assert(is_list(?TO_L("list"))),
    ?assert(is_list(?TO_L(42))),
    ?assert(is_list(?TO_L(4.2))).

to_float() ->
    ?assert(is_float(?TO_F(4.2))),
    ?assert(is_float(?TO_F(<<"4.2">>))),
    ?assert(is_float(?TO_F(42))),
    ?assert(is_float(?TO_F("4.2"))).
