%%%-------------------------------------------------------------------
%%% @doc hydrogen_proplist
%%%
%%% Wrapper for proplists
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_proplist).

-export([new/0,
        insert/2, append/2, update/2, add_update/2, delete/2, delete_all/2,
        get/2, get/3, get_all/2,
        sort/1, apply/2,
        keys/1, member/2]).

-spec new() -> list().
new() ->
    [].

-spec insert(list(), {any(), any()}) -> list().
insert(List, {Key, Value}) ->
    [{Key, Value} | List].

-spec append(list(), {any(), any()}) -> list().
append(List, {Key, Value}) ->
    List ++ [{Key, Value}].

-spec update(list(), {any(), any()}) -> list().
update(List, {Key, Value}) ->
    lists:keyreplace(Key, 1, List, {Key, Value}).

-spec add_update(list(), {any(), any()}) -> list().
add_update(List, {Key, Value}) ->
    lists:keystore(Key, 1, List, {Key, Value}).

-spec delete(list(), any()) -> list().
delete(List, Key) ->
    lists:keydelete(Key, 1, List).

-spec delete_all(list(), any()) -> list().
delete_all(List, Key) ->
    proplists:delete(Key, List).

-spec get(list(), any()) -> any().
get(List, Key) ->
    proplists:get_value(Key, List).

-spec get(list(), any(), any()) -> any().
get(List, Key, Default) ->
    proplists:get_value(Key, List, Default).

-spec get_all(list(), any()) -> list().
get_all(List, Key) ->
    proplists:get_all_values(Key, List).

-spec sort(list()) -> list().
sort(List) ->
    lists:keysort(1, List).

-spec apply(list(), list()) -> list().
apply(List, Fs) ->
    lists:foldl(fun (F, L) -> F(L) end, List, Fs).

-spec keys(list()) -> list().
keys(List) ->
    [Key || {Key, _Value} <- List].

-spec member(list(), any()) -> list().
member(List, Key) ->
    lists:keymember(Key, 1, List).
