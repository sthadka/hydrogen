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

-type key_val() :: {any(), any()}.
-type proplist() :: [key_val()].

-spec new() -> proplist().
new() ->
    [].

-spec insert(proplist(), key_val()) -> proplist().
insert(List, {Key, Value}) ->
    [{Key, Value} | List].

-spec append(proplist(), key_val()) -> proplist().
append(List, {Key, Value}) ->
    lists:reverse([{Key, Value} | lists:reverse(List)]).

-spec update(proplist(), key_val()) -> proplist().
update(List, {Key, Value}) ->
    lists:keyreplace(Key, 1, List, {Key, Value}).

-spec add_update(proplist(), key_val()) -> proplist().
add_update(List, {Key, Value}) ->
    lists:keystore(Key, 1, List, {Key, Value}).

-spec delete(proplist(), any()) -> proplist().
delete(List, Key) ->
    lists:keydelete(Key, 1, List).

-spec delete_all(proplist(), any()) -> proplist().
delete_all(List, Key) ->
    proplists:delete(Key, List).

-spec get(proplist(), any()) -> any().
get(List, Key) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false        -> undefined
    end.

-spec get(proplist(), any(), any()) -> any().
get(List, Key, Default) ->
    proplists:get_value(Key, List, Default).

-spec get_all(proplist(), any()) -> list().
get_all(List, Key) ->
    get_all_values(List, Key).

-spec sort(proplist()) -> proplist().
sort(List) ->
    lists:keysort(1, List).

-spec apply(proplist(), [fun((proplist()) -> proplist())]) -> proplist().
apply(List, Fs) ->
    lists:foldl(fun (F, L) -> F(L) end, List, Fs).

-spec keys(proplist()) -> list().
keys(List) ->
    [Key || {Key, _Value} <- List].

-spec member(proplist(), any()) -> boolean().
member(List, Key) ->
    lists:keymember(Key, 1, List).


%%-------------------------------------------------------------------
%% Internal helpers
%%-------------------------------------------------------------------

get_all_values(List, Key) ->
    get_all_values(List, Key, []).

get_all_values([], _Key, Acc) ->
    Acc;
get_all_values([{Key, Value} | T], Key, Acc) ->
    get_all_values(T, Key, [Value | Acc]);
get_all_values([_H | T], Key, Acc) ->
    get_all_values(T, Key, Acc).
