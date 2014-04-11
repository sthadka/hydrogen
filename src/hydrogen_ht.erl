%%%-------------------------------------------------------------------
%%% @doc hydrogen hash table
%%%
%%% Simple hash table which uses ETS for storage
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_ht).

-export([new/0, new/1, del/1, reset/1, to_list/1,
         get/2, put/3, del/2]).

-record(hydrogen_ht, {name}).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec new() -> #hydrogen_ht{}.
new() ->
    #hydrogen_ht{name=ets:new(ht, [])}.

-spec new(list()) -> #hydrogen_ht{}.
new(Options) ->
    #hydrogen_ht{name=ets:new(hd(Options), tl(Options))}.

-spec del(#hydrogen_ht{}) -> true.
del(#hydrogen_ht{name=Table}) ->
    ets:delete(Table).

-spec put(#hydrogen_ht{}, Key::term(), Val::term()) -> ok.
put(#hydrogen_ht{name=Table}=TableData, Key, Value) ->
    ets:insert(Table, {Key, Value}),
    TableData.

-spec get(#hydrogen_ht{}, Key::term()) -> Val::term() | error.
get(#hydrogen_ht{name=Table}, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            undefined;
        [{Key, Value}] ->
            Value
    end.

-spec del(#hydrogen_ht{}, Key::term()) -> ok.
del(#hydrogen_ht{name=Table}=TableData, Key) ->
    ets:delete(Table, Key),
    TableData.

-spec to_list(#hydrogen_ht{}) -> [{term(), term()}].
to_list(#hydrogen_ht{name=Table}) ->
    ets:tab2list(Table).

% TODO: Check if dropping the table and creating a new table it is faster
-spec reset(#hydrogen_ht{}) -> true.
reset(#hydrogen_ht{name=Table}=TableData) ->
    ets:delete_all_objects(Table),
    TableData.
