%%%-------------------------------------------------------------------
%%% @doc hydrogen hash table
%%%
%%% Simple hash table with configurable storage
%%%
%%% Note: The behaviours of some functions may not be idempotent based on used
%%% hash table type, e.g. ets
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_ht).

%% TODO: Maybe have ets as default and have rest storage types as options

-export([new/1, new/2,
         reset/1,
         to_list/1,
         get/2,
         set/3,
         del/2]).

-record(hydrogen_ht, {type :: ht_type(),
                      table :: table()}).

-type ht_type() :: ets | proplist | dict | tree.

-type tid() :: pos_integer().
-type ets() :: tid() | atom().
-type proplist() :: [tuple()].
-type tree() :: gb_trees:tree().
-type table() :: proplist() | tree() | ets() | dict:dict().

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
-spec new(ht_type()) -> #hydrogen_ht{}.
new(proplist = Type) ->
    Table = hydrogen_proplist:new(),
    #hydrogen_ht{type = Type, table = Table};
new(dict = Type) ->
    Table = dict:new(),
    #hydrogen_ht{type = Type, table = Table};
new(tree = Type) ->
    Table = gb_trees:empty(),
    #hydrogen_ht{type = Type, table = Table};
new(ets = Type) ->
    Table = ets:new(?MODULE, []),
    #hydrogen_ht{type = Type, table = Table}.

-spec new(ht_type(), list()) -> #hydrogen_ht{}.
new(proplist = Type, []) ->
    new(Type);
new(dict = Type, []) ->
    new(Type);
new(tree = Type, []) ->
    new(Type);
new(ets = Type, Options) ->
    Table = ets:new(hd(Options), tl(Options)),
    #hydrogen_ht{type = Type, table = Table}.

-spec set(#hydrogen_ht{}, Key::term(), Val::term()) -> #hydrogen_ht{}.
set(#hydrogen_ht{type = proplist, table = PList} = HT, Key, Val) ->
    NewPList = hydrogen_proplist:add_update(PList, {Key, Val}),
    HT#hydrogen_ht{table = NewPList};
set(#hydrogen_ht{type = dict, table = Dict} = HT, Key, Val) ->
    NewDict = dict:store(Key, Val, Dict),
    HT#hydrogen_ht{table = NewDict};
set(#hydrogen_ht{type = tree, table = Tree} = HT, Key, Val) ->
    NewTree = gb_trees:enter(Key, Val, Tree),
    HT#hydrogen_ht{table = NewTree};
set(#hydrogen_ht{type = ets, table = Ets} = HT, Key, Val) ->
    ets:insert(Ets, {Key, Val}),
    HT.

-spec get(#hydrogen_ht{}, Key::term()) -> Val::term() | error.
get(#hydrogen_ht{type = proplist, table = PList}, Key) ->
    hydrogen_proplist:get(PList, Key);
get(#hydrogen_ht{type = dict, table = Dict}, Key) ->
    case dict:is_key(Key, Dict) of
        true ->
            dict:fetch(Key, Dict);
        false ->
            undefined
    end;
get(#hydrogen_ht{type = tree, table = Tree}, Key) ->
    case gb_trees:is_defined(Key, Tree) of
        true ->
            gb_trees:get(Key, Tree);
        false ->
            undefined
    end;
get(#hydrogen_ht{type = ets, table = Ets}, Key) ->
    case ets:lookup(Ets, Key) of
        [] ->
            undefined;
        [{Key, Val}] ->
            Val
    end.

-spec del(#hydrogen_ht{}, Key::term()) -> #hydrogen_ht{}.
del(#hydrogen_ht{type = proplist, table = PList} = HT, Key) ->
    NewPList = hydrogen_proplist:delete(PList, Key),
    HT#hydrogen_ht{table = NewPList};
del(#hydrogen_ht{type = dict, table = Dict} = HT, Key) ->
    NewDict = dict:erase(Key, Dict),
    HT#hydrogen_ht{table = NewDict};
del(#hydrogen_ht{type = tree, table = Tree} = HT, Key) ->
    NewTree = gb_trees:delete_any(Key, Tree),
    HT#hydrogen_ht{table = NewTree};
del(#hydrogen_ht{type = ets, table = Ets}, Key) ->
    ets:delete(Ets, Key).

-spec to_list(#hydrogen_ht{}) -> [{term(), term()}].
to_list(#hydrogen_ht{type = proplist, table = PList}) ->
    PList;
to_list(#hydrogen_ht{type = dict, table = Dict}) ->
    dict:to_list(Dict);
to_list(#hydrogen_ht{type = tree, table = Tree}) ->
    gb_trees:to_list(Tree);
to_list(#hydrogen_ht{type = ets, table = Ets}) ->
    ets:tab2list(Ets).

-spec reset(#hydrogen_ht{}) -> #hydrogen_ht{}.
reset(#hydrogen_ht{type = ets, table = Ets} = HT) ->
    ets:delete_all_objects(Ets),
    HT;
reset(#hydrogen_ht{type = Type}) ->
    new(Type).
