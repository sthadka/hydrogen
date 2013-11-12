%%%-------------------------------------------------------------------
%%% @doc hydrogen_convert
%%%
%%% Data type conversion library.
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_convert).

-include_lib("hydrogen.hrl").

-export([to_atom/1, to_atom/2,
         to_existing_atom/1, to_existing_atom/2,
         to_integer/1,
         to_binary/1,
         to_list/1,
         to_float/1
        ]).

-spec to_atom(atom() | list() | binary()) -> atom().
to_atom(A) when is_atom(A) ->
    A;
to_atom(L) when is_list(L) ->
    ?L2A(L);
to_atom(B) when is_binary(B) ->
    to_atom(B, utf8).

-spec to_atom(atom() | nonempty_string() | binary(), atom()) -> atom().
to_atom(B, Encoding) when is_binary(B) ->
    ?B2A(B, Encoding);
to_atom(Other, _Encoding) ->
    to_atom(Other).

-spec to_existing_atom(atom() | nonempty_string() | binary()) ->
    atom() | no_return().
to_existing_atom(A) when is_atom(A) ->
    A;
to_existing_atom(L) when is_list(L) ->
    to_existing_atom(?L2B(L));
to_existing_atom(B) when is_binary(B) ->
    to_existing_atom(B, utf8).

-spec to_existing_atom(atom() | list() | binary(), atom()) -> atom().
to_existing_atom(B, Encoding) when is_binary(B) ->
    case catch binary_to_existing_atom(B, Encoding) of
        {'EXIT', _} ->
            throw({hydrogen, convert, no_such_atom, [{binary, B}]});
        Atom ->
            Atom
    end;
to_existing_atom(Other, _Encoding) ->
    to_existing_atom(Other).

-spec to_integer(integer() | binary() | list()) -> integer().
to_integer(I) when is_integer(I) ->
    I;
to_integer(B) when is_binary(B) ->
    to_integer(?B2L(B));
to_integer(L) when is_list(L) ->
    case catch ?L2I(L) of
        {'EXIT', _} ->
            throw({hydrogen, convert, not_a_valid_integer, [{list, L}]});
        Int ->
            Int
    end.

-spec to_binary(binary() | list() | atom() | integer() | float()) -> binary().
to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    ?L2B(L);
to_binary(A) when is_atom(A) ->
    ?A2B(A);
to_binary(I) when is_integer(I) ->
    ?I2B(I);
to_binary(F) when is_float(F) ->
    ?F2B(F).

-spec to_list(list() | binary() | atom() | integer()) -> list().
to_list(L) when is_list(L)    -> L;
to_list(B) when is_binary(B)  -> ?B2L(B);
to_list(A) when is_atom(A)    -> ?A2L(A);
to_list(I) when is_integer(I) -> ?I2L(I);
to_list(F) when is_float(F)   -> ?B2L(?F2B(F)).

-spec to_float(float() | binary() | integer() | [integer()] ) -> float().
to_float(F) when is_float(F) ->
    F;
to_float(B) when is_binary(B) ->
    to_float(?B2L(B));
to_float(I) when is_integer(I) ->
    float(I);
to_float(L) when is_list(L) ->
    case catch ?L2F(L) of
        {'EXIT', _} ->
            case catch ?L2I(L) of
                {'EXIT', _} ->
                    throw({hydrogen, convert, not_a_valid_float,
                           [{list, L}]});
                Int ->
                    to_float(Int)
            end;
        Float ->
            Float
    end.
