%%%-------------------------------------------------------------------
%%% @doc hydrogen_web
%%%
%%% Web related utilities
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_web).

-include_lib("hydrogen.hrl").

-export([url_encode/1,
         base64_url_decode/1,
         base64_url_encode/1,
         ip_to_int/1]).

%%
%% Taken from https://github.com/tim/erlang-percent-encoding/blob/master/src/percent.erl
%% Percent encoding/decoding as defined by the application/x-www-form-urlencoded
%% content type (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1).
%%

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

-type encodable() :: binary() | atom() | [byte()] | integer().
-spec url_encode(encodable()) -> string().
url_encode(Str) when is_binary(Str) ->
    url_encode(binary_to_list(Str));
url_encode(Str) when is_list(Str) ->
    url_encode(lists:reverse(Str, []), []);
url_encode(Str) when is_integer(Str) ->
    url_encode(integer_to_list(Str));
url_encode(Str) when is_atom(Str) ->
    url_encode(atom_to_list(Str)).

-spec url_encode(list(), list()) -> list().
url_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $. ->
    url_encode(T, [X | Acc]);
url_encode([32 | T], Acc) ->
    url_encode(T, [$+ | Acc]);
url_encode([X | T], Acc) ->
    NewAcc = [$%, hexchr_encode(X bsr 4), hexchr_encode(X band 16#0f) | Acc],
    url_encode(T, NewAcc);
url_encode([], Acc) ->
    Acc.

-compile({inline, [{hexchr_encode, 1}]}).

hexchr_encode(N) when N >= 10 andalso N < 16 ->
    N + $A - 10;
hexchr_encode(N) when N >= 0 andalso N < 10 ->
    N + $0.

base64_url_decode(Content) when is_binary(Content) ->
    base64_url_decode(?B2L(Content));
base64_url_decode(Content) when is_list(Content) ->
    Base64Unpadded = [base64_url_decode_char(Char) || Char <- Content, Char =/= $\n],
    Base64Padded = base64_pad(Base64Unpadded),
    base64:decode(?L2B(Base64Padded)).

base64_url_encode(Content) when is_binary(Content) ->
    base64_url_encode(?B2L(Content));
base64_url_encode(Content) when is_list(Content) ->
    Base64 = base64:encode_to_string(Content),
    ?L2B([base64_url_encode_char(Char) || Char <- Base64, Char =/= $=]).

ip_to_int(Ip) when is_list(Ip) ->
    {ok, RE} = re:compile("(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)"),
    case re:run(Ip, RE, [{capture, all_but_first, list}]) of
        {match, [A, B, C, D]} ->
            (?L2I(A) bsl 24) bor (?L2I(B) bsl 16) bor (?L2I(C) bsl 8) bor ?L2I(D);
        _ ->
            0
    end;

ip_to_int(Ip) when is_binary(Ip) ->
    ip_to_int(?B2L(Ip));

ip_to_int(_) -> 0.

%%-------------------------------------------------------------------
%% Internal helpers
%%-------------------------------------------------------------------
base64_url_decode_char($-) ->
    $+;
base64_url_decode_char($_) ->
    $/;
base64_url_decode_char(Char) ->
    Char.


base64_pad(String) ->
    Length = length(String),

    Rem = Length rem 4,
    ToPad = case Rem of
                0 -> 0;
                N -> 4 - N
            end,
    string:left(String, Length + ToPad, $=).


base64_url_encode_char($+) ->
    $-;
base64_url_encode_char($/) ->
    $_;
base64_url_encode_char(Char) ->
    Char.
