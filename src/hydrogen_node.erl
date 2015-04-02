%%% -----------------------------------------------------------------------------
%%% @doc hydrogen_node
%%%
%%% Node helper functions
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_node).

-include("hydrogen_convert.hrl").

-export([hostname/0, hostname/1]).

hostname() ->
    hostname(node()).

hostname(Node) ->
    case string:tokens(?TO_L(Node), "@") of
        [_NodeName, Hostname] ->
            Hostname;
        _ ->
            []
    end.
