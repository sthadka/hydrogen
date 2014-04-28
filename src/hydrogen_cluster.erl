%%% -----------------------------------------------------------------------------
%%% @doc hydrogen_cluster
%%%
%%% Cluster helper functions
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_cluster).

-include("hydrogen_convert.hrl").

-export([nodefinder/0]).

nodefinder() ->
    [node() | nodes()].
