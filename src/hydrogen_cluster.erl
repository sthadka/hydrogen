%%% -----------------------------------------------------------------------------
%%% @doc hydrogen_cluster
%%%
%%% Cluster helper functions
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_cluster).

-export([nodefinder/0]).

nodefinder() ->
    [node() | nodes()].
