%%% -----------------------------------------------------------------------------
%%% @doc hydrogen_cluster
%%%
%%% Cluster helper functions
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_cluster).

-export([nodes/0]).

%% @doc Get a list of all the nodes in the cluster, including current one.
-spec nodes() -> [node()].
nodes() ->
    [node() | erlang:nodes()].
