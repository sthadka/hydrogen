%%%-------------------------------------------------------------------
%%% @doc hydrogen_random
%%%
%%% Sampling and randomization library
%%% @end
%%%-------------------------------------------------------------------

-module(hydrogen_random).

-export([shuffle/1,
         pick/1,
         sample/2,
         random_in_range/2,
         weighted_random_selector/1, weighted_random_selector/2,
         random_normal/0,
         random_gaussian/2,
         random_uniform/1]).

%% @doc Takes a list and randomly shuffles it. Relies on random:uniform
%% inspired by: http://www.trapexit.org/RandomShuffle
-spec shuffle(list()) -> list().
shuffle(List) ->
    D = [ {random:uniform(), E} || E <- List ],
    {_, ShuffledList} = lists:unzip(lists:keysort(1, D)),
    ShuffledList.

%% @doc Pick a random element from given list
-spec pick([] | list()) -> undefined | any().
pick([]) ->
    undefined;
pick(List) ->
    hd(sample(1, List)).

%% @doc Return a random sample of N elements from a given list
-spec sample(pos_integer(), list()) -> undefined | list().
sample(_N, []) ->
    undefined;
sample(1, [E]) ->
    [E];
sample(1, List) ->
    lists:sublist(List, random:uniform(length(List)), 1);
sample(N, List) when N >= length(List) ->
    List;
sample(N, List) ->
    lists:sublist(shuffle(List), N).

%% @doc Get a random integer between Min and Max (both inclusive)
-spec random_in_range(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
random_in_range(Min, Max) when Min =:= Max ->
    Min;
random_in_range(Min, Max) when Min > Max ->
    random_in_range(Max, Min);
random_in_range(Min, Max) ->
    Min + random:uniform(Max - Min + 1) - 1.

%% @doc Select a random option based on its weight
-spec weighted_random_selector([{term(), integer()}]) -> term() | undefined.
weighted_random_selector(Options) ->
    AvailableOptions = lists:filter(fun({_Opt, W}) -> W > 0 end, Options),
    Total = lists:foldl(fun({_Opt, W}, Sum) -> W + Sum end, 0, AvailableOptions),
    weighted_random_selector(Options, Total).

%% @doc Select a random option based on its weight with max random value
%% provided
-spec weighted_random_selector([{term(), integer()}], pos_integer()) ->
    term() | undefined.
weighted_random_selector([], _MaxRandomVal) ->
    undefined;
weighted_random_selector(Options, MaxRandomVal) ->
    % Filter options with weight = 0
    AvailableOptions = lists:filter(fun({_Opt, W}) -> W > 0 end, Options),
    case MaxRandomVal > 0 of
        true ->
            Random = random:uniform(MaxRandomVal),
            select_option(Random, AvailableOptions);
        false -> undefined
    end.

-spec random_gaussian(integer(), float()) -> float().
random_gaussian(Mu, Sigma) ->
    Mu + random_normal() * Sigma.

-spec random_normal() -> float().
random_normal() ->
    % When x and y are two variables from [0, 1), uniformly
    % distributed, then
    %
    %    cos(2*pi*x)*sqrt(-2*log(1-y))
    %    sin(2*pi*x)*sqrt(-2*log(1-y))
    %
    % are two *independent* variables with normal distribution
    % (mu = 0, sigma = 1).
    math:cos(2 * math:pi() * random:uniform()) *
    math:sqrt((-2.0 * math:log(1 - random:uniform()))).

%% @doc A small wrapper around random:uniform allowing to be called with zero
%% and negative integers without crashing.
-spec random_uniform(integer()) -> integer().
random_uniform(0) ->
    0;
random_uniform(N) when N > 0 ->
    random:uniform(N);
random_uniform(N) when N < 0 ->
    -random:uniform(-N).

%%-------------------------------------------------------------------
%% Internal helpers
%%-------------------------------------------------------------------
select_option(_Random, []) ->
      undefined;
  select_option(Random, [{Option, Weight} | Options]) ->
      case 0 >= (Random - Weight) of
          true -> Option;
          false -> select_option(Random - Weight, Options)
      end.
