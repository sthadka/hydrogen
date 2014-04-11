%% Sampling and randomization functions
-define(SHUFFLE(L),
        hydrogen_random:shuffle(L)).
-define(PICK(L),
        hydrogen_random:pick(L)).
-define(SAMPLE(N, L),
        hydrogen_random:sample(N, L)).
-define(RANDOM_IN_RANGE(Mn, Mx),
        hydrogen_random:random_in_range(Mn, Mx)).
-define(WEIGHTED_SELECT(L),
        hydrogen_random:weighted_random_selector(L)).
-define(WEIGHTED_SELECT(L, Mx),
        hydrogen_random:weighted_random_selector(L, Mx)).
-define(RANDOM_NORMAL(),
        hydrogen_random:random_normal()).
-define(RANDOM_GAUSSIAN(Mu, S),
        hydrogen_random:random_gaussian(Mu, S).
-define(RANDOM_UNIFORM(N),
        hydrogen_random:random_uniform(N)).
