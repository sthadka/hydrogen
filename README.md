# hydrogen - Erlang library for commonly used functions

[![Build Status](https://travis-ci.org/sthadka/hydrogen.svg?branch=master)](https://travis-ci.org/sthadka/hydrogen)

hydrogen is a project that aims to help you avoid repeating the same util code
in each of your project. Its goals are

* Be very useful and reusable
* Be easy to use and integrate with
* Be fast

## Usage

The modules can be directly called:

```
1> hydrogen_convert:to_atom("String").
'String'
```

or you could use the helpful macros using the include library `hydrogen_convert.hrl`
or just get everything including the general `hydrogen.hrl`

```
?TO_A(String)
```

## Modules

#### hydrogen_convert
Data type conversion library that you can use to cast data to a specific data
type. It tries its best or dies trying.

```
1> hydrogen_convert:to_float(12.35).
12.35
2> hydrogen_convert:to_float(<<"12.35">>).
12.35
3> hydrogen_convert:to_float(12).
12.0
4> hydrogen_convert:to_float("12.35").
12.35
5> hydrogen_convert:to_float("test").
** exception throw: {hydrogen,convert,not_a_valid_float,[{list,"test"}]}
     in function  hydrogen_convert:to_float/1 (src/hydrogen_convert.erl, line 98)
```

#### hydrogen_deploy
Helper functions to detect changed modules and deploy them.

#### hydrogen_ht
Hash table helper. Consistent access to different storage types in erlang.

#### hydrogen_node
Node helper functions.

#### hydrogen_proplist
Provides consistent interface to proplist, with some additional helpers.

#### hydrogen_random
Helpers for sampling, shuffling and picking random numbers.

#### hydrogen_time
Time conversion helpers.

#### hydrogen_web
Helpers functions for working with web resources.

#### hydrogen_cluster
Provides useful functions for managing a cluster.
