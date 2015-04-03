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
2> hydrogen_convert:to_list(<<"binary">>).
"binary"
3> hydrogen_convert:to_list(atom).
"atom"
4> hydrogen_convert:to_list(10).
"10"
```

#### hydrogen_deploy
#### hydrogen_ht
#### hydrogen_node
#### hydrogen_proplist
#### hydrogen_random
#### hydrogen_sup
#### hydrogen_time
#### hydrogen_web
#### hydrogen_cluster
