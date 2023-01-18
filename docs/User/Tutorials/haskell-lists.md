<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# Haskell Lists

Lists are a versatile and very useful data structure in Haskell. Consider
streams as lists with IO support. If you use streams instead of lists you can
insert IO actions anywhere in your program without having to restructure it.

In this section we will go over basics of Haskell lists so that we can
compare and contrast them with streams later.

## Lists

List is a built-in data type that represents a sequence of items of the same
type.

Constructing lists:

```
>>> []
[]
>>> 1 : []
[1]
>>> 1 : 2 : 3 : 4 : 5 : []
[1,2,3,4,5]
>>> numbers = [1, 2, 3, 4, 5]
>>> numbers
[1,2,3,4,5]
```

Using lists:

```
>>> numbers !! 1
2
>>> length numbers
5
>>> sum numbers
15
```

Filtering:

```
>>> take 3 numbers
[1,2,3]
>>> drop 3 numbers
[4,5]
>>> filter odd numbers
[1,3,5]
```

Appending:

```
>>> numbers ++ [6,7]
[1,2,3,4,5,6,7]
```

Nested lists:

```
>>> concat [[1,2,3], [4,5]]
```

## Looping with Lists

In functional programming we avoid explicit loops or recursion, we
represent the sequence of data we want to operate on in a loop using
lists, and then we use functional combinators like map, fold and filter
on the list as an alternative to the explicit looping constructs found in
imperative languages.

## Pipeline of Transformations

```haskell
main =
    [1..3]       -- [Int]
      & map (+1) -- [Int]
      & take 2   -- [Int]
      & sum      -- Int
      & print    -- IO
```

That's an elegant way to write loops. Loops are composed as a sequence
of pipelines, each pipeline performing a particular transformation
(`map` and `take`) on the data flowing through the loop and finally we
fold (`sum`) the data as we want.
