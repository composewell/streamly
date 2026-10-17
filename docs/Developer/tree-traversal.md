# Tree traversal design notes

These notes cover the tree and graph traversal combinators: the Stream
combinators in `Streamly.Internal.Data.Stream`, the StreamK ones in
`Streamly.Internal.Data.StreamK`, and the concurrent ones in
`Streamly.Internal.Data.Stream.Concurrent`. They record why the combinators
have the signatures they have, and what the benchmarks showed. The filesystem
side of directory traversal (symlinks, errors, options) is in
`FileSystem.DirIO.md`.

## The primitive form

The list `iterate` applies a function repeatedly to an element:

```
iterate :: (a -> a) -> a -> [a]
```

The streaming form generates a stream of elements from each element, instead
of a single element, and feeds every generated element back:

```
concatIterate :: (a -> Stream m a) -> Stream m a -> Stream m a
unfoldIterate :: Unfold m a a -> Stream m a -> Stream m a
```

The output is a traversal of a tree: an element is a node and the stream
generated from it is its children. A leaf is an element that generates an
empty stream. `iterate f x` is `concatIterate (fromPure . f) (fromPure x)`.

These two signatures can express any tree traversal. The forms below add a
`Maybe` or an `Either` to the types. They do not add power: they let the
traversal treat leaves differently, for performance.

## Traversal orders and resource use

Each form comes in five traversal orders: DFS, BFS, BFS Rev, wide DFS and wide
DFS Rev. Wide DFS goes depth first along a path, like DFS, but it emits all the
children of a node before it descends into any of them. BFS emits a whole level
before it opens the next one. The Rev variants expand the children, or the
elements of a level, in reverse order. This avoids a `reverse`, and was faster
on the binary and 10-ary trees in the benchmarks below.

| Order    | Open streams                 | Pending elements held                          |
|----------|------------------------------|------------------------------------------------|
| DFS      | one per level of the current path | none                                      |
| wide DFS | one                          | unexpanded children of the nodes on the current path |
| BFS      | one                          | the non-leaf elements of the next level        |

In a directory traversal an open stream is an open directory. DFS keeps one
file descriptor open for each directory on the current path. Wide DFS and BFS
keep one open, and read each directory from start to end before opening the
next one. Wide DFS holds memory proportional to the path, BFS to the widest
level.

## Marking the leaves: Maybe

In the primitive form a leaf generates an empty stream. The output is correct,
but BFS and wide DFS store every leaf in the state until its turn comes,
and DFS does an `inject`, a push, a step that returns `Stop` and a pop
for every leaf.

`concatIterate` therefore returns a `Maybe`, with `Nothing` for a leaf:

```
concatIterate :: (a -> Maybe (Stream m a)) -> Stream m a -> Stream m a
```

For a leaf the traversal stores nothing and does not generate a stream. The
stored state is then proportional to the number of non-leaf nodes, not the
total number of nodes.

The benchmark sums the nodes of complete trees of 10^6 nodes held in memory.
In a binary tree half the nodes are leaves, in a 1000-ary tree 999 of every
1000. Unfold BFS, without and with the leaf marker (time, peak memory):

| Tree     | `Unfold m a a`     | `b -> Maybe a`  |
|----------|--------------------|-----------------|
| binary   | 95 ms, 57 MB       | 46 ms, 33 MB    |
| 10-ary   | 145 ms, 104 MB     | 8.8 ms, 12 MB   |
| 1000-ary | 134 ms, 129 MB     | 4.2 ms, 6 MB    |

DFS stores no leaves either way, but it still gains from skipping the per-leaf
work: `unfoldIterate` took 9.00, 8.67 and 8.40 ms on the three trees,
`unfoldIterateMaybe` 5.76, 2.86 and 2.01 ms. The `Just` is not allocated: in
the 1000-ary BFS case the whole run allocated 94 KB.

## The unfold form: a seed type

An `Unfold` cannot mark a leaf itself: `inject :: a -> m s` has no way to say
"no children". The leaf marker has to be a separate function on the element.
For fusion the `Unfold` is a static argument, so this function cannot
construct the children the way the `concatIterate` function does. It can only
return the seed of the unfold, and the seed type can differ from the element
type:

```
unfoldIterateMaybe :: (b -> Maybe a) -> Unfold m a b -> Stream m b -> Stream m b
```

The unfold form now has two types, `b` for the elements and `a` for the seeds.
In a directory traversal the elements are `Either Path Path` and the unfold
`Dir.eitherReaderPaths` takes a `Path`:

```
unfoldIterateMaybe (either Just (const Nothing)) Dir.eitherReaderPaths input
```

With a single type and an `a -> Bool` predicate, the unfold would have to
accept `Either Path Path`, and the adapter for `Right` would be partial:

```
unfoldIterateBool isLeft
    (Unfold.lmap (either id (error "unreachable")) Dir.eitherReaderPaths)
    input
```

The predicate has already checked that the element is a `Left`, but the type
does not record that, so the unfold has to handle `Right` again. The
alternative to `error` is an unfold that yields nothing for `Right`, which
checks each element twice. With `b -> Maybe a`, the leaf check and the
conversion to a seed are one function.

`concatIterate` does not need the second type. Its function builds the child
stream itself and can pattern match on the element:

```
concatIterate (either (Just . Dir.readEitherPaths) (const Nothing)) input
```

## Marking the leaves: Either

Since the unfold form needs two types anyway, the leaf marker could be in the
element type instead, with `Left` for a non-leaf and `Right` for a leaf:

```
unfoldIterateLefts :: Unfold m a (Either a b) -> Stream m (Either a b) -> Stream m (Either a b)
unfoldIterateLefts Dir.eitherReaderPaths input
```

This has one argument less and matches the directory readers directly. For a
tree with a uniform node type, the classification moves into the unfold, using
`fmap` on the `Unfold`, which fuses. It is a special case of the `Maybe` form:

```
unfoldIterateLefts = unfoldIterateMaybe (either Just (const Nothing))
```

The converse is not true without tagging the non-leaves with both the element
and the seed: in the `Either` form the payload of a `Left` element is both the
output element and the seed, while the `Maybe` form can hold a seed smaller
than the element.

The `Either` forms were implemented as wrappers of this kind over the `Maybe`
forms and benchmarked on the in-memory trees. For a tree with a single node
type, the unfold tags each child with `fmap` on the `Unfold`, and
`fmap (either id id)` removes the tags from the output. The tags were not
allocated, but DFS took 15 to 20 percent more time. In the same benchmark
binary, at 10^6 nodes (ms):

| Tree     | `unfoldIterateMaybe` | `unfoldIterateLefts` |
|----------|----------------------|----------------------|
| binary   | 5.66                 | 6.82                 |
| 10-ary   | 2.81                 | 3.27                 |
| 1000-ary | 1.99                 | 2.31                 |

BFS took the same time in both forms, 45 to 49 ms for `Maybe` and 44 ms for
`Either` on the binary tree. A directory traversal has the tags in its
elements in either form.

The unfold combinators keep the `Maybe` form. It is more general, it is faster
for trees with a single node type, and it has the same shape as
`concatIterate`, whose function returns `Maybe`. The `Either` form saves one
argument, `either Just (const Nothing)`, and the haddock of each `Maybe` form
shows its `Either` form defined this way.

With `Either` elements, a traversal that emits only the leaves is
`Stream.catRights` applied to the output. No separate combinator is needed.

## Concat vs unfold

The `Unfold` is a static argument, so its step function fuses with the
traversal loop. The children streams of `concatIterate` are created at run
time, and their step functions are called as unknown functions.

On the in-memory trees above, at 10^5 and 10^6 nodes, `unfoldIterateMaybe`
was 12 to 18 times faster than `concatIterate`. When processing an element
costs much more than the traversal, the difference disappears: listing a tree
of 3906 directories took about 31 ms with every traversal, concat or unfold.

Both forms are kept. Concat is more flexible: the children function can build
any stream, using the element in any way. Unfold is faster when the traversal
itself is the cost.

All unfold traversals in one run, time in ms:

| Function                  | binary | 10-ary | 1000-ary |
|---------------------------|--------|--------|----------|
| `unfoldIterateMaybe`      | 5.76   | 2.86   | 2.01     |
| `wideDfsUnfoldIterate`    | 9.37   | 4.99   | 3.78     |
| `wideDfsUnfoldIterateRev` | 5.78   | 4.66   | 4.06     |
| `bfsUnfoldIterateMaybe`   | 50.8   | 8.87   | 4.17     |
| `bfsUnfoldIterateRev`     | 31.5   | 7.06   | 4.32     |

## StreamK forms

The StreamK combinators keep the primitive `a -> StreamK m a` signature, and
take the function that combines the streams as an argument:

```
concatIterateWith, mergeIterateWith
    :: (StreamK m a -> StreamK m a -> StreamK m a)
    -> (a -> StreamK m a)
    -> StreamK m a
    -> StreamK m a
```

`concatIterateWith` combines the streams with `concatMapWith`. With `append`
the result is DFS:

```
concatIterateWith combine f = iterateStream
    where
    iterateStream = concatMapWith combine generate
    generate x = x `cons` iterateStream (f x)
```

`mergeIterateWith` uses `mergeMapWith` instead, which combines the streams
pairwise, as a balanced tree. With `interleave` this gives a balanced
traversal of the tree.

In these forms the traversal order is decided by the combine function, not by
the combinator name. They are CPS streams and do not fuse. They are the
general form, and the way to get orders that the Stream combinators do not
provide.

The `Either` form is in StreamK too, defined from the primitive form:

```
concatIterateLeftsWith combine f = concatIterateWith combine (either f (const nil))
```

`iterateM` is a special case:

```
iterateM f = concatIterateWith append (fromEffect . f) . fromEffect
```

A feedback loop, in which `f` generates either values to feed back (`b`) or
outputs (`c`), and a function `fb` turns a fed-back value into new input, is
also a special case:

```
loop :: (a -> StreamK m (Either b c)) -> (b -> StreamK m a) -> StreamK m a -> Stream m c
loop f fb =
      Stream.catRights
    . StreamK.toStream
    . concatIterateLeftsWith append (concatMapWith append f . fb)
    . concatMapWith append f
```

The `Left b` elements are the non-leaves; their children are `fb b` with `f`
applied to each element.

## Chunked elements

The element type can be a batch of nodes instead of one node. No new
combinator is needed. The per-element cost of the traversal, and of every
later stage of the pipeline, is then paid once per batch:

```
f :: Either [Path] b -> Stream IO (Either [Path] (Array Word8))
f = either (Dir.readEitherByteChunks id) (const Stream.nil)
```

On a tree of about 10,000 directories and 50,000 files, hot caches, with
`concatIterate`:

| Element                                   | Time   | Allocation |
|-------------------------------------------|--------|------------|
| one path (`readEitherPaths`)              | 184 ms | 161 MB     |
| batch of paths (`readEitherChunks`)       | 144 ms | 32 MB      |
| batch of directories, byte-array output (`readEitherByteChunks`) | 134 ms | 21 MB |

The traversal order then applies to batches:

- `readEitherChunks` emits the subdirectories of a batch as new batches of 4
  and does not descend into them. BFS on these batches is BFS on directories,
  because a batch contains directories of one level only. DFS on batches is
  not DFS on directories.
- `readEitherByteChunks` traverses part of the tree itself. It adds the
  subdirectories it finds to its own list, fills a 32,000-byte output buffer,
  and when the buffer is full hands half of its pending directories back to
  the combinator. DFS, BFS and wide DFS then took the same time, 154 ms, in
  the `ListDir.hs` example. A consumer that needs a specific order, such as
  `cp -r`, needs one path or one directory per element.

## Stateful and graph traversal

A graph traversal is a tree traversal that does not visit a node twice. It
needs state: the set of visited nodes. There are four places to keep it:

| State            | Where                                   | Combinators                    | Order |
|------------------|-----------------------------------------|--------------------------------|-------|
| global, mutable  | in the children function, e.g. `filterM` on an `IORef` or a bitmap | all, including concurrent | that of the combinator |
| global, pure     | in the traversal loop                   | `concatIterateScan`            | decided by the generator |
| per path         | in the traversal                        | `StreamK.concatIterateScanWith` | decided by the combine function |
| per path         | in the element, as `(state, node)`      | all                            | that of the combinator |

**Global mutable state.** The visited check goes in the children function:

```
visit x = atomicModifyIORef' ref $ \s ->
    if Set.member x s then (s, False) else (Set.insert x s, True)
next x = Just $ Stream.filterM visit (Stream.fromList (adj x))
```

The check runs when a child is pulled from its parent's stream, so nodes are
marked in the order the traversal reaches them. With `atomicModifyIORef'` it
is thread safe and works unchanged with `parConcatIterate`. With the unfold
traversals the filter goes on the `Unfold`, `Unfold.filterM visit`.

A BFS over a graph of 10^6 nodes and 3 edges per node, with
`bfsConcatIterate`, took 1,500 ms with an `IORef IntSet` and 680 to 750 ms
with an unboxed `MutArray Bool` bitmap updated with `modifyIndex`.

**Global pure state: `concatIterateScan`.**

```
concatIterateScan :: Monad m
    => (b -> a -> m b)                   -- update the state with an output
    -> (b -> m (Maybe (b, Stream m a)))  -- next stream from the state
    -> b                                 -- initial state
    -> Stream m a
```

It generates a stream from the state, runs every element of the stream
through the scan function, and when the stream ends generates the next stream
from the final state. It stops when the generator returns `Nothing`. With the
visited set and the next level as the state, it gives a BFS with an immutable
`Set`. It runs one stream at a time and is serial.

**Per-path state: `concatIterateScanWith`.**

```
concatIterateScanWith :: Monad m
    => (StreamK m a -> StreamK m a -> StreamK m a)
    -> (b -> a -> m (b, StreamK m a))
    -> m b
    -> StreamK m a
    -> StreamK m a
```

The function receives the parent's state and a node, and returns the state
for the node's children and the stream of the children. Siblings start from
the same state. With the ancestors of a node as the state, a node is expanded
only into children not on its path. A directory walker that follows symbolic
links needs this check: a directory that is its own ancestor is a loop, but
two links to the same directory are not. It is a StreamK combinator and does
not fuse. For speed, the per-path state can be carried in the element instead,
which works with every fused combinator.

## Concurrent traversal

```
parConcatIterate :: MonadAsync m
    => (Config -> Config) -> (a -> Stream m a) -> Stream m a -> Stream m a
```

It has the structure of `concatIterateWith`, with `concatMapWith append`
replaced by `chanConcatMapK`, the concat-map of a concurrent channel, as used
by `parConcatMap`. The whole traversal uses one channel:

```
parConcatIterate modifier f input =
    Stream.fromStreamK
        $ withChannelK modifier (Stream.toStreamK input) iterateStream
    where
    iterateStream chan = chanConcatMapK modifier chan (generate chan)
    generate chan x = x `K.cons` iterateStream chan (Stream.toStreamK $ f x)
```

A worker takes a stream from the work queue, takes its first element, puts the
rest of the stream back on the work queue, and evaluates the expansion of the
element. Every stream is split at every element, so any idle worker can pick
up the rest of any stream. Each split is a queue operation shared between
threads, which is why chunked elements help most in the concurrent case.

The configuration decides the order:

- default: the work queue is LIFO, and results are emitted as they are
  produced;
- `interleaved True`: the work queue is FIFO, and streams are scheduled round
  robin;
- `ordered True`: the output is in the order of the serial DFS;
- `maxThreads n` and `maxBuffer n` limit the workers and the buffered results.

`ListDir.hs` from streamly-examples, one directory tree, hot caches. These
numbers are from a separate measurement, not the one in the chunked table:

| Element        | serial `concatIterate` | `parConcatIterate id` | `interleaved True` | `ordered True` |
|----------------|------------------------|-----------------------|--------------------|----------------|
| one path       | 274 ms                 | 174 ms                | 224 ms             | 234 ms         |
| batch of paths | 264 ms                 | 124 ms                | 134 ms             | 174 ms         |
| byte chunks    | 154 ms                 | 94 ms                 | 94 ms              | 154 ms         |

`parConcatIterate` takes the primitive `a -> Stream m a`, with `Stream.nil`
for a leaf.

Not implemented yet:

- `parMergeIterate`, the concurrent `mergeIterateWith`, is declared but is
  `undefined`.
- Concurrent BFS or wide DFS with a guaranteed order. That needs a barrier
  between levels.

## Bottom-up reduction

The combinators above generate a tree top down, from the roots, and output a
stream. A reduction goes the other way: the elements of the input stream are
the leaves, and the tree is built bottom up into one value.

```
bfsReduceIterate :: Monad m => (a -> a -> m a) -> Stream m a -> m (Maybe a)
```

`bfsReduceIterate` combines the elements of a level pairwise, collects the
results as the next level, and repeats until one value is left. The last
elements of the previous level are combined first. It holds one level, half
the size of the previous one.

An n-ary version using a `Fold` is declared but not implemented:

```
bfsFoldIterate :: Fold m a (Either a a) -> Stream m a -> m (Maybe a)
```

The stream counterpart is `StreamK.mergeMapWith`, which combines streams
pairwise as a balanced tree. A merge sort has this shape:
`StreamK.mergeMapWith (StreamK.mergeBy compare) StreamK.fromPure`.

## Current API

All the Stream combinators take the `Maybe` leaf marker. `unfoldIterate`,
`bfsUnfoldIterate` and the other `Unfold m a a` forms are deprecated and
defined as the `Maybe` form applied to `Just`.

| Order        | Concat                    | Unfold                    |
|--------------|---------------------------|---------------------------|
| DFS          | `concatIterate`           | `unfoldIterateMaybe`      |
| BFS          | `bfsConcatIterate`        | `bfsUnfoldIterateMaybe`   |
| BFS Rev      | `bfsConcatIterateRev`     | `bfsUnfoldIterateRev`     |
| wide DFS     | `wideDfsConcatIterate`    | `wideDfsUnfoldIterate`    |
| wide DFS Rev | `wideDfsConcatIterateRev` | `wideDfsUnfoldIterateRev` |

The other combinators:

| Module             | Combinators                                                    |
|--------------------|----------------------------------------------------------------|
| `Stream`           | `concatIterateScan`                                            |
| `Stream` (reduction) | `bfsReduceIterate`                                           |
| `StreamK`          | `concatIterateWith`, `mergeIterateWith`, `concatIterateScanWith`, `concatIterateLeftsWith` |
| `Stream.Concurrent` | `parConcatIterate`                                            |

The deprecated `IsStream.Expand` module still has the 2020 names:
`iterateMapWith`, `iterateMapLeftsWith`, `iterateSmapMWith` and
`iterateUnfold` (unimplemented).

## Naming

The names pair the one level combinators with their recursive versions:

| One level      | Recursive            |
|----------------|----------------------|
| `concatMap`    | `concatIterate`      |
| `unfoldEach`   | `unfoldIterate`      |
| `concatMapWith`| `concatIterateWith`  |
| `mergeMapWith` | `mergeIterateWith`   |

The `Iterate` suffix means that the output is fed back as input, and keeps the
connection with `iterate`. The traversal order is a prefix, `bfs` or
`wideDfs`, and DFS has no prefix, because it is the order that nested
`concatMap` gives. `Rev` means that the pending elements, the children in wide
DFS or a level in BFS, are expanded in reverse order. Concurrent versions take
a `par` prefix.

Proposed changes:

* **The `Maybe` suffix.** `unfoldIterateMaybe` and `bfsUnfoldIterateMaybe`
  have it only because the plain names are taken by the deprecated
  `Unfold m a a` versions; the other unfold traversals do not have it. When
  the combinators are exposed in `Streamly.Data.Stream`, the plain names
  should take the `Maybe` signature, and the `Maybe` names should become
  deprecated aliases.
* **`concatIterateScan` vs `concatIterateScanWith`.** The two have the same
  root but different semantics. The `Stream` one keeps one global state and
  generates the next stream from the final state of the previous one, a
  feedback loop. The `StreamK` one traverses a tree and passes a state down
  each path. Rename the `StreamK` one to `concatIterateAccumWith`, as the
  `XXX` note on it suggests, by analogy with `mapMAccum`.
* **An `Either` form for unfolds**, if added, should be `unfoldIterateLefts`,
  matching `concatIterateLeftsWith`.
* **`parConcatIterate`** takes `a -> Stream m a`, while `concatIterate` takes
  `a -> Maybe (Stream m a)`. With the same signature, moving between the
  serial and the concurrent traversal changes only the name.

The primitive forms are not provided as separate `Stream` combinators. They
are the `Maybe` forms applied to `Just`, and the `Just` is removed after
inlining:

```
concatIterate (Just . f)
unfoldIterateMaybe Just
```

In `StreamK` the primitive form is the general form, `concatIterateWith`.

## Expressiveness and gaps

What can be expressed with the current combinators:

| Need                          | How                                                    |
|-------------------------------|--------------------------------------------------------|
| DFS, BFS, wide DFS            | the fixed order combinators                            |
| other orders, balanced        | `concatIterateWith` or `mergeIterateWith` with a combine function |
| prune a subtree               | `Nothing` or a filter in the children function         |
| stop early                    | `take` or `takeWhile` on the output                    |
| depth limit, path             | carried in the element                                 |
| visited set, cycles           | state in the children function, the traversal or the element |
| leaves only                   | `Stream.catRights` on `Either` elements                |
| less per-element cost         | chunked elements                                       |
| concurrency                   | `parConcatIterate`                                     |
| iterative deepening           | repeated depth limited traversals                      |
| reduce a flat stream as a tree | `bfsReduceIterate`                                    |
| fixpoint iteration, a level at a time | `concatIterateScan`: the state is the current level, the generator returns `Nothing` when nothing changes |

Gaps:

* **Post-order and enter/exit events.** Some tasks need a node after its
  children: `rm -r` deletes the contents of a directory before the directory,
  `du` sums the sizes of the children, topological sort is the reverse
  post-order of a DFS. With DFS this can be expressed by tagging, emitting an
  `Exit` event after the children of a node:

  ```
  f (Enter x) = Just (fmap Enter (children x) <> fromPure (Exit x))
  f (Exit _)  = Nothing
  ```

  The output is a stream of enter and exit events, as `fts` reports `FTS_D`
  and `FTS_DP`. Post-order is the `Exit` events. This should be documented or
  provided as a combinator.
* **Bottom-up aggregation over a generated tree**, a catamorphism: a value for
  each node computed from the values of its children, for example the size of
  each directory in `du`. `bfsReduceIterate` does not cover it: it reduces a
  flat stream as a balanced tree, and the tree has no relation to the
  generated one. With the enter/exit events, a fold that keeps a stack covers
  it: on `Enter` it pushes an empty accumulator, on `Exit` it pops the node's
  value and adds it to its parent's accumulator.
* **Best-first order** (Dijkstra, A*). The fixed order combinators cannot
  express it. When every child is ordered after its parent, as with
  non-negative edge weights, `concatIterateWith (mergeBy compare)` should give
  a best-first order: each subtree's output is sorted, and merging sorted
  streams keeps them sorted. This has not been verified. Priorities that do not
  increase along a path need a priority queue traversal.
* **Concurrent BFS and wide DFS with a guaranteed order**, and
  `parMergeIterate`. Not implemented.
* **Fixpoint iteration in general**, applying a stream transformation
  `Stream m a -> Stream m a` until the output does not change. Only the level
  at a time form is covered, by `concatIterateScan`. The general form is
  outside tree traversal and is not covered here.
* **Concurrent unfold traversal.** Not provided. When the work is concurrent,
  the cost of processing an element usually dominates, and fusion does not
  help: listing 3906 directories took about 31 ms with every serial traversal,
  concat or unfold.

## Implementation notes

**Delayed inject.** BFS and wide DFS unfold traversals store the seed of a
pending element and call `inject` only when its turn comes. The earlier
versions stored the injected state, which for a directory is an open
directory, so BFS kept a directory open for every pending entry and could fail
with "too many open files". With `ulimit -n 64` every traversal, concat and
unfold, now lists the 3905-directory tree. The concat versions never had this
problem: `Stream.unfold` opens the directory on the first step of the stream,
not when the stream is created.

**NoSpecConstr on the stack.** In `unfoldIterate` the stack of suspended inner
states is wrapped in a newtype annotated `NoSpecConstr`. Without it,
SpecConstr creates a copy of the loop for each stack depth it sees, and falls
back to a generic loop that boxes the state on every step after that. With
`-fspec-constr-recursive=16` the module took 376 s to compile and then ran out
of memory at 1.5 GB; with the newtype it took 11 s. With the newtype, SpecConstr
still specializes the loop on the current inner state, and the traversal ran
4 to 7 times faster, for example 7.7 ms against 29.4 ms on the binary tree.

## TODO

* Measure whether a `Maybe` leaf marker, `a -> Maybe (Stream m a)`, makes
  `parConcatIterate` faster, as it does for the serial traversals. A leaf now
  returns `Stream.nil`, and `generate` still calls `iterateStream` on it.
* Change `parConcatIterate` to `a -> Maybe (Stream m a)`, the signature of
  `concatIterate`.
* Rename `StreamK.concatIterateScanWith` to `concatIterateAccumWith`.
* Pick the final unfold names, without the `Maybe` suffix, when the
  combinators are exposed.
* Document the enter/exit event pattern, or add a combinator for it, with a
  per-node aggregation example such as `du`.
* Verify that `concatIterateWith (mergeBy compare)` gives a best-first order
  when children are ordered after their parents.

## Appendix: history

| Date     | Change |
|----------|--------|
| Nov 2019 | `concatMapTreeWith :: (a -> t m (Either a b)) -> t m (Either a b) -> t m (Either a b)`, with the leaf marker in the element type. An unfold version, `concatUnfoldTree :: Unfold m a (Either a b) -> ...`, was planned but not implemented. |
| Dec 2019 | The single typed `concatMapIterateWith :: (a -> t m a) -> t m a -> t m a`, the feedback loop `concatMapLoopWith`, and `concatMapTreeYieldLeavesWith`, which emits only the leaves. |
| Aug 2020 | The single typed form became the core combinator, renamed `iterateMapWith`, so that it reads like an iterate operation. `concatMapTreeWith` became `iterateMapLeftsWith = iterateMapWith combine (either f (const nil))`. The leaves-only version was dropped as `rights` on the output. `concatMapLoopWith` was replaced by the stateful `iterateSmapMWith`. The unfold version was single typed, `iterateUnfold :: Unfold m a a` (unimplemented). |
| Dec 2022 | Fixed order combinators: `iterateConcatMapDFS`, `iterateConcatMapBFS` and `iterateConcatMapBFSRev` with `a -> Maybe (Stream m a)`, so that leaves are not stored, and `iterateUnfoldManyDFS` etc. with `Unfold m a a`. The `Maybe` gives the distinction of the `Either` tag, but was added for performance and keeps the single element type. `iterateConcatScan` added. `iterateConcatMapWith` added, and `iterateMapWith` changed to the pairwise `iterateMergeMapWith`. `iterateMapLeftsWith` renamed to `iterateLeftsConcatMapWith`, `iterateSmapMWith` to `iterateConcatScanWith`. |
| Jan 2023 | Renames: `iterateConcatMap*` to `concatIterate*` (`concatIterateDfs`, `concatIterateBfs`, `concatIterateBfsRev`, `concatIterateWith`, `concatIterateScan`, `concatIterateScanWith`, `concatIterateLeftsWith`), `iterateMergeMapWith` to `mergeIterateWith`, `iterateUnfoldMany*` to `unfoldIterate*`. `parConcatIterate` and `reduceIterateBfs` added. The StreamK forms moved to the StreamK module during 2023. |
| Jan 2025 | `parMergeIterate` declared as an unimplemented prototype. |
| Aug 2025 | Renames to the order-prefix convention: `concatIterateDfs` to `concatIterate`, `concatIterateBfs` to `bfsConcatIterate`, `concatIterateBfsRev` to `altBfsConcatIterate`, the same for the unfold versions, `reduceIterateBfs` to `bfsReduceIterate` and `foldIterateBfs` to `bfsFoldIterate`. |
| Oct 2026 | `altBfsConcatIterate` renamed to `wideDfsConcatIterateRev`, and `wideDfsConcatIterate`, `bfsConcatIterateRev` and the unfold equivalents added. The unfold versions take `b -> Maybe a` (`unfoldIterateMaybe`, `bfsUnfoldIterateMaybe`). The fixed `Unfold` brings in a second type, so the `Either` form was tried again as wrappers (`unfoldIterateLefts`) and dropped: it was 15 to 20 percent slower in DFS on trees with a single node type. |
