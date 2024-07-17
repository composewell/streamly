## Design

### Design Goals

Our goals for [Streamly][] from the very beginning have been:

1. To achieve simplicity by unifying abstractions.
2. To offer high performance.

These goals are hard to achieve simultaneously because they are usually
inversely related.  We have spent many years trying to get the abstractions
right without compromising performance.

`Unfold` is an example of an abstraction that we have created to achieve
high performance when mapping streams on streams.  `Unfold` allows stream
generation to be optimized well by the compiler through stream fusion.
A `Fold` with termination capability is another example which modularizes
stream elimination operations through stream fusion.  Terminating folds
can perform many simple parsing tasks that do not require backtracking.
In Streamly, `Parser`s are a natural extension to terminating `Fold`s;
`Parser`s add the ability to backtrack to `Fold`s.  Unification leads
to simpler abstractions and lower cognitive overheads while also not
compromising performance.

### Concurrency Design

Streamly uses lock-free synchronization for achieving concurrent
operation with low overheads.  The number of tasks performed concurrently
are determined automatically based on the rate at which a consumer
is consuming the results. In other words, you do not need to manage
thread pools or decide how many threads to use for a particular task.
For CPU-bound tasks Streamly will try to keep the number of threads
close to the number of CPUs available; for IO-bound tasks it will utilize
more threads.

The parallelism available during program execution can be utilized with
very little overhead even where the task size is very small, because
Streamly will automatically switch between serial or batched execution
of tasks on the same CPU depending on whichever is more efficient.
Please see our [concurrency benchmarks][concurrency-benchmarks] for more
detailed performance measurements, and for a comparison with the `async`
package.
