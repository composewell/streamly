# Streamly Design Goals and Innovations

## Contradictory Goals

Our goals for Streamly from the very beginning have been:

1. To achieve simplicity by unifying abstractions.
2. To offer high performance.

These goals are hard to achieve simultaneously, they contradict
each other. If we create nice beautiful abstractions, they do
not perform well. If we optimize for performance we cannot have
beautiful abstractions. We have thought long and hard to achieve
both these contradictory goals at the same time, we are always
continuously striving for that.

Streamly abstractions have matured over many years. We have gone through
the pains of breaking existing deployments of the library to ensure that
the abstractions are right. In streamly-0.9.0 we went through a major
breaking change to ensure that the performance, usability, and unified
abstraction goals are satisfied in the long run.

## Simplicity and Performance

In Haskell, it is relatively easier to design beautiful abstractions but
the hard part is performance which has been our primary goal. We have
not left any stone unturned to achieve that. Abstractions are useless
unless the performance goal is met.

After the fact, it may look simple, but we have gone through a rigorous
grind of researching and innovating over the years to come up with
the simplicity and performance we have today.

“Simplicity is a great virtue but it requires hard work to achieve it
and education to appreciate it. And to make matters worse: complexity
sells better.” — Edsger Wybe Dijkstra

All changes go through performance benchmarking. We do not tolerate any
performance regression. We have a vast set of performance benchmarks,
there are benchmarks for each and every operation. Each performance
problem reported by the benchmarks leads to a new understanding, to
fixes for a class of problems or to new design decisions. This leads
to better design and stability of performance aspects.  Sometimes new
releases of GHC have broken the performance badly which is caught by the
benchmarks and often we have reported it to the GHC folks and they have
fixed the issues.

## Motivation

I started looking at the programming language landscape in 2015 with
the purpose of choosing the ideal language that I would like to use if
I have start a fresh project. I discovered Haskell for the first time
that year, it had everything that I always wished for in a programming
language, but I never knew about it.

After programming in Haskell for a while I felt that there were too many
options for libraries yet there were no good choices in many cases. You
had to learn too many DSLs. You still had to write low level code if
you wanted good performance, idiomatic code was only meant for average
performance. Haskell could do better especially because GHC could do
powerful optimizations such as stream fusion, the vector library already
demonstrated that it could give you the raw performance of C. Why can't
we have both idiomatic code and high-performance?

`transient` provided concurrency and distributed computing in a nice
way which I liked. But I wanted a unified programming framework with
high-performance so that we could use it for everything and not just
something special for concurrency and distributed computing. Instead
of many different libraries for different use cases, with their own
idiosyncracies and performance issues, we needed a unified core and
everything around it should be designed for it and well-integrated with
it.

I started a quest for the holy grail and wrote the first version of
streamly in 2017. The first version was inspired by `transient` because
I wanted it to be concurrent to begin with.

## Design Innovations

Over the course of many years of development of streamly a lot of tough
problems were solved by discovering and using existing knowledge,
sometimes re-inventing, and sometimes by innovations. We are listing
some of those here in a chronological order.

During the initial development problems were much harder, because it
was a blank slate, everything was undefined and we had a goal to unify
the solution to all sorts of programming problems in a single framework
which was a tough task. All design challenges finally boiled down to
a single root challenge, how to create concise and elegant APIs with
good performance in all cases. After thinking hard we found the best of
both worlds in many cases, but we also had to make trade offs at times,
we mostly chose performance over some complexity of APIs unless the
performance difference was not significant.

### CPS Based Stream Representation (2017)

It was clear to me that streaming is the paradigm to be used for
modularity and declarative expression. So whatever I wanted to write
would have some streaming nature. But concurrency was a first class
goal, so we had to somehow marry streaming with concurrency.

We already had `conduit`, `pipes`, `streaming` available so why not use
those one might ask. They all had one or the other problem. Conduit
and pipes had a complicated API and performance issues in some cases,
also the implementation was not straightforward, bidirectionality made
them even more complex, which made it hard to play with them to try out
concurrency implementation. Streaming had a relatively simpler API and
much simpler implementation, though complicated a bit by the functor
general design. It also had some performance issues. If I had to use one
of these then `streaming` would have been the one to use. But none of
these types could meet the requirements listed below.

I wanted something very close to lists in API and I wanted built-in
concurrency support. For concurrency implementation I wanted to pass a
state through the stream, and for performance reasons, the state had
to be built into the type rather than using a monad like StateT.  So I
started playing with the simplest possible type from scratch and wanted
to see how it evolves. The requirements were as follows:

* Should be similar to lists but effectful (Stream m a)
* Build the stream like lists using cons and nil
* Monad instance should behave like lists (list-t)
* Should be able to implement logic-t without performance issues
* Amenable to implicit concurrent evaluation
* Simple enough to be able to play with concurrent implementation

The CPS type that evolved from these requirements was something that
no other library had used before. It was pretty simple and the entire
implementation was very simple.

```haskell
newtype StreamK m a =
    MkStream (forall r.
               State StreamK m a         -- state
            -> (a -> StreamK m a -> m r) -- yield
            -> m r                       -- stop
            -> m r
            )
```

Later I learnt that this is known as Scott encoding. We were able to
build streams like lists using cons and nil, we could implement list-t
with good performance, we could implement the functionality of logic-t
as well and it did not suffer from the problems that logic-t suffered
from.

### Declarative Concurrency (2017)

In the Haskell community, we heard statements like concurrency and
streaming are completely independent and unrelated to each other.
But concurrency was a first-class requirement for streamly. In fact,
concurrency was the primary requirement. So we had to challenege that
notion. What evolved from this was magical.

The basic idea is to build a stream of monadic actions and have the ability to
evaluate these actions concurrently. We wanted the concurrent evaluation to be
transparent to the programmer.

```haskell
newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}

instance IsStream AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT
    consM = Async.consMAsync
```

Monadic concurrency is magical, no other language can provide it.

### Lock-free Concurrency (2017)

Streamly uses lock-free synchronization for achieving concurrent
operation with low overheads.  The number of tasks performed concurrently
are determined automatically based on the rate at which a consumer
is consuming the results. In other words, you do not need to manage
thread pools or decide how many threads to use for a particular task.
For CPU-bound tasks Streamly will try to keep the number of threads
close to the number of CPUs available; for IO-bound tasks it will utilize
more threads.

### Low-overhead Concurrency (2017)

The parallelism available during program execution can be utilized with
very little overhead even where the task size is very small, because
Streamly will automatically switch between serial or batched execution
of tasks on the same CPU depending on whichever is more efficient.
Please see our [concurrency benchmarks][concurrency-benchmarks] for more
detailed performance measurements, and for a comparison with the `async`
package.

### On-demand Concurrency Scaling (2017)

### Integrating Stream Fusion

### Fusion Plugin

Stream fusion is one of the staple pillars of performance in
streamly. All abstractions are designed to take advantage of stream
fusion. We found a lot of issues breaking stream fusion in the
beginning. We took it on a case by case basis and solved most of the
problems.

### Nested Stream Fusion (Unfold)

`Unfold` is an example of an abstraction that we have created to achieve
high performance when mapping streams on streams.  `Unfold` allows stream
generation to be optimized well by the compiler through stream fusion.

### Fold Fusion (Fold)

A `Fold` with termination capability is another example which modularizes
stream elimination operations through stream fusion.  Terminating folds
can perform many simple parsing tasks that do not require backtracking.
In Streamly, `Parser`s are a natural extension to terminating `Fold`s;
`Parser`s add the ability to backtrack to `Fold`s.  Unification leads
to simpler abstractions and lower cognitive overheads while also not
compromising performance.

### Nested Fold Fusion (Refold)

### Unifying Parsers with Folds

Fold and Parser.

### Unified Array Abstractions

