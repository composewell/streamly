# Internal vs External Modules

## Exposing Internal Modules

We keep all modules exposed to facilitate convenient exposure of experimental
APIs and constructors to users. It allows users of the library to experiment
much more easily and carry a caveat that these APIs can change in future
without notice.  Since everything is exposed, maintainers do not have to think
about what to expose as experimental and what remains completely hidden every
time something is added to the library.

## Internal module Namespace

We expose the internal modules via `Streamly.Internal.*` namespace to keep all
the internal modules together under one module tree and to have their
documentation also separated under one head in haddock docs.

## Exposed Modules as Wrappers

Another decision point is about two choices:

* Keep the implementation of all the APIs in an internal module and just
reexport selected APIs through the external module. The disadvantages of this
are:

  1) users may not be able to easily figure out what unexposed APIs are available
  other than the ones exposed through the external module. To avoid this problem
  we can mark the unexposed APIs in the docs with a special comment.

  2) In tests and benchmarks we will be using internal modules to test internal
  and unexposed APIs. Since exposed APIs are exported via both internal and
  external modules we will have to be careful in not using the internal module
  for testing accidentally, instead we should always be using the exposed module
  so that we are always testing exactly the way users will be using the APIs.

* Keep the implementations of unexposed modules in the internal module file
and exposed module in the external module file. In this approach, users can
easily figure out the unexposed vs exposed APIs. But maintaining this would
require us to move the APIs from internal to external module file whenever we
expose an API.

We choose the first approach.

# Module Types and Naming

## Abstract modules

An abstract module represents an abstract interface using a type
class.  Multiple concrete modules can make use of this interface and possibly
extend it.

For example we have an `IsStream` type class that all stream types
implement.  The concrete stream types are `SerialT`, `AsyncT`
etc. Assuming `Streamly.Data.Stream` is the containing name space for
all stream modules, there are multiple ways to organize abstract and
concrete modules. The `IsStream` type class can be placed in:

* `Streamly.Data.Stream.Class` or `Streamly.Data.Stream.Interface`
* `Streamly.Data.Stream.IsStream`
* `Streamly.Data.Stream`

Using `Streamly.Data.Stream.IsStream` is preferred over `.Class` because it
conveys more information by the class name. We can place it in
`Streamly.Data.Stream` as well.

Concrete modules depend on the abstract module `IsStream` to implement
that interface.

Polymorphic operations utilizing the abstract interface can go in the
parent module `Streamly.Data.Stream`.

## Constrained Modules

Some modules represent operations on a type which constrain a type using a type
class or a specific instance of a general type. For example, we may have Arrays
that operate on a `Storable` or a `Prim` type.

One possible way to organize such module is to have a `Storable` or `Prim`
hierarchy and all data structures using that type constraint are bundled under
it. However, in general, a data structure may have multiple such
constraints or may have to be organized based on some other dimension
like an abstract interface it is implementing.

General purpose constraints like `Prim` can be defined in their own module
hierarchy and can be used everywhere. For example, we can have the following
Array types, here we have organized the types under the `Array` hierarchy
rather than putting the `PrimArray` under a `Prim` hierarchy.

```
Streamly.Internal.Data.Array.Boxed
Streamly.Internal.Data.Array.Prim
Streamly.Internal.Data.Array.Prim.Pinned
```

## Common Modules

Some modules represent common types or utility functions that are shared across
multiple higher level modules. Possible naming for such modules are:

* `Module.Types`
* `Module.Common`
* `Module.Core`
* `Module.Shared`

## Aggregate modules

In some cases we may want to aggregate the functionality of several small
modules in a combined aggregate module. In many cases, the aggregate module is
made a parent module of the constituent modules.  The parent module depends on
the child modules and exposes the functionality from the constituent modules.

## Placeholder Modules

In some cases a parent module is just a placeholder in the namespace and does
not export any functionality.

# Polymorphic vs Monomorphic Modules

In general we can just provide a polymorphic stream API and let the user use it
at the type he/she wants to use it. However, it has some disadvantages:

* Some constraints e.g. "MonadAsync" are unnecessarily imposed on all APIs even
  though they are needed by only concurrent types.
* type errors could be harder to resolve when using polymorphic types
* combinators like `asyncly` can make all the combinators concurrent in one go
  which is usually problematic. If we use monorphic combinators it encourages
  to pick the required concurrent combinators one at a time which is
  usually better for performance.

Keeping this in mind our plan is to provide monomoprhic modules for each stream
type, keep the combinators that are specific to that stream type in the
monomorphic module and combinators that are exactly the same for all stream
types can be kept in the polymorphic module or in both the modules. Having
complete set of operations available in the monomorphic module has the
advantage that if we want we can just import a `Serial` module and get
everything if we just want to use the Serial stream.

# Streamly Modules

We use the "Streamly" prefix to all the module names so that they do not
conflict with any other module on Hackage.

We have the following module hierarchy under Streamly:

* Data: This is a generic bucket for basic data structures a la the `base`
  package's `Data` hierarchy. 
    * Streamly.Data.Array
  
  Streams can be classified under `Data` or `Control`.  Though they are
  mostly used for processing, they can also be used to store data in
  memory.
    * Streamly.Data.Stream

  The following modules could in fact be classified under `Control`
  too as they are about processing of data rather than data itself:
    * Streamly.Data.Unfold
    * Streamly.Data.Fold
    * Streamly.Data.Parser

* Unicode: Unicode text processing:
    * Streamly.Unicode.Char       -- operations on individual chars
    * Streamly.Unicode.Stream     -- operations on streams of Char
    * Streamly.Unicode.Array.Char -- compact strings of UTF-32 chars
    * Streamly.Unicode.Array.Utf8 -- compact strings of UTF-8 encoded chars

* FileSystem: This name space is for data structures that reside in files
  provided by a file system interface on top of storage devices.

* Network: This name space is for APIs that access data from remote computers
  over the network.

## Stream modules

By default the streaming modules are effectful. The basic effectful
stream types are:

* `Streamly.Data.Stream`
* `Streamly.Data.Stream.Async`
* `Streamly.Data.Stream.Ahead`
* `Streamly.Data.Stream.Parallel`
* `Streamly.Data.Stream.IsStream` -- polymorphic operations
* `Streamly.Data.Stream.Using`    -- e.g. mapMUsing consMAsync

The above streams have an append-like multi-stream combining behavior
i.e. `concatMap` and `bind` would by default evaluate the streams one
after another. Alternative implementations of `concatMap` and `bind` are
possible. We can either use rebindable syntax to use a different bind or
define newtypes with a different bind behavior, all other operations for
these remain the same as the base type:

* `Streamly.Data.Stream.Zip`
* `Streamly.Data.Stream.Interleaved`
* `Streamly.Data.Stream.RoundRobin`
* `Streamly.Data.Stream.Async.Zip`
* `Streamly.Data.Stream.Async.Interleaved`
* `Streamly.Data.Stream.Async.RoundRobin`
* ...

Pure streams are a special case of effectful streams and have the same
interface as lists, so we put them under `Streamly.Data.List`:

* `Streamly.Data.List`
* `Streamly.Data.List.Zip`
* `Streamly.Data.List.Interleaved`
* `Streamly.Data.List.RoundRobin`
* ...

We could possibly use the same type named `Stream` for all stream
types, as the names of all stream operation are also the same and we
distinguish only by the module name.

## Array modules

Similarly, the immutable Array modules would go in:

* `Streamly.Data.Array`                  -- unpinned, native memory arrays
* `Streamly.Data.Array.Storable`         -- unpinned, unboxed, native memory arrays
* `Streamly.Data.Array.Storable.Pinned`  -- pinned, unboxed, native memory arrays
* `Streamly.Data.Array.Storable.Foreign` -- pinned, unboxed, foreign capable arrays

Unboxed arrays, based on `Prim` type class:

* `Streamly.Data.Array.Prim`
* `Streamly.Data.Array.Prim.Pinned`

Mutable arrays are a generalization of immutable arrays:

* `Streamly.Data.Array.Mut`
* `Streamly.Data.Array.Storable.Mut`
* `Streamly.Data.Array.Storable.Pinned.Mut`
* ...

## Stream and Fold Channels (SVar)

* `Streamly.Data.Stream.Channel`
* `Streamly.Data.Stream.Channel.Storable`
* `Streamly.Data.Fold.Channel`
* `Streamly.Data.Fold.Channel.Storable`

## Mutable variables

Unboxed IORef:

* `Streamly.Data.IORef.Prim`

## Strict Data

* `Streamly.Data.Tuple.Strict`
* `Streamly.Data.Maybe.Strict`
* `Streamly.Data.Either.Strict`
