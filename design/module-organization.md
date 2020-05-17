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

Concrete/monomorphic pure stream modules would be placed in:

* `Streamly.Data.Stream.Serial`
* `Streamly.Data.Stream.WSerial`
* `Streamly.Data.Stream.ZipSerial`
* ...

Monadic/effectful streams could go in:

* `Streamly.Data.StreamM.Serial`
* `Streamly.Data.StreamM.WSerial`
* `Streamly.Data.StreamM.Async`
* ...

Pure streams are just a special case of monadic streams.

We could possibly use the same type named `Stream` for all stream types, as we
also have all the operation names also same and we distinguish only by the
module name.

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

Streamly.Internal.Data.ArrayM.Boxed
Streamly.Internal.Data.ArrayM.Prim
Streamly.Internal.Data.ArrayM.Prim.Pinned
```

Pure arrays are just a special case of mutable arrays.

We use the name `ArrayM` insted of `MArray` consistent with the naming
of monadic operations like `mapM` and also because `Array` and `ArrayM`
are listed together in alphabetical listing, plus camel case naming of the
latter is clearer to read.

We could use an `IsArray` type class, like `IsStream`, but it will
require the `Prim` constraint on all polymorphic operations, rendering
it of little use. So the `Array` module, unlike in streams, is just a
name space placeholder here. We could assign the `Array` module to the
Boxed array module but, having an explicit `Boxed` module along with
other types of arrays keeps naming explicit and clearer.

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

* Unicode: Unicode text processing:
    * Streamly.Unicode.Char
    * Streamly.Unicode.Stream
    * Streamly.Unicode.Array

* FileSystem: This name space is for data structures that reside in files
  provided by a file system interface on top of storage devices.

* Network: This name space is for APIs that access data from remote computers
  over the network.
