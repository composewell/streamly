# Internal vs External

We keep all modules exposed to faciliate convenient exposure of experimental
APIs and constructors to users. It allows users of the library to experiment
much more easily and carry a caveat that these APIs can change in future
without notice.  Since everything is exposed, maintainers do not have to think
about what to expose as experimental and what remains completely hidden every
time something is added to the library.

We expose the internal modules via `Streamly.Internal` namespace to keep all
the internal modules together under one module tree and to have their
documentation also separated under one head in haddock docs.

Another decision point is about two choices:

1) Keep the implementation of all the APIs in an internal module and just
reexport selected APIs through the external module. The disadvantages of this
are:
a) users may not be able to easily figure out what unexposed APIs are available
other than the ones exposed through the external module. To avoid this problem
we can mark the unexposed APIs in the docs with a special comment.
b) In tests and benchmarks we will be using internal modules to test internal
and unexposed APIs. Since exposed APIs are exported via both internal and
external modules we will have to be careful in not using the internal module
for testing accidentally, instead we should always be using the exposed module
so that we are always testing exactly the way users will be using the APIs.

2) Keep the implementations of unexposed modules in the internal module file
and exposed module in the external module file. In this approach, users can
easily figure out the unexposed vs exposed APIs. But maintaining this would
require us to move the APIs from internal to external module file whenever we
expose an API.

We choose the first approach.

# Name Spaces

We use the "Streamly" prefix to all the module names so that they do not
conflict with any other module on Hackage.

We have the following division under Streamly:

* Data: All the data structures that are immutable and make use of the GC
  memory to store data. These data structures are mainly useful for stream
  processing and not for storing data in memory for longer durations.

* Memory: This name space is for data structures that make use of the memory as
  a storage device. This is not GC allocated memory but rather pinned memory.
  The memory in this case is treated just like any other storage device e.g.
  disk drives, but faster. The data is serialized into memory. Unlike the data
  structure under "Data" these data structures have no impact on GC. Many of
  the data structures in this category are parallel to the data structures
  under "FileSystem".

* FileSystem: This name space is for data structures that reside in files
  provided by a file system interface on top of storage devices.

* Network: This name space is for APIs that access data from remote computers
  over the network.
