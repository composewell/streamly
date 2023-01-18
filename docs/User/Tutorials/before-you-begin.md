<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# Before You Begin

In this tutorial, we assume basic knowledge of Haskell syntax, constructs and
lazy evaluation. The [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell)
may be a good place to start and get familiar with Haskell.

If you wish to follow along and run the examples in this tutorial, you will
need to have [Streamly][] installed.  Please see the [Setup and
Usage](/docs/User/Tutorials/setup-and-usage.md) guide for instructions
on how to install and use [Streamly][].

If you wish to run benchmarks, please be sure to build your
application using the instructions in the [Build Guide](/docs/User/HowTo/Compiling.md).

## Streamly Library Packages

Streamly comprises two packages, the
[streamly-core](https://hackage.haskell.org/package/streamly-core)
package provides functionality that depends only on boot libraries, and
the [streamly](https://hackage.haskell.org/package/streamly) package
provides additional functionality like concurrency, time, lifted
exceptions, and networking.

For high-level functionality built over streamly like streaming
system processes, shell programming, GNU coreutils, statistics,
and compression libraries please see the [streamly ecosystem
packages](https://streamly.composewell.com/ecosystem.html).

## Released and Pre-release modules

Some of the examples in this tutorial may use modules from the
`Internal` Streamly module hierarchy.  These modules are not really
internal to the library.  We classify `Streamly` modules into two
categories:

* _Released modules and APIs_: These modules and APIs are
  stable. Significant changes to these modules and APIs will cause
  Streamly's version number to change according to the package versioning
  policy.
* _Pre-release modules and APIs_: These modules and APIs have not been
  formally released yet.  They may change in the near future, and such
  changes will not necessarily be reflected in Streamly's package
  version number.  As yet unreleased modules and APIs reside in the
  `Internal` namespace.

Please use a minor release upper bound to adhere to the Haskell PVP when
using a pre-release (internal) module.

<!--
Link References.
-->

[Streamly]: https://streamly.composewell.com/
