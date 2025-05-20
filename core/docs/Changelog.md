# Changelog

## Unreleased

### Breaking Changes

* In the `Streamly.Data.Parser` module, constructors `Partial`,
  `Continue`, `Done` have been deprecated and replaced by `SPartial`,
  `SContinue` and `SDone`.  Old code can be changed to use new
  constructors as follows:

  * In the step function of a parser, constructor `Partial n` should
    be replaced by `SPartial (1-n)`, `Continue n` by `SContinue (1-n)`
    and `Done n` by `SDone (1-n)`. A pattern match `SPartial n` can be
    replaced by `SPartial m -> let n = 1 - m; ... ` and so on.
  * In the extract function `Contine n` should be replaced by `Continue (-n)`
    and `Done n` by `Done (-n)`.

    If the `n` returned by these constructors is used in making some decisions,
    that will have to be modified accordingly. See the docs for more details.

* Concurrent streams and folds now use separate types for concurrency
  config. If you are importing the `Config` type from an incorrect module,
  you will have to fix that.

### Enhancements

* Add several new concurrent combinators for folds in
  `Streamly.Data.Fold.Prelude`.
* Add `Streamly.Data.Scanl` module is added for the new `Scanl` type. Composable
  scans can be used to split streams into multiple streams, process them
  independently (concurrently too) and merge the results. The `Fold` type has
  been split in two types, `Fold` and `Scanl`.
* Add a `Path` type and some type wrappers for flexibly typed file
  system paths, following modules are added:
  - Streamly.FileSystem.Path
  - Streamly.FileSystem.Path.Node
  - Streamly.FileSystem.Path.Seg
  - Streamly.FileSystem.Path.SegNode
* Add `Streamly.FileSystem.DirIO` and `Streamly.FileSystem.FileIO`
  modules. These new module replace `Streamly.FileSystem.Dir`,
  `Streamly.FileSystem.File` modules which have been deprecated.  The
  new modules have the same API as old ones except that they use
  the streamly native `Path` type instead of `FilePath` for path
  representation. The DirIO module API takes an additional ReadOptions
  argument to modify the behavior.  Please note that the directory read
  APIs in the new module do not follow symlinks by default.
* Remove the `Storable` constraint from the following functions:
  - Streamly.Data.Stream.isInfixOf
  - Streamly.Data.Array.writeLastN

### Deprecations and API changes

* `Streamly.FileSystem.Dir`, `Streamly.FileSystem.File` modules have
  been deprecated.
* Rename `writeN`-like APIs to `createOf`-like in Array modules.
* Rename `new`-like APIs to `emptyOf`-like in Array modules.
* In the Fold module `indexGeneric`, `lengthGeneric`, and `foldlM1'` to
   `genericIndex`, `genericLength`, and `foldl1M'` respectively.

See [0.2.2-0.3.0 API Changelog](/core/docs/ApiChangelogs/0.2.2-0.3.0.txt) for a
full list of deprecations, additions, and changes to the function signatures.

### Internal API Changes

* Functions in internal (mut)array modules now explicitly use
  `IO` callback instead of a lifted callback.
* The internal `FileSystem.Event.*` modules have been deprecated in favor of the
  [streamly-fsevents](https://github.com/composewell/streamly-fsevents) package.
* Remove the `Storable` constraint from several functions involving the ring
  buffer.
* `Streamly.Internal.Data.IORef` is a new module exposing `IORef` and friends.

## 0.2.2 (Jan 2024)

* Add fixities `infixr 5` for `cons` and `consM` functions.
* Fix a bug in Array `Eq` instance when the type is a sum type with
  differently sized constructors.
* lpackArraysChunksOf, compact, writeChunksWith, putChunksWith now take the
  buffer size in number of array elements instead of bytes.

## 0.2.1 (Dec 2023)

* Make the serialization of the unit constructor deterministic.
* Expose `pinnedSerialize` & `deserialize` via `Streamly.Data.Array`.

## 0.2.0 (Nov 2023)

See [0.1.0-0.2.0 API Changelog](https://github.com/composewell/streamly/blob/streamly-0.10.0/core/docs/ApiChangelogs/0.1.0-0.2.0.txt)
for a full list of API changes in this release. Only a few significant
changes are mentioned here.

### Breaking Changes

* `ParserK` in `Streamly.Data.ParserK` is not implicitly specialized
  to arrays anymore. To adapt to the new code, change `ParserK a m
  b` to `ParserK (Array a) m b` where the `Array` type comes from
  `Streamly.Data.Array`. This change also affected the signatures of
  `parseChunks` and `parseBreakChunks`.
* Changed the signature of 'Streamly.Data.Stream.handle' to make the
  exception handler monadic.
* Behavior change: Exceptions are now rethrown promptly in `bracketIO`.

### Enhancements

* __Serialization__: Added a `Streamly.Data.MutByteArray` module with a
  `Serialize` type class for fast binary serialization. The Data.Array
  module supplies the `serialize` and `deserialize` operations for arrays.
* __Unpinned Arrays__: Unboxed arrays are now created unpinned by default,
  they were created pinned earlier. During IO operations, unpinned arrays
  are automatically copied to pinned memory. When arrays are directly
  passed to IO operations programmers can choose to create them pinned to
  avoid a copy.  To create pinned arrays, use the internal APIs with the
  `pinned*` prefix.
* StreamK now supports native exception handling routines (handle, bracketIO).
  Earlier we had to convert it to the `Stream` type for exception handling.

### Deprecations

See [0.1.0-0.2.0 API Changelog](https://github.com/composewell/streamly/blob/streamly-0.10.0/core/docs/ApiChangelogs/0.1.0-0.2.0.txt)
for a full list of deprecations.

### Internal API Changes

* Fold constructor has changed, added a `final` field to support
  finalization and cleanup of a chain of folds. The `extract` field is
  now used only for mapping the fold internal state to fold result for
  scanning purposes. If your fold does not require cleanup you can just use
  your existing `extract` function as `final` as well to adapt to this change.
* Many low level internal modules have been removed, they are entirely
  exported from higher level internal modules. If you were importing any
  of the missing low level modules then import the higher level modules instead.
* Internal module changes:
  * Streamly.Internal.Serialize.FromBytes -> Streamly.Internal.Data.Binary.Parser
  * Streamly.Internal.Serialize.ToBytes ->   Streamly.Internal.Data.Binary.Stream
  * Streamly.Internal.Data.Unbox is now exported via Streamly.Internal.Data.Serialize
  * Streamly.Internal.Data.IORef.Unboxed is now exported via Streamly.Internal.Data.Serialize

## 0.1.0 (March 2023)

Also see [streamly-core-0.1.0 API Changelog](https://github.com/composewell/streamly/blob/streamly-0.10.0/core/docs/ApiChangelogs/0.1.0.txt) or
https://hackage.haskell.org/package/streamly-core-0.1.0/docs/docs/ApiChangelogs/0.1.0.txt

`streamly` package is split into two packages, (1) `streamly-core` that
has only GHC boot library depdendecies, and (2) `streamly` that contains
higher level operations (including concurrent ones) with additional
dependencies.

* Moved the following modules from `streamly` package to the
  `streamly-core` package:
  * Streamly.Console.Stdio
  * Streamly.Data.Fold
  * Streamly.Data.Unfold
  * Streamly.FileSystem.Handle
  * Streamly.Unicode.Stream
* Added the following new modules:
  * Streamly.Data.Array
  * Streamly.Data.Array.Generic
  * Streamly.Data.MutArray
  * Streamly.Data.MutArray.Generic
  * Streamly.Data.Parser
  * Streamly.Data.ParserK
  * Streamly.Data.Stream
  * Streamly.Data.StreamK
  * Streamly.FileSystem.Dir
  * Streamly.FileSystem.File
  * Streamly.Unicode.Parser
  * Streamly.Unicode.String
