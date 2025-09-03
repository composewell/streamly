# Changelog

## Unreleased

## 0.3.0

See [0.2.2-0.3.0 API Changelog](/core/docs/ApiChangelogs/0.2.2-0.3.0.txt) for a
full list of deprecations, additions, and changes to the function signatures.

### Enhancements

* Added APIs for prompt cleanup of resources, allowing guaranteed
  cleanup as an alternative to GC-based cleanup.
* Added operations for fair nesting of inner and outer streams for
  exploring them equally, generally useful but especially useful for logic
  programming use cases.
* Introduced `Streamly.Data.Scanl` with a new `Scanl` type. Scans can
  split a stream into multiple streams, process them independently, and
  merge the results. The `Fold` type is now split into `Fold` and `Scanl`.
* Added `RingArray` module for high-performance, unboxed circular buffers.
* Added `Streamly.FileSystem.Path` module with a `Path` type for flexibly typed
  file system paths.
* Added `Streamly.FileSystem.DirIO` and `Streamly.FileSystem.FileIO` to replace
  the deprecated `Streamly.FileSystem.Dir` and `Streamly.FileSystem.File`. The
  new modules use Streamly’s native `Path` type instead of `FilePath`. `DirIO`
  APIs take a `ReadOptions` argument, and its directory read APIs do not follow
  symlinks by default.
* Removed `Storable` constraint from:
  - `Streamly.Data.Stream.isInfixOf`
  - `Streamly.Data.Array.writeLastN`

### Deprecations

Following APIs/modules are deprecated and renamed or replaced with new
APIs.

* `Streamly.FileSystem.Dir`, `Streamly.FileSystem.File` have been replaced by
  new modules.
* Renamed `writeN`-like APIs to `createOf`-like in Array modules.
* Renamed `new`-like APIs to `emptyOf`-like in Array modules.
* In the Fold module renamed `indexGeneric`, `lengthGeneric`, and `foldlM1'` to
  `genericIndex`, `genericLength`, and `foldl1M'` respectively.

### Internal API Changes

* In `Streamly.Internal.Data.Parser`, constructors `Partial`, `Continue`, and
  `Done` are deprecated and replaced with `SPartial`, `SContinue`, and `SDone`.
  Migration steps:
  * In parser step functions:
    - `Partial n` -> `SPartial (1-n)`
    - `Continue n` -> `SContinue (1-n)`
    - `Done n` -> `SDone (1-n)`
    - `Error` -> `SError`
  * Extract function now returns `Parser.Final` (instead of `Parser.Step`):
    - `Continue n` -> `FContinue (-n)`
    - `Done n` -> `FDone (-n)`
    - `Partial n` -> `FContinue (-n)`
    - `Error` -> `FError`
  * If `n` is used for decision-making, the logic must be updated accordingly.
    See docs for details.
* Internal (mut)array functions now use explicit IO callbacks instead of lifted
  callbacks.
* Removed `Storable` constraint from several ring buffer functions.
* Added `Streamly.Internal.Data.IORef` module exposing `IORef` and related
  functions.

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
