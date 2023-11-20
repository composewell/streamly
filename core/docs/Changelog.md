# Changelog

## Unreleased

* Arrays are now created unpinned by default, they were created pinned
  earlier. During IO operations unpinned arrays are automatically copied
  to pinned memory. When arrays are directly passed to IO operations
  programmers can choose to create them pinned to avoid a copy.  To
  create pinned arrays, use the internal APIs with the `pinned*` prefix.

* Changed the signature of 'Streamly.Data.Stream.handle' to make the exception
  handler monadic.
* Rethrow the exception promptly in bracketIO.

### Breaking

* `ParserK` in `Streamly.Data.ParserK` is not specialized to arrays anymore. To
  adapt to the new code, change `ParserK a m b` to `ParserK (Array a) m b` where
  `Array` comes from `Streamly.Data.Array`.
* Internal module changes:
  * Streamly.Internal.Serialize.FromBytes -> Streamly.Internal.Data.Serialize.Parser
  * Streamly.Internal.Serialize.ToBytes ->   Streamly.Internal.Data.Serialize.Stream
  * Streamly.Internal.Data.Unbox is now exported via Streamly.Internal.Data.Serialize
  * Streamly.Internal.Data.IORef.Unboxed is now exported via Streamly.Internal.Data.Serialize

### Deprecations

* `Streamly.Data.MutArray.newUnpinned` is renamed to
  `Streamly.Data.MutArray.pinnedNew`

## 0.1.0 (March 2023)

Also see [streamly-core-0.1.0 API Changelog](/core/docs/ApiChangelogs/0.1.0.txt) or
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
