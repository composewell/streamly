# Changelog

## Unreleased

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
