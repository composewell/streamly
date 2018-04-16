## Unreleased

### Breaking changes
* Change the semantics of the Semigroup instance for `InterleavedT`, `AsyncT`
  and `ParallelT`. Now the `<>` operation interleaves two streams for
  `InterleavedT` (just like the now deprecated `<=>` operation which has been
  renamed to `interleave`). For `AsyncT` `<>` now concurrently merges two
  streams (just like the now deprecated `<|` operation which has been renamed
  to `asyncmerge`). For `ParallelT` the `<>` operation now behaves like the
  earlier `Alternative` operator `<|>`.
* Change the semantics of `Alternative` instance. The `<|>` operator now has a
  different behavior for each type. See the documentation for more details.
* Change the type of `foldrM` to make it consistent with `foldrM` in base

### Deprecations
* Deprecate and rename the following symbols:
    * `StreamT` to `SerialT`
    * `runStreamT` to `runSerialT`
    * `ZipStream` to `ZipSerial`
    * `runZipStream` to `runZipSerial`
    * `Streaming` to `IsStream`
    * `runStreaming` to `runStream`
    * `<=>` to `interleave`
    * `<|` to `asyncmerge`
    * `each` to `fromFoldable`
    * `scan` to `scanx`
    * `foldl` to `foldx`
    * `foldlM` to `foldxM`

### Enhancements
* Add the following functions:
    * `scanl'` strict left scan
    * `foldl'` strict left fold
    * `foldlM'` strict left fold with a monadic fold function

## 0.1.2

### Enhancements
* Add `iterate`, `iterateM` stream operations

### Bug Fixes
* Fixed a bug that casued unexpected behavior when `pure` was used to inject
  values in Applicative composition of `ZipStream` and `ZipAsync` types.

## 0.1.1

### Enhancements
* Make `cons` right associative and provide an operator form `.:` for it
* Add `null`, `tail`, `reverse`, `replicateM`, `scan` stream operations
* Improve performance of some stream operations (`foldl`, `dropWhile`)

### Bug Fixes
* Fix the `product` operation. Earlier, it always returned 0 due to a bug
* Fix the `last` operation, which returned `Nothing` for singleton streams

## 0.1.0

* Initial release
