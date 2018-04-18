## Unreleased

### Breaking changes
* Change the semantics of the Semigroup instance for `InterleavedT`, `AsyncT`
  and `ParallelT`. The new semantics are as follows:
  * For `InterleavedT`, `<>` operation interleaves two streams
  * For `AsyncT`, `<>` now concurrently merges two streams in a left biased
    manner using demand based concurrency.
  * For `ParallelT`, the `<>` operation now concurrently meges the two streams
    in a fairly parallel manner.

  To adapt to the new changes, replace `<>` with `append` wherever it is used
  for streams other than `StreamT`.

* Change the semantics of `Alternative` instance. The `<|>` operator now has a
  different behavior for each type. See the documentation for more details. To
  adapt to this change replace any usage of `<|>` with `parallel` and
  `empty` with `nil`.
* Change the type of `foldrM` to make it consistent with `foldrM` in base.

### Deprecations
* Deprecate and rename the following symbols:
    * `StreamT` to `SerialT`
    * `runStreamT` to `runSerialT`
    * `ZipStream` to `ZipSerial`
    * `runZipStream` to `runZipSerial`
    * `AsyncT` to `AParallelT`
    * `runAsyncT` to `runAParallelT`
    * `asyncly` to `aparallely`
    * `Streaming` to `IsStream`
    * `runStreaming` to `runStream`
    * `<=>` to `interleave`
    * `<|` to `aparallel`
    * `each` to `fromFoldable`
    * `scan` to `scanx`
    * `foldl` to `foldx`
    * `foldlM` to `foldxM`

### Enhancements
* Add the following functions:
    * `scanl'` strict left scan
    * `foldl'` strict left fold
    * `foldlM'` strict left fold with a monadic fold function
    * `append` run two streams serially one after the other
    * `parallel` run two streams in parallel

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
