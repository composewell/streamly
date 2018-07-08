## Unreleased

### Bug Fixes

* Fix a bug in concurrent function application that in certain cases would
  unnecessarily share the concurrency state resulting in incorrect output
  stream.

### Enhancements

* Added `maxRate` combinator to control the yield rate of a stream.
* Add `foldl1'`, `foldr1`, `intersperseM`, `find`, `lookup`, `and`, `or`,
  `findIndices`, `findIndex`, `elemIndices`, `elemIndex`, `init` to Prelude

## 0.4.1

### Bug Fixes

* foldxM was not fully strict, fixed.

## 0.4.0

### Breaking changes

* Signatures of `zipWithM` and `zipAsyncWithM` have changed
* Some functions in prelude now require an additional `Monad` constraint on
  the underlying type of the stream.

### Deprecations

* `once` has been deprecated and renamed to `yieldM`

### Enhancements

* Add concurrency control primitives `maxThreads` and `maxBuffer`.
* Concurrency of a stream with bounded concurrency when used with `take` is now
  limited by the number elements demanded by `take`.
* Significant performance improvements utilizing stream fusion optimizations.
* Add `yield` to construct a singleton stream from a pure value
* Add `repeat` to generate an infinite stream by repeating a pure value
* Add `fromList` and `fromListM` to generate streams from lists, faster than
  `fromFoldable` and `fromFoldableM`
* Add `map` as a synonym of fmap
* Add `scanlM'`, the monadic version of scanl'
* Add `takeWhileM` and `dropWhileM`
* Add `filterM`

## 0.3.0

### Breaking changes

* Some prelude functions, to whom concurrency capability has been added, will
  now require a `MonadAsync` constraint.

### Bug Fixes

* Fixed a race due to which, in a rare case, we might block indefinitely on
  an MVar due to a lost wakeup.
* Fixed an issue in adaptive concurrency. The issue caused us to stop creating
  more worker threads in some cases due to a race. This bug would not cause any
  functional issue but may reduce concurrency in some cases.

### Enhancements
* Added a concurrent lookahead stream type `Ahead`
* Added `fromFoldableM` API that creates a stream from a container of monadic
  actions
* Monadic stream generation functions `consM`, `|:`, `unfoldrM`, `replicateM`,
  `repeatM`, `iterateM` and `fromFoldableM` can now generate streams
  concurrently when used with concurrent stream types.
* Monad transformation functions `mapM` and `sequence` can now map actions
  concurrently when used at appropriate stream types.
* Added concurrent function application operators to run stages of a
  stream processing function application pipeline concurrently.
* Added `mapMaybe` and `mapMaybeM`.

## 0.2.1

### Bug Fixes
* Fixed a bug that caused some transformation ops to return incorrect results
  when used with concurrent streams. The affected ops are `take`, `filter`,
  `takeWhile`, `drop`, `dropWhile`, and `reverse`.

## 0.2.0

### Breaking changes
* Changed the semantics of the Semigroup instance for `InterleavedT`, `AsyncT`
  and `ParallelT`. The new semantics are as follows:
  * For `InterleavedT`, `<>` operation interleaves two streams
  * For `AsyncT`, `<>` now concurrently merges two streams in a left biased
    manner using demand based concurrency.
  * For `ParallelT`, the `<>` operation now concurrently meges the two streams
    in a fairly parallel manner.

  To adapt to the new changes, replace `<>` with `serial` wherever it is used
  for stream types other than `StreamT`.

* Remove the `Alternative` instance.  To adapt to this change replace any usage
  of `<|>` with `parallel` and `empty` with `nil`.
* Stream type now defaults to the `SerialT` type unless explicitly specified
  using a type combinator or a monomorphic type.  This change reduces puzzling
  type errors for beginners. It includes the following two changes:
  * Change the type of all stream elimination functions to use `SerialT`
    instead of a polymorphic type. This makes sure that the stream type is
    always fixed at all exits.
  * Change the type combinators (e.g. `parallely`) to only fix the argument
    stream type and the output stream type remains polymorphic.

  Stream types may have to be changed or type combinators may have to be added
  or removed to adapt to this change.
* Change the type of `foldrM` to make it consistent with `foldrM` in base.
* `async` is renamed to `mkAsync` and `async` is now a new API with a different
  meaning.
* `ZipAsync` is renamed to `ZipAsyncM` and `ZipAsync` is now ZipAsyncM
  specialized to the IO Monad.
* Remove the `MonadError` instance as it was not working correctly for
  parallel compositions. Use `MonadThrow` instead for error propagation.
* Remove Num/Fractional/Floating instances as they are not very useful. Use
  `fmap` and `liftA2` instead.

### Deprecations
* Deprecate and rename the following symbols:
    * `Streaming` to `IsStream`
    * `runStreaming` to `runStream`
    * `StreamT` to `SerialT`
    * `InterleavedT` to `WSerialT`
    * `ZipStream` to `ZipSerialM`
    * `ZipAsync` to `ZipAsyncM`
    * `interleaving` to `wSerially`
    * `zipping` to `zipSerially`
    * `zippingAsync` to `zipAsyncly`
    * `<=>` to `wSerial`
    * `<|` to `async`
    * `each` to `fromFoldable`
    * `scan` to `scanx`
    * `foldl` to `foldx`
    * `foldlM` to `foldxM`
* Deprecate the following symbols for future removal:
    * `runStreamT`
    * `runInterleavedT`
    * `runAsyncT`
    * `runParallelT`
    * `runZipStream`
    * `runZipAsync`

### Enhancements
* Add the following functions:
    * `consM` and `|:` operator to construct streams from monadic actions
    * `once` to create a singleton stream from a monadic action
    * `repeatM` to construct a stream by repeating a monadic action
    * `scanl'` strict left scan
    * `foldl'` strict left fold
    * `foldlM'` strict left fold with a monadic fold function
    * `serial` run two streams serially one after the other
    * `async` run two streams asynchronously
    * `parallel` run two streams in parallel (replaces `<|>`)
    * `WAsyncT` stream type for BFS version of `AsyncT` composition
* Add simpler stream types that are specialized to the IO monad
* Put a bound (1500) on the output buffer used for asynchronous tasks
* Put a limit (1500) on the number of threads used for Async and WAsync types

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
