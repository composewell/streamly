## Unreleased

### Breaking changes
* Change the type of `foldrM` to make it consistent with `foldrM` in base

### Deprecations
* Deprecate `StreamT`, rename to `SerialT`
* Deprecate `runStreamT`, rename to `runSerialT`
* Deprecate `ZipStream`, rename to `ZipSerial`
* Deprecate `runZipStream`, rename to `runZipSerial`

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
