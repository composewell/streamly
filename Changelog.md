## Unreleased

### Enhancements
* Add the `scan` operation for left scan of a stream
* Improve performance of some stream operations (`foldl`, `dropWhile`)
* Add `mapM`, `mapM_` and `sequence_` operation.

### Bug Fixes
* Fix the `product` operation. Earlier, it always returned 0 due to a bug
* Fix the `last` operation, which returned `Nothing` for singleton streams

## 0.1.0

* Initial release
