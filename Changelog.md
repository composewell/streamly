## Unreleased

### Bug Fixes
* Fix Zip/AsyncZip applicative instances to handle applicative injection of function like `pure f <*> s1 <*> s2`

## 0.1.1

### Enhancements
* Make `cons` right associative and provide an operator form `.:` for it
* Add `iterate`, `null`, `tail`, `reverse`, `replicateM`, `scan` stream operations
* Improve performance of some stream operations (`foldl`, `dropWhile`)

### Bug Fixes
* Fix the `product` operation. Earlier, it always returned 0 due to a bug
* Fix the `last` operation, which returned `Nothing` for singleton streams

## 0.1.0

* Initial release
