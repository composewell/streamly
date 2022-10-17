# Upgrading

## From 0.8 to 0.9

### Major changes

- `Streamly.Prelude` is deprecated.
- The concurrency model has changed.

### `Streamly.Prelude`

`Streamly.Prelude` is now split into multiple modules, namely,

- `Streamly.Data.Stream`
- `Streamly.Data.Fold`
- `Streamly.Data.Stream.Concurrent`

Use the following imports where necessary,

```
import Streamly.Data.Stream.Concurrent (Config)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur
```

Elimination operations of the form `... -> Stream m a -> m b` can be expressed
in terms of folds.

For example, `Streamly.Prelude.(!!)` can be expressed using `Fold.index`,
```
(!!) i = Stream.fold (Fold.index i)
```

Transformation operations of the form `... -> Stream m a -> Stream m b` that are
reducing in nature (grouping) can be implemented using folds or parsers on
groups made by some predicate. Essentially applying the fold or parser many
times. This can be achieved by using `Stream.foldMany`, `Stream.foldManyPost`, &
`Stream.parseMany`.

For example, the operations from `Streamly.Prelude` can be expressed as follows,
```
groupsBy eq f = Stream.parseMany (Parser.groupsBy eq f)
splitOn predicate f = foldManyPost (FL.takeEndBy_ predicate f)
splitOnSuffix p f = Stream.foldMany (Fold.takeEndBy_ p f)
```

See the end of this document for the complete list of alternatives.

### Concurrency model

The types that represented concurrency earlier, namely, `SerialT`, `AsyncT`,
`AheadT`, `WAheadT`, `WAsyncT`, `ParallelT` are now deprecated.

The essential type is now `Stream`. `Stream` can be evaluated concurrently or
multiple `Stream`s can be combined concurrently using respective combinators.

| Type        | Alternative         |
|-------------|---------------------|
| `SerialT`   | `Stream.append`     |
| `WSerialT`  | `Stream.interleave` |
| `AsyncT`    | `Concur.append`     |
| `WAsyncT`   | `Concur.interleave` |
| `AheadT`    | `Concur.ahead`      |
| `ParallelT` | `Concur.parallel`   |

Concurrency is now explicitly expressed in terms of configuration modifiers
(`Config -> Config`).

| Type      | Alternative                                                   |
|-----------|---------------------------------------------------------------|
| `WAheadT` | `Concur.concatListWith (Concur.ordered . Concur.interleaved)` |

`Concur.append`, `Concur.ahead`, etc. are implemented in terms of
`Concur.concatListWith` and appropriate configuration modifier.

### Alternatives to removed ops in `Streamly.Prelude`

The following provide enough information for an alternative implementation.

* IsStream: Abandoned
* ZipSerialM: `Streamly.Data.Stream.Zip.ZipStream`
* ZipAsyncM: `Streamly.Data.Stream.Zip.Concurrent.ZipConcurrent`
* WSerialT: `Stream.interleave`
* WAsyncT: `Concur.append`
* ParallelT: `Concur.parallel`
* SerialT: `Stream.append`
* Rate: `Concur.Rate`
* AsyncT: `Concur.append`
* AheadT: `Concur.ahead`
* WAheadT: `Concur.concatListWith (Concur.ordered . Concur.interleaved`
* (|:): Abandoned
* (|$.): `(|$.) f = f . Concur.evalWith Concur.eager`
* (|&.): `x |&. f = f |$. x`
* (|$): `(|$) f = f . Concur.evalWith Concur.eager`
* (|&): `x |& f = f |$ x`
* (.:): `Stream.cons`
* zipAsyncWithM: `Concur.zipWithM`
* zipAsyncWith: `Concur.zipWith`
* wordsBy: `wordsBy p f = Stream.foldMany (Fold.wordBy p f)`
* wSerial: `Stream.interleave`
* wAsync: `Concur.interleave`
* trace: `trace f = mapM (\x -> void (f x) >> return x)`
* toList: `toList = Stream.fold Fold.toList`
* uniq: `Stream.catMaybes $ Stream.foldMany Fold.uniq`
* the: `Stream.fold Fold.the`
* tail: `tail = snd . Stream.foldBreak Fold.one`
* sum: `sum = Stream.fold Fold.sum`
* splitWithSuffix: `sum = Stream.fold Fold.sum`
* splitOnSuffix: `splitOnSuffix p f = Stream.foldMany (Fold.takeEndBy_ p f)`
* splitWithSuffix: `splitWithSuffix p f = Stream.foldMany (Fold.takeEndBy p f)`
* splitOn: `splitOn predicate f = foldManyPost (FL.takeEndBy_ predicate f)`
* serial: `Stream.append`
* scanlM': Use `Stream.scan` and `Stream.scanMany` accordingly
* scanl1M': Use `Stream.scan` and `Stream.scanMany` accordingly
* scanl1': Use `Stream.scan` and `Stream.scanMany` accordingly
* scanl: Use `Stream.scan` and `Stream.scanMany` accordingly
* replicateM: `replicateM n = Stream.sequence . Stream.replicate n`
* repeatM: `repeatM n = Stream.sequence . Stream.repeat n`
* rate: Config combinator now. Use this to create the config and then use
  `Concur.evalWith`.
* product: `sum = Stream.fold Fold.product`
* postscanlM': Use `Stream.postScan` instead
* postscanl': Use `Stream.postScan` instead
* or: `or = Stream.fold Fold.or`
* null: `null = Stream.fold Fold.null`
* notElem: `notElem = Stream.fold Fold.notElem`
* mkAsync: `Concur.eval`
* minumumBy: `minumumBy = Stream.fold Fold.minumumBy`
* minumum: `minumum = Stream.fold Fold.minumum`
* minRate: Config combinator now. Use this to create the config and
  then use `Concur.evalWith`.
* mergeAsyncByM: `mergeAsyncByM f m1 m2 = Stream.mergeByM f (Concur.evalWith Concur.eager m1) (Concur.evalWith Concur.eager m2)`
* mergeAsyncBy: `mergeAsyncBy = mergeAsyncByM (\a b -> return $ f a b)`
* maximumBy:  `maximumBy = Stream.fold Fold.maximumBy`
* maximum:  `maximum = Stream.fold Fold.maximum`
* maxThreads: Config combinator now. Use this to create the config and then use
  `Concur.evalWith`.
* maxRate: Config combinator now. Use this to create the config and then use
  `Concur.evalWith`.
* maxBuffer: Config combinator now. Use this to create the config and then use
  `Concur.evalWith`.
* `mapM_`: `mapM_ f = Stream.fold (Fold.drainBy f)`
* map: Use fmap instead
* lookup: `lookup = Stream.fold Fold.lookup`
* length: `length = Stream.fold Fold.length`
* last: `last = Stream.fold Fold.last`
* intervalsOf: `Concur.intervalsOf`
* init: Abandoned
* head: `head = Stream.fold Fold.one`
* groupsByRolling: `groupsByRolling eq f = Stream.parseMany
  (Parser.groupByRolling eq f)`
* groupsBy: `groupsBy eq f = Stream.parseMany (Parser.groupsBy eq f)`
* groups:  `groups f = groupsBy (==)`
* fromZipSerial: Abandoned
* fromZipAsync: Abandoned
* fromWSerial: Abandoned
* fromWAsync: Abandoned
* fromSerial: Abandoned
* fromParallel: Abandoned
* fromListM: `fromListM = Stream.sequence . Stream.fromList`
* fromIndicesM: `fromIndicesM f = Stream.mapM f $ Stream.enumerateFrom 0`
* fromIndices: `fromIndices f = fmap f $ Stream.enumerateFrom 0`
* fromFoldableM: `fromFoldableM = Prelude.foldr Stream.consM Stream.nil`
* fromAsync: Abandoned
* fromAhead: Abandoned
* foldxM: Use `Stream.fold` and folds (`Fold`) accordingly.
* foldx: Use `Stream.fold` and folds (`Fold`) accordingly.
* foldrM: Use `Stream.fold` and folds (`Fold`) accordingly.
* foldr1: Use `Stream.fold` and folds (`Fold`) accordingly.
* foldr: Use `Stream.fold Fold.foldr`
* findIndex: `findIndex = Stream.fold Fold.findIndex`
* find: `find = Stream.fold Fold.find`
* elemIndex: `elemIndex = Stream.fold Fold.elemIndex`
* elem: `elem = Stream.fold Fold.elem`
* drainWhile: `drainWhile p = Stream.drain . Stream.takeWhile p`
* drainN: `drainN n = Stream.fold Fold.drain . Stream.take n`
* drain: `drain = Stream.fold Fold.drain`
* constRate: Config combinator now. Use this to create the config and then use
  `Concur.evalWith`.
* concatMapFoldableWith: `concatMapFoldableWith f g = Prelude.foldr (f . g) nil`
* concatForFoldableWith: `concatForFoldableWith f = flip (concatMapFoldableWith
  f)`
* concatFoldableWith: `concatFoldableWith f = concatMapFoldableWith f id`
* chunksOf: `chunksOf n f = foldMany (FL.take n f)`
* avgRate: Config combinator now. Use this to create the config and then use
  `Concur.evalWith`.
* async: `Concur.async`
* any: `any = Stream.fold Fold.any`
* and: `and = Stream.fold Fold.and`
* all: `all = Stream.fold Fold.all`
* adapt: Abandoned
* (!!): `(!!) i = Stream.fold (Fold.index i)`
