-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Monoid (Last(..))
import System.Random (randomRIO)

import qualified GHC.Exts as GHC
import qualified Streamly.Benchmark.Prelude as Ops

import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Sink as Sink

import Streamly.Internal.Data.Time.Units
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Internal.Data.Pipe as Pipe

import qualified NestedOps as Nested
import qualified NestedUnfoldOps as NestedUnfold

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source value

{-# INLINE benchHoistSink #-}
benchHoistSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> IO b) -> Benchmark
benchHoistSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f .  Ops.sourceUnfoldr value

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (IsStream t, NFData b)
    => Int -> String -> (t Identity Int -> Identity b) -> Benchmark
benchIdentitySink value name f = bench name $ nf (f . Ops.sourceUnfoldr value) 1

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= Ops.toNull t . f

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => Int -> String -> (SerialT Identity Int -> b) -> Benchmark
benchPureSink value name f = benchPure name (Ops.sourceUnfoldr value) f

benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches
        [ bgroup "serially"
          [ bgroup "pure"
            [ benchPureSink value "id" id
            , benchPureSink1 "eqBy" (Ops.eqByPure value)
            , benchPureSink value "==" Ops.eqInstance
            , benchPureSink value "/=" Ops.eqInstanceNotEq
            , benchPureSink1 "cmpBy" (Ops.cmpByPure value)
            , benchPureSink value "<" Ops.ordInstance
            , benchPureSink value "min" Ops.ordInstanceMin
            , benchPureSrc "IsList.fromList" (Ops.sourceIsList value)
            -- length is used to check for foldr/build fusion
            , benchPureSink value "length . IsList.toList" (length . GHC.toList)
            , benchPureSrc "IsString.fromString" (Ops.sourceIsString value)
            , benchPureSink value "showsPrec pure streams" Ops.showInstance
            , benchPureSink value "foldl'" Ops.pureFoldl'
            ]
          , bgroup "foldable"
            [ -- Foldable instance
              -- type class operations
              bench "foldl'" $ nf (Ops.foldableFoldl' value) 1
            , bench "foldrElem" $ nf (Ops.foldableFoldrElem value) 1
            -- , bench "null" $ nf (Ops.foldableNull value) 1
            , bench "elem" $ nf (Ops.foldableElem value) 1
            , bench "length" $ nf (Ops.foldableLength value) 1
            , bench "sum" $ nf (Ops.foldableSum value) 1
            , bench "product" $ nf (Ops.foldableProduct value) 1
            , bench "minimum" $ nf (Ops.foldableMin value) 1
            , bench "maximum" $ nf (Ops.foldableMax value) 1
            , bench "length . toList" $
                nf (length . Ops.foldableToList value) 1

            -- folds
            , bench "notElem" $ nf (Ops.foldableNotElem value) 1
            , bench "find" $ nf (Ops.foldableFind value) 1
            , bench "all" $ nf (Ops.foldableAll value) 1
            , bench "any" $ nf (Ops.foldableAny value) 1
            , bench "and" $ nf (Ops.foldableAnd value) 1
            , bench "or" $ nf (Ops.foldableOr value) 1

            -- Note: minimumBy/maximumBy do not work in constant memory they are in
            -- the O(n) group of benchmarks down below in this file.

            -- Applicative and Traversable operations
            -- TBD: traverse_
            , benchIOSink1 "mapM_" (Ops.foldableMapM_ value)
            -- TBD: for_
            -- TBD: forM_
            , benchIOSink1 "sequence_" (Ops.foldableSequence_ value)
            -- TBD: sequenceA_
            -- TBD: asum
            -- , benchIOSink1 "msum" (Ops.foldableMsum value)
            ]
          , bgroup "generation"
            [ -- Most basic, barely stream continuations running
              benchIOSrc serially "unfoldr" (Ops.sourceUnfoldr value)
            , benchIOSrc serially "unfoldrM" (Ops.sourceUnfoldrM value)
            , benchIOSrc serially "intFromTo" (Ops.sourceIntFromTo value)
            , benchIOSrc serially "intFromThenTo" (Ops.sourceIntFromThenTo value)
            , benchIOSrc serially "integerFromStep" (Ops.sourceIntegerFromStep value)
            , benchIOSrc serially "fracFromThenTo" (Ops.sourceFracFromThenTo value)
            , benchIOSrc serially "fracFromTo" (Ops.sourceFracFromTo value)
            , benchIOSrc serially "fromList" (Ops.sourceFromList value)
            , benchIOSrc serially "fromListM" (Ops.sourceFromListM value)
            -- These are essentially cons and consM
            , benchIOSrc serially "fromFoldable" (Ops.sourceFromFoldable value)
            , benchIOSrc serially "fromFoldableM" (Ops.sourceFromFoldableM value)
            , benchIOSrc serially "currentTime/0.00001s"
                $ Ops.currentTime value 0.00001
            ]
          , bgroup "elimination"
            [ bgroup "reduce"
              [ bgroup "IO"
                [
                  benchIOSink value "foldl'" Ops.foldl'Reduce
                , benchIOSink value "foldl1'" Ops.foldl1'Reduce
                , benchIOSink value "foldlM'" Ops.foldlM'Reduce
                ]
              , bgroup "Identity"
                [
                  benchIdentitySink value "foldl'" Ops.foldl'Reduce
                , benchIdentitySink value "foldl1'" Ops.foldl1'Reduce
                , benchIdentitySink value "foldlM'" Ops.foldlM'Reduce
                ]
              ]

            , bgroup "build"
              [ bgroup "IO"
                [ benchIOSink value "foldrMElem" (Ops.foldrMElem value)
                ]
              , bgroup "Identity"
                [ benchIdentitySink value "foldrMElem" (Ops.foldrMElem value)
                , benchIdentitySink value "foldrMToStreamLength"
                    (S.length . runIdentity . Ops.foldrMToStream)
                , benchPureSink value "foldrMToListLength"
                    (length . runIdentity . Ops.foldrMBuild)
                ]
              ]
            , benchIOSink value "uncons" Ops.uncons
            , benchIOSink value "toNull" $ Ops.toNull serially
            , benchIOSink value "mapM_" Ops.mapM_

            , benchIOSink value "init" Ops.init

            -- this is too low and causes all benchmarks reported in ns
            -- , benchIOSink value "head" Ops.head
            , benchIOSink value "last" Ops.last
            -- , benchIOSink value "lookup" Ops.lookup
            , benchIOSink value "find" (Ops.find value)
            , benchIOSink value "findIndex" (Ops.findIndex value)
            , benchIOSink value "elemIndex" (Ops.elemIndex value)

            -- this is too low and causes all benchmarks reported in ns
            -- , benchIOSink value "null" Ops.null
            , benchIOSink value "elem" (Ops.elem value)
            , benchIOSink value "notElem" (Ops.notElem value)
            , benchIOSink value "all" (Ops.all value)
            , benchIOSink value "any" (Ops.any value)
            , benchIOSink value "and" (Ops.and value)
            , benchIOSink value "or" (Ops.or value)

            , benchIOSink value "length" Ops.length
            , benchHoistSink value "length . generally" (Ops.length . IP.generally)
            , benchIOSink value "sum" Ops.sum
            , benchIOSink value "product" Ops.product

            , benchIOSink value "maximumBy" Ops.maximumBy
            , benchIOSink value "maximum" Ops.maximum
            , benchIOSink value "minimumBy" Ops.minimumBy
            , benchIOSink value "minimum" Ops.minimum

            ]
          , bgroup "folds"
            [ benchIOSink value "drain" (S.fold FL.drain)
            , benchIOSink value "drainN" (S.fold (IFL.drainN value))
            , benchIOSink value "drainWhileTrue" (S.fold (IFL.drainWhile $ (<=) (value + 1)))
            , benchIOSink value "drainWhileFalse" (S.fold (IFL.drainWhile $ (>=) (value + 1)))
            , benchIOSink value "sink" (S.fold $ Sink.toFold Sink.drain)
            , benchIOSink value "last" (S.fold FL.last)
            , benchIOSink value "lastN.1" (S.fold (IA.lastN 1))
            , benchIOSink value "lastN.10" (S.fold (IA.lastN 10))
            , benchIOSink value "length" (S.fold FL.length)
            , benchIOSink value "sum" (S.fold FL.sum)
            , benchIOSink value "product" (S.fold FL.product)
            , benchIOSink value "maximumBy" (S.fold (FL.maximumBy compare))
            , benchIOSink value "maximum" (S.fold FL.maximum)
            , benchIOSink value "minimumBy" (S.fold (FL.minimumBy compare))
            , benchIOSink value "minimum" (S.fold FL.minimum)
            , benchIOSink value "mean" (\s -> S.fold FL.mean (S.map (fromIntegral :: Int -> Double) s))
            , benchIOSink value "variance" (\s -> S.fold FL.variance (S.map (fromIntegral :: Int -> Double) s))
            , benchIOSink value "stdDev" (\s -> S.fold FL.stdDev (S.map (fromIntegral :: Int -> Double) s))

            , benchIOSink value "mconcat" (S.fold FL.mconcat . (S.map (Last . Just)))
            , benchIOSink value "foldMap" (S.fold (FL.foldMap (Last . Just)))

            , benchIOSink value "index" (S.fold (FL.index (value + 1)))
            , benchIOSink value "head" (S.fold FL.head)
            , benchIOSink value "find" (S.fold (FL.find (== (value + 1))))
            , benchIOSink value "findIndex" (S.fold (FL.findIndex (== (value + 1))))
            , benchIOSink value "elemIndex" (S.fold (FL.elemIndex (value + 1)))

            , benchIOSink value "null" (S.fold FL.null)
            , benchIOSink value "elem" (S.fold (FL.elem (value + 1)))
            , benchIOSink value "notElem" (S.fold (FL.notElem (value + 1)))
            , benchIOSink value "all" (S.fold (FL.all (<= (value + 1))))
            , benchIOSink value "any" (S.fold (FL.any (> (value + 1))))
            , benchIOSink value "and" (\s -> S.fold FL.and (S.map (<= (value + 1)) s))
            , benchIOSink value "or" (\s -> S.fold FL.or (S.map (> (value + 1)) s))
            ]
          , bgroup "fold-multi-stream"
            [ benchIOSink1 "eqBy" (Ops.eqBy value)
            , benchIOSink1 "cmpBy" (Ops.cmpBy value)
            , benchIOSink value "isPrefixOf" Ops.isPrefixOf
            , benchIOSink value "isSubsequenceOf" Ops.isSubsequenceOf
            , benchIOSink value "stripPrefix" Ops.stripPrefix
            ]
          , bgroup "folds-transforms"
            [ benchIOSink value "drain" (S.fold FL.drain)
            , benchIOSink value "lmap" (S.fold (IFL.lmap (+1) FL.drain))
            , benchIOSink value "pipe-mapM"
                 (S.fold (IFL.transform (Pipe.mapM (\x -> return $ x + 1)) FL.drain))
            ]
          , bgroup "folds-compositions" -- Applicative
            [
              benchIOSink value "all,any"    (S.fold ((,) <$> FL.all (<= (value + 1))
                                                      <*> FL.any (> (value + 1))))
            , benchIOSink value "sum,length" (S.fold ((,) <$> FL.sum <*> FL.length))
            ]
          , bgroup "pipes"
            [ benchIOSink value "mapM" (Ops.transformMapM serially 1)
            , benchIOSink value "compose" (Ops.transformComposeMapM serially 1)
            , benchIOSink value "tee" (Ops.transformTeeMapM serially 1)
            , benchIOSink value "zip" (Ops.transformZipMapM serially 1)
            ]
          , bgroup "pipesX4"
            [ benchIOSink value "mapM" (Ops.transformMapM serially 4)
            , benchIOSink value "compose" (Ops.transformComposeMapM serially 4)
            , benchIOSink value "tee" (Ops.transformTeeMapM serially 4)
            , benchIOSink value "zip" (Ops.transformZipMapM serially 4)
            ]
          , bgroup "transformer"
            [ benchIOSrc serially "evalState" (Ops.evalStateT value)
            , benchIOSrc serially "withState" (Ops.withState value)
            ]
          , bgroup "transformation"
            [ benchIOSink value "scanl" (Ops.scan 1)
            , benchIOSink value "scanl1'" (Ops.scanl1' 1)
            , benchIOSink value "map" (Ops.map 1)
            , benchIOSink value "fmap" (Ops.fmap 1)
            , benchIOSink value "mapM" (Ops.mapM serially 1)
            , benchIOSink value "mapMaybe" (Ops.mapMaybe 1)
            , benchIOSink value "mapMaybeM" (Ops.mapMaybeM 1)
            , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
                Ops.sequence serially (Ops.sourceUnfoldrMAction value n)
            , benchIOSink value "findIndices" (Ops.findIndices value 1)
            , benchIOSink value "elemIndices" (Ops.elemIndices value 1)
            , benchIOSink value "foldrS" (Ops.foldrS 1)
            , benchIOSink value "foldrSMap" (Ops.foldrSMap 1)
            , benchIOSink value "foldrT" (Ops.foldrT 1)
            , benchIOSink value "foldrTMap" (Ops.foldrTMap 1)
            , benchIOSink value "tap" (Ops.tap 1)
            , benchIOSink value "tapRate 1 second" (Ops.tapRate 1)
            , benchIOSink value "pollCounts 1 second" (Ops.pollCounts 1)
            , benchIOSink value "tapAsync" (Ops.tapAsync 1)
            , benchIOSink value "tapAsyncS" (Ops.tapAsyncS 1)

            , bench "reassembleBy srcInOrd" $ nfIO $ randomRIO (0,0) >>= Ops.benchReassembleBy (Ops.srcInOrd value)
            , bench "reassembleBy srcOrd 0" $ nfIO $ randomRIO (0,0) >>= Ops.benchReassembleBy (Ops.srcOrd value)
            , bench "reassembleBy srcOrd 1" $ nfIO $ randomRIO (1,1) >>= Ops.benchReassembleBy (Ops.srcOrd value)
            , bench "reassembleBy srcOrd 4" $ nfIO $ randomRIO (4,4) >>= Ops.benchReassembleBy (Ops.srcOrd value)

            ]
          , bgroup "transformationX4"
            [ benchIOSink value "scan" (Ops.scan 4)
            , benchIOSink value "scanl1'" (Ops.scanl1' 4)
            , benchIOSink value "map" (Ops.map 4)
            , benchIOSink value "fmap" (Ops.fmap 4)
            , benchIOSink value "mapM" (Ops.mapM serially 4)
            , benchIOSink value "mapMaybe" (Ops.mapMaybe 4)
            , benchIOSink value "mapMaybeM" (Ops.mapMaybeM 4)
            -- , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
                -- Ops.sequence serially (Ops.sourceUnfoldrMAction n)
            , benchIOSink value "findIndices" (Ops.findIndices value 4)
            , benchIOSink value "elemIndices" (Ops.elemIndices value 4)
            ]
          , bgroup "filtering"
            [ benchIOSink value "filter-even"     (Ops.filterEven 1)
            , benchIOSink value "filter-all-out"  (Ops.filterAllOut value 1)
            , benchIOSink value "filter-all-in"   (Ops.filterAllIn value 1)

            , benchIOSink value "take-all"        (Ops.takeAll value 1)
            , benchIOSink value "takeByTime-all"
                (Ops.takeByTime (NanoSecond64 maxBound) 1)
            , benchIOSink value "takeWhile-true"  (Ops.takeWhileTrue value 1)
            --, benchIOSink value "takeWhileM-true" (Ops.takeWhileMTrue 1)

            -- "drop-one" is dual to "last"
            , benchIOSink value "drop-one"        (Ops.dropOne 1)
            , benchIOSink value "drop-all"        (Ops.dropAll value 1)
            , benchIOSink value "dropByTime-all"
                (Ops.dropByTime (NanoSecond64 maxBound) 1)
            , benchIOSink value "dropWhile-true"  (Ops.dropWhileTrue value 1)
            --, benchIOSink value "dropWhileM-true" (Ops.dropWhileMTrue 1)
            , benchIOSink value "dropWhile-false" (Ops.dropWhileFalse value 1)

            , benchIOSink value "deleteBy" (Ops.deleteBy value 1)
            , benchIOSink value "intersperse" (Ops.intersperse value 1)
            , benchIOSink value "insertBy" (Ops.insertBy value 1)
            ]
          , bgroup "filteringX4"
            [ benchIOSink value "filter-even"     (Ops.filterEven 4)
            , benchIOSink value "filter-all-out"  (Ops.filterAllOut value 4)
            , benchIOSink value "filter-all-in"   (Ops.filterAllIn value 4)
            , benchIOSink value "take-all"        (Ops.takeAll value 4)
            , benchIOSink value "takeWhile-true"  (Ops.takeWhileTrue value 4)
            --, benchIOSink value "takeWhileM-true" (Ops.takeWhileMTrue 4)
            , benchIOSink value "drop-one"        (Ops.dropOne 4)
            , benchIOSink value "drop-all"        (Ops.dropAll value 4)
            , benchIOSink value "dropWhile-true"  (Ops.dropWhileTrue value 4)
            --, benchIOSink value "dropWhileM-true" (Ops.dropWhileMTrue 4)
            , benchIOSink value "dropWhile-false" (Ops.dropWhileFalse value 4)
            , benchIOSink value "deleteBy" (Ops.deleteBy value 4)
            , benchIOSink value "intersperse" (Ops.intersperse value 4)
            , benchIOSink value "insertBy" (Ops.insertBy value 4)
            ]
          , bgroup "joining"
            [ benchIOSrc1 "zip (2,x/2)" (Ops.zip (value `div` 2))
            , benchIOSrc1 "zipM (2,x/2)" (Ops.zipM (value `div` 2))
            , benchIOSrc1 "mergeBy (2,x/2)" (Ops.mergeBy (value `div` 2))
            , benchIOSrc1 "serial (2,x/2)" (Ops.serial2 (value `div` 2))
            , benchIOSrc1 "append (2,x/2)" (Ops.append2 (value `div` 2))
            , benchIOSrc1 "serial (2,2,x/4)" (Ops.serial4 (value `div` 4))
            , benchIOSrc1 "append (2,2,x/4)" (Ops.append4 (value `div` 4))
            , benchIOSrc1 "wSerial (2,x/2)" (Ops.wSerial2 value)
            , benchIOSrc1 "interleave (2,x/2)" (Ops.interleave2 value)
            , benchIOSrc1 "roundRobin (2,x/2)" (Ops.roundRobin2 value)
            ]
          , bgroup "concat-foldable"
            [ benchIOSrc serially "foldMapWith" (Ops.sourceFoldMapWith value)
            , benchIOSrc serially "foldMapWithM" (Ops.sourceFoldMapWithM value)
            , benchIOSrc serially "foldMapM" (Ops.sourceFoldMapM value)
            , benchIOSrc serially "foldWithConcatMapId" (Ops.sourceConcatMapId value)
            ]
          , bgroup "concat-serial"
            [ benchIOSrc1 "concatMapPure (2,x/2)" (Ops.concatMapPure 2 (value `div` 2))
            , benchIOSrc1 "concatMap (2,x/2)" (Ops.concatMap 2 (value `div` 2))
            , benchIOSrc1 "concatMap (x/2,2)" (Ops.concatMap (value `div` 2) 2)
            , benchIOSrc1 "concatMapRepl (x/4,4)" (Ops.concatMapRepl4xN value)
            , benchIOSrc1 "concatUnfoldRepl (x/4,4)" (Ops.concatUnfoldRepl4xN value)

            , benchIOSrc1 "concatMapWithSerial (2,x/2)"
                (Ops.concatMapWithSerial 2 (value `div` 2))
            , benchIOSrc1 "concatMapWithSerial (x/2,2)"
                (Ops.concatMapWithSerial (value `div` 2) 2)

            , benchIOSrc1 "concatMapWithAppend (2,x/2)"
                (Ops.concatMapWithAppend 2 (value `div` 2))
            ]
          , bgroup "outer-product-streams"
            [ benchIO "toNullAp"       $ Nested.toNullAp value       serially
            , benchIO "toNull"         $ Nested.toNull value         serially
            , benchIO "toNull3"        $ Nested.toNull3 value        serially
            , benchIO "filterAllOut"   $ Nested.filterAllOut value   serially
            , benchIO "filterAllIn"    $ Nested.filterAllIn value    serially
            , benchIO "filterSome"     $ Nested.filterSome value     serially
            , benchIO "breakAfterSome" $ Nested.breakAfterSome value serially
            ]
          , bgroup "outer-product-unfolds"
            [ benchIO "toNull"         $ NestedUnfold.toNull value
            , benchIO "toNull3"        $ NestedUnfold.toNull3 value
            , benchIO "concat"         $ NestedUnfold.concat value
            , benchIO "filterAllOut"   $ NestedUnfold.filterAllOut value
            , benchIO "filterAllIn"    $ NestedUnfold.filterAllIn value
            , benchIO "filterSome"     $ NestedUnfold.filterSome value
            , benchIO "breakAfterSome" $ NestedUnfold.breakAfterSome value
            ]
          -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
          -- library. If scan/fold followed by a map is efficient enough we may not
          -- need monolithic implementations of these.
          , bgroup "mixed"
            [ benchIOSink value "scanl-map" (Ops.scanMap 1)
            , benchIOSink value "foldl-map" Ops.foldl'ReduceMap
            , benchIOSink value "sum-product-fold"  Ops.sumProductFold
            , benchIOSink value "sum-product-scan"  Ops.sumProductScan
            ]
          , bgroup "mixedX4"
            [ benchIOSink value "scan-map"    (Ops.scanMap 4)
            , benchIOSink value "drop-map"    (Ops.dropMap 4)
            , benchIOSink value "drop-scan"   (Ops.dropScan 4)
            , benchIOSink value "take-drop"   (Ops.takeDrop value 4)
            , benchIOSink value "take-scan"   (Ops.takeScan value 4)
            , benchIOSink value "take-map"    (Ops.takeMap value 4)
            , benchIOSink value "filter-drop" (Ops.filterDrop value 4)
            , benchIOSink value "filter-take" (Ops.filterTake value 4)
            , benchIOSink value "filter-scan" (Ops.filterScan 4)
            , benchIOSink value "filter-scanl1" (Ops.filterScanl1 4)
            , benchIOSink value "filter-map"  (Ops.filterMap value 4)
            ]
          ]
        , bgroup "wSerially"
          [ bgroup "transformation"
            [ benchIOSink value "fmap"   $ Ops.fmap' wSerially 1
            ]
          , bgroup "concatMap"
            [ benchIOSrc1 "concatMapWithWSerial (2,x/2)"
                (Ops.concatMapWithWSerial 2 (value `div` 2))
            , benchIOSrc1 "concatMapWithWSerial (x/2,2)"
                (Ops.concatMapWithWSerial (value `div` 2) 2)
            ]
          , bgroup "outer-product"
            [ benchIO "toNullAp"       $ Nested.toNullAp value       wSerially
            , benchIO "toNull"         $ Nested.toNull value         wSerially
            , benchIO "toNull3"        $ Nested.toNull3 value        wSerially
            , benchIO "filterAllOut"   $ Nested.filterAllOut value   wSerially
            , benchIO "filterAllIn"    $ Nested.filterAllIn value    wSerially
            , benchIO "filterSome"     $ Nested.filterSome value     wSerially
            , benchIO "breakAfterSome" $ Nested.breakAfterSome value wSerially
            ]
          ]
        , bgroup "zipSerially"
          [ bgroup "transformation"
            [ benchIOSink value "fmap"   $ Ops.fmap' zipSerially 1
            ]
            -- XXX needs fixing
            {-
          , bgroup "outer-product"
            [ benchIO "toNullAp"  $ Nested.toNullAp value  zipSerially
            ]
            -}
          ]
        ]
