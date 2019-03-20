-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.DeepSeq (NFData(..), rnf)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (randomRIO)
import Data.Monoid (Last(..))
import Control.Monad.StrictIdentity -- (StrictIdentity)
import Streamly.Internal.MonadLazy

import qualified GHC.Exts as GHC
import qualified LinearOps as Ops

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Foldr   as FR
import qualified Streamly.Foldl   as FL
import qualified Streamly.Sink    as Sink
import Gauge

-------------------------------------------------------------------------------
-- MyIO and StrictIdentity are for performance experimentation for Foldr
-------------------------------------------------------------------------------

instance NFData a => NFData (StrictIdentity a) where
    {-# INLINE rnf #-}
    rnf = rnf . runStrictIdentity

instance NFData a => NFData (MyIO a) where
    {-# INLINE rnf #-}
    rnf = rnf . runMyIO

{-# INLINE benchMyIOSink #-}
benchMyIOSink
    :: (IsStream t, NFData b)
    => String -> (t MyIO Int -> MyIO b) -> Benchmark
-- benchMyIOSink name f = bench name $ nfIO $ randomRIO (1,1) >>= \x -> myIOtoIO $ f $ Ops.source x
benchMyIOSink name f = bench name $ nf (f . Ops.source) 1

{-# INLINE benchStrictIdentitySink #-}
benchStrictIdentitySink
    :: (IsStream t, NFData b)
    => String -> (t StrictIdentity Int -> StrictIdentity b) -> Benchmark
benchStrictIdentitySink name f = bench name $ nf (f . Ops.source) 1

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => String -> (t IO Int -> IO b) -> Benchmark
benchIOSink name f = bench name $ nfIO $ randomRIO (1,1) >>= f . Ops.source

-- XXX We should be using sourceUnfoldrM for fair comparison with IO monad, but
-- we can't use it as it requires MonadAsync constraint.
{-# INLINE benchIdentitySink #-}
benchIdentitySink
    :: (IsStream t, NFData b)
    => String -> (t Identity Int -> Identity b) -> Benchmark
benchIdentitySink name f = bench name $ nf (f . Ops.sourceUnfoldr) 1

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: (t IO a -> SerialT IO a)
    -> String
    -> (Int -> t IO a)
    -> Benchmark
benchIOSrc t name f =
    bench name $ nfIO $ randomRIO (1,1) >>= Ops.toNull t . f

{-# INLINE benchPure #-}
benchPure :: NFData b => String -> (Int -> a) -> (a -> b) -> Benchmark
benchPure name src f = bench name $ nfIO $ randomRIO (1,1) >>= return . f . src

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => String -> (SerialT Identity Int -> b) -> Benchmark
benchPureSink name f = benchPure name Ops.sourceUnfoldr f

{-# INLINE benchPureSinkIO #-}
benchPureSinkIO
    :: NFData b
    => String -> (SerialT Identity Int -> IO b) -> Benchmark
benchPureSinkIO name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= f . Ops.sourceUnfoldr

{-# INLINE benchPureSrc #-}
benchPureSrc :: String -> (Int -> SerialT Identity a) -> Benchmark
benchPureSrc name src = benchPure name src (runIdentity . runStream)

main :: IO ()
main =
  defaultMain
    [ bgroup "serially"
      [ bgroup "pure"
        [ benchPureSink "id" id
        , benchPureSink "eqBy" Ops.eqBy
        , benchPureSink "==" Ops.eqInstance
        , benchPureSink "/=" Ops.eqInstanceNotEq
        , benchPureSink "cmpBy" Ops.cmpBy
        , benchPureSink "<" Ops.ordInstance
        , benchPureSink "min" Ops.ordInstanceMin
        , benchPureSrc "IsList.fromList" Ops.sourceIsList
        , benchPureSink "IsList.toList" GHC.toList
        , benchPureSrc "IsString.fromString" Ops.sourceIsString
        , benchPure "readsPrec" (\n -> S.fromList [1..n :: Int])
                    Ops.readInstance
        , benchPureSink "showsPrec" Ops.showInstance
        , benchPure "showsPrecList" (\n -> S.fromList [1..n :: Int])
                    Ops.showInstanceList
        , benchPureSink "foldl'" Ops.pureFoldl'
        , benchPureSink "foldable/foldl'" Ops.foldableFoldl'
        , benchPureSink "foldable/sum" Ops.foldableSum
        , benchPureSinkIO "traversable/mapM" Ops.traversableMapM
        ]
      , bgroup "generation"
        [ -- Most basic, barely stream continuations running
          benchIOSrc serially "unfoldr" Ops.sourceUnfoldr
        , benchIOSrc serially "unfoldrM" Ops.sourceUnfoldrM
        , benchIOSrc serially "intFromTo" Ops.sourceIntFromTo
        , benchIOSrc serially "intFromThenTo" Ops.sourceIntFromThenTo
        , benchIOSrc serially "integerFromStep" Ops.sourceIntegerFromStep
        , benchIOSrc serially "fracFromThenTo" Ops.sourceFracFromThenTo
        , benchIOSrc serially "fracFromTo" Ops.sourceFracFromTo
        , benchIOSrc serially "fromList" Ops.sourceFromList
        , benchIOSrc serially "fromListM" Ops.sourceFromListM
        -- These are essentially cons and consM
        , benchIOSrc serially "fromFoldable" Ops.sourceFromFoldable
        , benchIOSrc serially "fromFoldableM" Ops.sourceFromFoldableM
        -- These are essentially appends
        , benchIOSrc serially "foldMapWith" Ops.sourceFoldMapWith
        , benchIOSrc serially "foldMapWithM" Ops.sourceFoldMapWithM
        , benchIOSrc serially "foldMapM" Ops.sourceFoldMapM
        ]
      , bgroup "generic-folds"
        [ bgroup "reduce"
          [ bgroup "IO"
            [ benchIOSink "foldr" Ops.foldrReduce
            , benchIOSink "foldr1" Ops.foldr1Reduce
            , benchIOSink "foldl'" Ops.foldl'Reduce
            , benchIOSink "foldl1'" Ops.foldl1'Reduce
            , benchIOSink "foldlM'" Ops.foldlM'Reduce
            ]
          , bgroup "Identity"
            [ benchIdentitySink "foldr" Ops.foldrReduce
            , benchIdentitySink "foldr1" Ops.foldr1Reduce
            , benchIdentitySink "foldl'" Ops.foldl'Reduce
            , benchIdentitySink "foldl1'" Ops.foldl1'Reduce
            , benchIdentitySink "foldlM'" Ops.foldlM'Reduce
            ]
          ]

        , bgroup "build"
          [ bgroup "IO"
            [ benchIOSink "foldr" Ops.foldrBuild
            , benchIOSink "foldrM" Ops.foldrMBuild
            , benchIOSink "foldl'" Ops.foldl'Build
            , benchIOSink "foldlM'" Ops.foldlM'Build
            ]
          , bgroup "Identity"
            [ benchIdentitySink "foldr" Ops.foldrBuild
            , benchIdentitySink "foldrM" Ops.foldrMBuild
            , benchIdentitySink "foldl'" Ops.foldl'Build
            , benchIdentitySink "foldlM'" Ops.foldlM'Build
            ]
          ]
        ]
      , bgroup "specific-folds"
        [
          benchIOSink "uncons" Ops.uncons
        , benchIOSink "toNull" $ Ops.toNull serially
        , benchIOSink "mapM_" Ops.mapM_

        , benchIOSink "init" Ops.init
        , benchIOSink "tail" Ops.tail
        , benchIOSink "nullHeadTail" Ops.nullHeadTail

        , benchIOSink "head" Ops.head
        , benchIOSink "last" Ops.last
        -- , benchIOSink "lookup" Ops.lookup
        , benchIOSink "find" Ops.find
        , benchIOSink "findIndex" Ops.findIndex
        , benchIOSink "elemIndex" Ops.elemIndex

        , benchIOSink "null" Ops.null
        , benchIOSink "elem" Ops.elem
        , benchIOSink "notElem" Ops.notElem
        , benchIdentitySink "all" Ops.all
        , benchIOSink "any" Ops.any
        , benchIOSink "and" Ops.and
        , benchIOSink "or" Ops.or

        , benchIOSink "length" Ops.length
        , benchIOSink "sum" Ops.sum
        , benchIOSink "product" Ops.product

        , benchIOSink "maximumBy" Ops.maximumBy
        , benchIOSink "maximum" Ops.maximum
        , benchIOSink "minimumBy" Ops.minimumBy
        , benchIOSink "minimum" Ops.minimum

        , benchIOSink "toList" Ops.toList
        ]
      , bgroup "composable-foldr"
        [
          benchIOSink "drain" (FR.foldr FR.drain)
        , benchIOSink "null" (FR.foldr FR.null)
        , benchIOSink "all" (FR.foldr (FR.all (<= Ops.maxValue)))
        , benchIOSink "any" (FR.foldr (FR.any (> Ops.maxValue)))
        , benchIOSink "length" (FR.foldr FR.length)
        , benchIOSink "toList" (FR.foldr FR.toList)
        ]
      , bgroup "composed-foldr"
        [
          benchIOSink "any,any-io" (FR.foldr ((,) <$> FR.any (> Ops.maxValue)
                                                  <*> FR.any (> Ops.maxValue)))
        , benchIdentitySink "any,any-identity" (FR.foldr ((,) <$> FR.any (> Ops.maxValue)
                                                  <*> FR.any (> Ops.maxValue)))
        , benchStrictIdentitySink "any,any-strictidentity" (FR.foldr ((,) <$> FR.any (> Ops.maxValue)
                                                  <*> FR.any (> Ops.maxValue)))
        , benchMyIOSink "any,any-myio" (FR.foldr ((,) <$> FR.any (> Ops.maxValue)
                                                  <*> FR.any (> Ops.maxValue)))
        , benchIOSink "all,any-io"    (FR.foldr ((,) <$> FR.all (<= Ops.maxValue)
                                                  <*> FR.any (> Ops.maxValue)))
        , benchIdentitySink "all,any-identity" (FR.foldr ((,) <$> FR.all (<= Ops.maxValue)
                                                  <*> FR.any (> Ops.maxValue)))
        ]
      , bgroup "composable-foldl"
        [
        -- ** Monoidal Folds
          benchIOSink "mconcat" (FL.foldl FL.mconcat . (S.map (Last . Just)))
        -- , benchIOSink "foldMap" (FL.foldl (FL.foldlMap (Last . Just)))

        -- ** Run Effects
        -- , runStream
        -- , runN
        -- , runWhile
        , benchIOSink "drain" (FL.foldl FL.drain)
        , benchIOSink "sink" (Sink.sink Sink.drain)

        -- ** To Elements
        -- | Folds that extract selected elements of a stream or their properties.
        -- , (!!)
        , benchIOSink "index" (FL.foldl (FL.index Ops.maxValue))
        , benchIOSink "head" (FL.foldl FL.head)
        , benchIOSink "last" (FL.foldl FL.last)
        -- , findM
        , benchIOSink "find" (FL.foldl (FL.find (== Ops.maxValue)))
        --, lookup
        , benchIOSink "findIndex" (FL.foldl (FL.findIndex (== Ops.maxValue)))
        , benchIOSink "elemIndex" (FL.foldl (FL.elemIndex Ops.maxValue))

        -- ** To Parts
        -- | Folds that extract selected parts of a stream.
        -- , tail
        -- , init

        -- ** To Boolean
        -- | Folds that summarize the stream to a boolean value.
        , benchIOSink "null" (FL.foldl FL.null)
        , benchIOSink "elem" (FL.foldl (FL.elem Ops.maxValue))
        , benchIOSink "notElem" (FL.foldl (FL.notElem Ops.maxValue))
        , benchIOSink "all" (FL.foldl (FL.all (<= Ops.maxValue)))
        , benchIOSink "any" (FL.foldl (FL.any (> Ops.maxValue)))
        , benchIOSink "and" (\s -> FL.foldl FL.and (S.map (<= Ops.maxValue) s))
        , benchIOSink "or" (\s -> FL.foldl FL.or (S.map (> Ops.maxValue) s))

        -- ** To Summary
        -- | Folds that summarize the stream to a single value.
        , benchIOSink "length" (FL.foldl FL.length)
        , benchIOSink "sum" (FL.foldl FL.sum)
        , benchIOSink "product" (FL.foldl FL.product)

        -- ** To Summary (Statistical)
        , benchIOSink "mean" (\s -> FL.foldl FL.mean (S.map (fromIntegral :: Int -> Double) s))
        , benchIOSink "variance" (\s -> FL.foldl FL.variance (S.map (fromIntegral :: Int -> Double) s))
        , benchIOSink "stdDev" (\s -> FL.foldl FL.stdDev (S.map (fromIntegral :: Int -> Double) s))

        -- ** To Summary (Maybe)
        -- | Folds that summarize a non-empty stream to a 'Just' value and return
        -- 'Nothing' for an empty stream.
        , benchIOSink "maximumBy" (FL.foldl (FL.maximumBy compare))
        , benchIOSink "maximum" (FL.foldl FL.maximum)
        , benchIOSink "minimumBy" (FL.foldl (FL.minimumBy compare))
        , benchIOSink "minimum" (FL.foldl FL.minimum)
        -- , the

        -- ** To Containers
        -- | Convert or divert a stream into an output structure, container or
        -- sink.
        , benchIOSink "toList" (FL.foldl FL.toList)
        , benchIOSink "toRevList" (FL.foldl FL.toRevList)
        -- , toHandle
        ]
        -- Applicative
      , bgroup "composed-foldl"
        [
          benchIOSink "all,any"    (FL.foldl ((,) <$> FL.all (<= Ops.maxValue)
                                                  <*> FL.any (> Ops.maxValue)))
        , benchIOSink "sum,length" (FL.foldl ((,) <$> FL.sum <*> FL.length))
        ]
      , bgroup "transformation"
        [ benchIOSink "scan" (Ops.scan 1)
        , benchIOSink "scanl1'" (Ops.scanl1' 1)
        , benchIOSink "map" (Ops.map 1)
        , benchIOSink "fmap" (Ops.fmap 1)
        , benchIOSink "mapM" (Ops.mapM serially 1)
        , benchIOSink "mapMaybe" (Ops.mapMaybe 1)
        , benchIOSink "mapMaybeM" (Ops.mapMaybeM 1)
        , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            Ops.sequence serially (Ops.sourceUnfoldrMAction n)
        , benchIOSink "findIndices" (Ops.findIndices 1)
        , benchIOSink "elemIndices" (Ops.elemIndices 1)
        ]
      , bgroup "transformationX4"
        [ benchIOSink "scan" (Ops.scan 4)
        , benchIOSink "scanl1'" (Ops.scanl1' 4)
        , benchIOSink "map" (Ops.map 4)
        , benchIOSink "fmap" (Ops.fmap 4)
        , benchIOSink "mapM" (Ops.mapM serially 4)
        , benchIOSink "mapMaybe" (Ops.mapMaybe 4)
        , benchIOSink "mapMaybeM" (Ops.mapMaybeM 4)
        -- , bench "sequence" $ nfIO $ randomRIO (1,1000) >>= \n ->
            -- Ops.sequence serially (Ops.sourceUnfoldrMAction n)
        , benchIOSink "findIndices" (Ops.findIndices 4)
        , benchIOSink "elemIndices" (Ops.elemIndices 4)
        ]
      , bgroup "filtering"
        [ benchIOSink "filter-even"     (Ops.filterEven 1)
        , benchIOSink "filter-all-out"  (Ops.filterAllOut 1)
        , benchIOSink "filter-all-in"   (Ops.filterAllIn 1)
        , benchIOSink "take-all"        (Ops.takeAll 1)
        , benchIOSink "takeWhile-true"  (Ops.takeWhileTrue 1)
        --, benchIOSink "takeWhileM-true" (Ops.takeWhileMTrue 1)
        , benchIOSink "drop-one"        (Ops.dropOne 1)
        , benchIOSink "drop-all"        (Ops.dropAll 1)
        , benchIOSink "dropWhile-true"  (Ops.dropWhileTrue 1)
        --, benchIOSink "dropWhileM-true" (Ops.dropWhileMTrue 1)
        , benchIOSink "dropWhile-false" (Ops.dropWhileFalse 1)
        , benchIOSink "deleteBy" (Ops.deleteBy 1)
        , benchIOSink "insertBy" (Ops.insertBy 1)
        ]
      , bgroup "filteringX4"
        [ benchIOSink "filter-even"     (Ops.filterEven 4)
        , benchIOSink "filter-all-out"  (Ops.filterAllOut 4)
        , benchIOSink "filter-all-in"   (Ops.filterAllIn 4)
        , benchIOSink "take-all"        (Ops.takeAll 4)
        , benchIOSink "takeWhile-true"  (Ops.takeWhileTrue 4)
        --, benchIOSink "takeWhileM-true" (Ops.takeWhileMTrue 4)
        , benchIOSink "drop-one"        (Ops.dropOne 4)
        , benchIOSink "drop-all"        (Ops.dropAll 4)
        , benchIOSink "dropWhile-true"  (Ops.dropWhileTrue 4)
        --, benchIOSink "dropWhileM-true" (Ops.dropWhileMTrue 4)
        , benchIOSink "dropWhile-false" (Ops.dropWhileFalse 4)
        , benchIOSink "deleteBy" (Ops.deleteBy 4)
        , benchIOSink "insertBy" (Ops.insertBy 4)
        ]
      , bgroup "multi-stream"
        [ benchIOSink "eqBy" Ops.eqBy
        , benchIOSink "cmpBy" Ops.cmpBy
        , benchIOSink "zip" Ops.zip
        , benchIOSink "zipM" Ops.zipM
        , benchIOSink "mergeBy" Ops.mergeBy
        , benchIOSink "isPrefixOf" Ops.isPrefixOf
        , benchIOSink "isSubsequenceOf" Ops.isSubsequenceOf
        , benchIOSink "stripPrefix" Ops.stripPrefix
        , benchIOSrc  serially "concatMap" Ops.concatMap
        ]
    , bgroup "mixed"
      [ benchIOSink "sum-product-fold"  Ops.sumProductFold
      , benchIOSink "sum-product-scan"  Ops.sumProductScan
      ]
    , bgroup "mixedX4"
      [ benchIOSink "scan-map"    (Ops.scanMap 4)
      , benchIOSink "drop-map"    (Ops.dropMap 4)
      , benchIOSink "drop-scan"   (Ops.dropScan 4)
      , benchIOSink "take-drop"   (Ops.takeDrop 4)
      , benchIOSink "take-scan"   (Ops.takeScan 4)
      , benchIOSink "take-map"    (Ops.takeMap 4)
      , benchIOSink "filter-drop" (Ops.filterDrop 4)
      , benchIOSink "filter-take" (Ops.filterTake 4)
      , benchIOSink "filter-scan" (Ops.filterScan 4)
      , benchIOSink "filter-scanl1" (Ops.filterScanl1 4)
      , benchIOSink "filter-map"  (Ops.filterMap 4)
      ]
    , bgroup "iterated"
      [ benchIOSrc serially "mapM"           Ops.iterateMapM
      , benchIOSrc serially "scan(1/100)"    Ops.iterateScan
      , benchIOSrc serially "scanl1(1/100)"  Ops.iterateScanl1
      , benchIOSrc serially "filterEven"     Ops.iterateFilterEven
      , benchIOSrc serially "takeAll"        Ops.iterateTakeAll
      , benchIOSrc serially "dropOne"        Ops.iterateDropOne
      , benchIOSrc serially "dropWhileFalse" Ops.iterateDropWhileFalse
      , benchIOSrc serially "dropWhileTrue"  Ops.iterateDropWhileTrue
      ]
      ]
    ]
