-- |
-- Module      : Streamly.Test.Data.Stream.Transform
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Transform (main) where

import Control.Exception (ErrorCall(..), catch)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units (NanoSecond64(..), fromRelTime64, diffAbsTime64)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Scan as Scan
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec as H
#ifdef DEVBUILD
#endif

toList :: Monad m => Stream m a -> m [a]
toList = Stream.toList

-------------------------------------------------------------------------------
-- Strictness
-------------------------------------------------------------------------------

checkScanlMStrictness :: (IORef Int -> Stream IO Int -> Stream IO ()) -> IO ()
checkScanlMStrictness f = do
  ref <- newIORef 0
  let s = Stream.fromList ((1 :: Int) : error "x")
  catch (Stream.drain $ f ref s) (\(_ :: ErrorCall) -> return ())
  readIORef ref `shouldReturn` 1

checkScanl'Strictness :: IO ()
checkScanl'Strictness = do
    let s = Stream.fromList ((1 :: Int) : error "failure")
    catch
        (Stream.drain
             (Stream.scanl'
                  (\_ a ->
                       if a == 1
                           then error "success"
                           else "done")
                  "begin"
                  s)
             >> return "finished"
        )
        (\(ErrorCall err) -> return err)
        `shouldReturn` "success"

scanlM'StrictCheck :: IORef Int -> Stream IO Int -> Stream IO ()
scanlM'StrictCheck ref = Stream.scanlM' (\_ _ -> writeIORef ref 1) (return ())

#ifdef DEVBUILD
checkScanxStrictness :: IO ()
checkScanxStrictness = do
  let s = Stream.fromList ((1 :: Int) : error "failure")
  catch
    (Stream.drain (
        Stream.scanlx' (\_ a ->
                    if a == 1
                    then error "success"
                    else "done")
                "begin" id s
        )
        >> return "finished"
    )
    (\(ErrorCall err) -> return err)
    `shouldReturn` "success"
#endif

-------------------------------------------------------------------------------
-- Mapping Effects
-------------------------------------------------------------------------------

testSequence :: Expectation
testSequence = do
    ref <- newIORef (0 :: Int)
    let acts = Stream.fromList
                  [ modifyIORef ref (+1) >> return 10
                  , modifyIORef ref (+2) >> return 20
                  , modifyIORef ref (+3) >> return 30
                  ]
    xs <- toList (Stream.sequence acts)
    xs `shouldBe` [10, 20, 30 :: Int]
    readIORef ref `shouldReturn` 6

testTapOffsetEvery :: Expectation
testTapOffsetEvery = do
    ref <- newIORef ([] :: [Int])
    let captureFold = Fold.drainMapM (\x -> modifyIORef ref (x:))
    xs <- toList $ Stream.tapOffsetEvery 0 2 captureFold
              (Stream.fromList [1, 2, 3, 4, 5, 6 :: Int])
    xs `shouldBe` [1..6]
    fmap reverse (readIORef ref) `shouldReturn` [1, 3, 5]

testTrace_ :: Expectation
testTrace_ = do
    ref <- newIORef (0 :: Int)
    xs <- toList $ Stream.trace_ (modifyIORef ref (+1))
              (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 3

-------------------------------------------------------------------------------
-- Folding into a Stream
-------------------------------------------------------------------------------

testFoldlS :: Expectation
testFoldlS = do
    let step acc x = Stream.cons x acc
    xs <- toList $ Stream.foldlS step Stream.nil (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [3, 2, 1]

testFoldlSEmpty :: Expectation
testFoldlSEmpty = do
    let step acc x = Stream.cons x acc
    xs <- toList $ Stream.foldlS step Stream.nil (Stream.fromList ([] :: [Int]))
    xs `shouldBe` []

-------------------------------------------------------------------------------
-- Composable Scans
-------------------------------------------------------------------------------

testScanlScanl :: Expectation
testScanlScanl =
    toList (Stream.scanl Scanl.sum (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3, 6]

testScanlScanlEmpty :: Expectation
testScanlScanlEmpty =
    toList (Stream.scanl Scanl.sum (Stream.fromList ([] :: [Int])))
        `shouldReturn` [0]

testPostscanl :: Expectation
testPostscanl =
    toList (Stream.postscanl Scanl.sum (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6]

testPostscanlEmpty :: Expectation
testPostscanlEmpty =
    toList (Stream.postscanl Scanl.sum (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

testScanlMany :: Expectation
testScanlMany =
    toList (Stream.scanlMany Scanl.sum (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3, 6]

testScanr :: Expectation
testScanr =
    toList (Stream.scanr Scan.sum (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6]

testPipe :: Expectation
testPipe =
    toList (Stream.pipe (Pipe.map (*2)) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [2, 4, 6]

testPipeFilter :: Expectation
testPipeFilter =
    toList (Stream.pipe (Pipe.filter even) (Stream.fromList [1..6 :: Int]))
        `shouldReturn` [2, 4, 6]

-------------------------------------------------------------------------------
-- Prescans
-------------------------------------------------------------------------------

testPrescanl' :: Expectation
testPrescanl' =
    toList (Stream.prescanl' (+) 0 (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3]

testPrescanlM' :: Expectation
testPrescanlM' =
    toList (Stream.prescanlM' (\a b -> return (a + b)) (return 0)
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3]

-------------------------------------------------------------------------------
-- Lazy Scans
-------------------------------------------------------------------------------

testScanlBy :: Expectation
testScanlBy =
    toList (Stream.scanlBy (+) 0 (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3, 6]

testScanlM :: Expectation
testScanlM =
    toList (Stream.scanlM (\a b -> return (a + b)) (return 0)
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3, 6]

testScanl1 :: Expectation
testScanl1 =
    toList (Stream.scanl1 (+) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6]

testScanl1M :: Expectation
testScanl1M =
    toList (Stream.scanl1M (\a b -> return (a + b))
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6]

testPostscanlBy :: Expectation
testPostscanlBy =
    toList (Stream.postscanlBy (+) 0 (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6]

testPostscanlM :: Expectation
testPostscanlM =
    toList (Stream.postscanlM (\a b -> return (a + b)) (return 0)
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6]

-------------------------------------------------------------------------------
-- Advanced Scans (with extractor)
-------------------------------------------------------------------------------

testPostscanlx' :: Expectation
testPostscanlx' =
    toList (Stream.postscanlx' (+) 0 (*2) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [2, 6, 12]

testScanlx' :: Expectation
testScanlx' =
    toList (Stream.scanlx' (+) 0 (*2) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 2, 6, 12]

testScanlMx' :: Expectation
testScanlMx' =
    toList (Stream.scanlMx' (\a b -> return (a + b)) (return 0) return
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3, 6]

testPostscanlMAfter' :: Expectation
testPostscanlMAfter' =
    toList (Stream.postscanlMAfter' (\a b -> return (a + b)) (return 0)
                (\s -> return (s * 10))
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 3, 6, 60]

testPostscanlMx' :: Expectation
testPostscanlMx' =
    toList (Stream.postscanlMx' (\a b -> return (a + b)) (return 0) (\x -> return (x * 2))
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [2, 6, 12]

testScanlMAfter' :: Expectation
testScanlMAfter' =
    toList (Stream.scanlMAfter' (\a b -> return (a + b)) (return 0)
                (\s -> return (s * 10))
                (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [0, 1, 3, 6, 60]

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

testWith :: Expectation
testWith = do
    let filterWithIndex = Stream.with Stream.indexed Stream.filter
    xs <- toList $ filterWithIndex (\(i, _) -> even i)
              (Stream.fromList [10, 20, 30, 40, 50 :: Int])
    xs `shouldBe` [10, 30, 50]

testPostscanlMaybe :: Expectation
testPostscanlMaybe =
    toList (Stream.postscanlMaybe (Scanl.filtering even)
                (Stream.fromList [1..6 :: Int]))
        `shouldReturn` [2, 4, 6]

testUniqBy :: Expectation
testUniqBy =
    toList (Stream.uniqBy (==) (Stream.fromList [1, 1, 2, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3]

testUniqByCustom :: Expectation
testUniqByCustom = do
    let sameSign a b = (a > 0) == (b > 0)
    xs <- toList $ Stream.uniqBy sameSign (Stream.fromList [1, 2, -1, -2, 3 :: Int])
    xs `shouldBe` [1, -1, 3]

-------------------------------------------------------------------------------
-- Sampling
-------------------------------------------------------------------------------

testSampleFromThen :: Expectation
testSampleFromThen =
    toList (Stream.sampleFromThen 0 2 (Stream.fromList [0..9 :: Int]))
        `shouldReturn` [0, 2, 4, 6, 8]

testSampleFromThenOffset :: Expectation
testSampleFromThenOffset =
    toList (Stream.sampleFromThen 1 3 (Stream.fromList [10, 20, 30, 40, 50, 60, 70 :: Int]))
        `shouldReturn` [20, 50]

-------------------------------------------------------------------------------
-- Trimming
-------------------------------------------------------------------------------

testInitNonEmpty :: Expectation
testInitNonEmpty =
    toList (Stream.initNonEmpty (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2]

testInitNonEmptySingle :: Expectation
testInitNonEmptySingle =
    toList (Stream.initNonEmpty (Stream.fromList [42 :: Int]))
        `shouldReturn` []

testTailNonEmpty :: Expectation
testTailNonEmpty =
    toList (Stream.tailNonEmpty (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [2, 3]

testTailNonEmptySingle :: Expectation
testTailNonEmptySingle =
    toList (Stream.tailNonEmpty (Stream.fromList [42 :: Int]))
        `shouldReturn` []

-------------------------------------------------------------------------------
-- Inserting
-------------------------------------------------------------------------------

testIntersperseEndByM :: Expectation
testIntersperseEndByM =
    toList (Stream.intersperseEndByM (return 0) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 0, 2, 0, 3, 0]

testIntersperseEndByMSingle :: Expectation
testIntersperseEndByMSingle =
    toList (Stream.intersperseEndByM (return 0) (Stream.fromList [1 :: Int]))
        `shouldReturn` [1, 0]

testIntersperseEndByEveryM :: Expectation
testIntersperseEndByEveryM =
    toList (Stream.intersperseEndByEveryM 2 (return ',')
                (Stream.fromList "abcdef"))
        `shouldReturn` "ab,cd,ef,"

testIntersperseM_ :: Expectation
testIntersperseM_ = do
    ref <- newIORef (0 :: Int)
    xs <- toList $ Stream.intersperseM_ (modifyIORef ref (+1))
              (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 2

testIntersperseEndByM_ :: Expectation
testIntersperseEndByM_ = do
    ref <- newIORef (0 :: Int)
    xs <- toList $ Stream.intersperseEndByM_ (modifyIORef ref (+1))
              (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 3

testIntersperseBeginByM_ :: Expectation
testIntersperseBeginByM_ = do
    ref <- newIORef (0 :: Int)
    xs <- toList $ Stream.intersperseBeginByM_ (modifyIORef ref (+1))
              (Stream.fromList [1, 2, 3 :: Int])
    xs `shouldBe` [1, 2, 3]
    readIORef ref `shouldReturn` 3

-------------------------------------------------------------------------------
-- Position Indexing
-------------------------------------------------------------------------------

testIndexed :: Expectation
testIndexed =
    toList (Stream.indexed (Stream.fromList ['a', 'b', 'c']))
        `shouldReturn` [(0, 'a'), (1, 'b'), (2, 'c')]

testIndexedEmpty :: Expectation
testIndexedEmpty =
    toList (Stream.indexed (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

testIndexedR :: Expectation
testIndexedR =
    toList (Stream.indexedR 3 (Stream.fromList [10, 20, 30 :: Int]))
        `shouldReturn` [(3, 10), (2, 20), (1, 30)]

-------------------------------------------------------------------------------
-- Time Indexing (smoke tests)
-------------------------------------------------------------------------------

testTimestampWith :: Expectation
testTimestampWith = do
    xs <- toList $ Stream.take 3 (Stream.timestampWith 0.01 Stream.times)
    length xs `shouldBe` 3

testTimestamped :: Expectation
testTimestamped = do
    xs <- toList $ Stream.take 3 (Stream.timestamped Stream.times)
    length xs `shouldBe` 3

testTimeIndexWith :: Expectation
testTimeIndexWith = do
    xs <- toList $ Stream.take 3 (Stream.timeIndexWith 0.01 Stream.times)
    length xs `shouldBe` 3

testTimeIndexed :: Expectation
testTimeIndexed = do
    xs <- toList $ Stream.take 3 (Stream.timeIndexed Stream.times)
    length xs `shouldBe` 3

-------------------------------------------------------------------------------
-- Rolling Map
-------------------------------------------------------------------------------

testRollingMap :: Expectation
testRollingMap =
    toList (Stream.rollingMap (\prev cur -> maybe cur (cur -) prev)
                (Stream.fromList [1, 3, 6, 10 :: Int]))
        `shouldReturn` [1, 2, 3, 4]

testRollingMapM :: Expectation
testRollingMapM =
    toList (Stream.rollingMapM (\prev cur -> return (maybe cur (cur -) prev))
                (Stream.fromList [1, 3, 6, 10 :: Int]))
        `shouldReturn` [1, 2, 3, 4]

testRollingMap2 :: Expectation
testRollingMap2 =
    toList (Stream.rollingMap2 (flip (-))
                (Stream.fromList [1, 3, 6, 10 :: Int]))
        `shouldReturn` [2, 3, 4]

testRollingMapSingle :: Expectation
testRollingMapSingle =
    toList (Stream.rollingMap (\prev cur -> maybe cur (cur -) prev)
                (Stream.fromList [42 :: Int]))
        `shouldReturn` [42]

-------------------------------------------------------------------------------
-- Maybe Streams
-------------------------------------------------------------------------------

testCatMaybes :: Expectation
testCatMaybes =
    toList (Stream.catMaybes
                (Stream.fromList [Just 1, Nothing, Just 2, Nothing, Just 3 :: Maybe Int]))
        `shouldReturn` [1, 2, 3]

testCatMaybesEmpty :: Expectation
testCatMaybesEmpty =
    toList (Stream.catMaybes (Stream.fromList ([] :: [Maybe Int])))
        `shouldReturn` []

-------------------------------------------------------------------------------
-- Either Streams
-------------------------------------------------------------------------------

testCatLefts :: Expectation
testCatLefts =
    toList (Stream.catLefts
                (Stream.fromList [Left 1, Right "a", Left 2, Right "b" :: Either Int String]))
        `shouldReturn` [1, 2]

testCatRights :: Expectation
testCatRights =
    toList (Stream.catRights
                (Stream.fromList [Left "a", Right 1, Left "b", Right 2 :: Either String Int]))
        `shouldReturn` [1, 2]

testCatEithers :: Expectation
testCatEithers =
    toList (Stream.catEithers
                (Stream.fromList [Left 1, Right 2, Left 3 :: Either Int Int]))
        `shouldReturn` [1, 2, 3]

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

testSplitSepBy_ :: Expectation
testSplitSepBy_ =
    toList (Stream.splitSepBy_ (== '.') Fold.toList
                (Stream.fromList "a.b.c"))
        `shouldReturn` ["a", "b", "c"]

testSplitSepBy_Empty :: Expectation
testSplitSepBy_Empty =
    toList (Stream.splitSepBy_ (== '.') Fold.toList
                (Stream.fromList ""))
        `shouldReturn` []

testSplitSepBy_Consecutive :: Expectation
testSplitSepBy_Consecutive =
    toList (Stream.splitSepBy_ (== '.') Fold.toList
                (Stream.fromList "a..b"))
        `shouldReturn` ["a", "", "b"]

testSplitSepBy_Leading :: Expectation
testSplitSepBy_Leading =
    toList (Stream.splitSepBy_ (== '.') Fold.toList
                (Stream.fromList ".a.b"))
        `shouldReturn` ["", "a", "b"]

testSplitSepBy_Trailing :: Expectation
testSplitSepBy_Trailing =
    toList (Stream.splitSepBy_ (== '.') Fold.toList
                (Stream.fromList "a.b."))
        `shouldReturn` ["a", "b", ""]

-------------------------------------------------------------------------------
-- Delay
-------------------------------------------------------------------------------

testDelay :: Expectation
testDelay = do
    t0 <- getTime Monotonic
    xs <- toList (Stream.delay 0.05 (Stream.fromList [1, 2 :: Int]))
    t1 <- getTime Monotonic
    xs `shouldBe` [1, 2]
    let NanoSecond64 elapsed = fromRelTime64 (diffAbsTime64 t1 t0)
    elapsed `shouldSatisfy` (>= 50000000)

testDelayPre :: Expectation
testDelayPre = do
    t0 <- getTime Monotonic
    xs <- toList (Stream.delayPre 0.05 (Stream.fromList [1, 2 :: Int]))
    t1 <- getTime Monotonic
    xs `shouldBe` [1, 2]
    let NanoSecond64 elapsed = fromRelTime64 (diffAbsTime64 t1 t0)
    elapsed `shouldSatisfy` (>= 50000000)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Transform"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do
#ifdef DEVBUILD
    it "scanx is strict enough" checkScanxStrictness
#endif
    it "scanl' is strict enough" checkScanl'Strictness
    it "scanlM' is strict enough" (checkScanlMStrictness scanlM'StrictCheck)

    describe "Mapping Effects" $ do
        it "sequence" testSequence
        it "tapOffsetEvery" testTapOffsetEvery
        it "trace_" testTrace_

    describe "Folding into a Stream" $ do
        it "foldlS" testFoldlS
        it "foldlS empty" testFoldlSEmpty

    describe "Composable Scans" $ do
        it "scanl (Scanl)" testScanlScanl
        it "scanl (Scanl) empty" testScanlScanlEmpty
        it "postscanl" testPostscanl
        it "postscanl empty" testPostscanlEmpty
        it "scanlMany" testScanlMany
        it "scanr" testScanr
        it "pipe map" testPipe
        it "pipe filter" testPipeFilter

    describe "Prescans" $ do
        it "prescanl'" testPrescanl'
        it "prescanlM'" testPrescanlM'

    describe "Lazy Scans" $ do
        it "scanlBy" testScanlBy
        it "scanlM" testScanlM
        it "scanl1" testScanl1
        it "scanl1M" testScanl1M
        it "postscanlBy" testPostscanlBy
        it "postscanlM" testPostscanlM

    describe "Advanced Scans" $ do
        it "postscanlx'" testPostscanlx'
        it "scanlx'" testScanlx'
        it "scanlMx'" testScanlMx'
        it "postscanlMAfter'" testPostscanlMAfter'
        it "scanlMAfter'" testScanlMAfter'
        it "postscanlMx'" testPostscanlMx'

    describe "Filtering" $ do
        it "with" testWith
        it "postscanlMaybe" testPostscanlMaybe
        it "uniqBy" testUniqBy
        it "uniqBy custom" testUniqByCustom

    describe "Sampling" $ do
        it "sampleFromThen" testSampleFromThen
        it "sampleFromThen with offset" testSampleFromThenOffset

    describe "Trimming" $ do
        it "initNonEmpty" testInitNonEmpty
        it "initNonEmpty single" testInitNonEmptySingle
        it "tailNonEmpty" testTailNonEmpty
        it "tailNonEmpty single" testTailNonEmptySingle

    describe "Inserting" $ do
        it "intersperseEndByM" testIntersperseEndByM
        it "intersperseEndByM single" testIntersperseEndByMSingle
        it "intersperseEndByEveryM" testIntersperseEndByEveryM
        it "intersperseM_" testIntersperseM_
        it "intersperseEndByM_" testIntersperseEndByM_
        it "intersperseBeginByM_" testIntersperseBeginByM_

    describe "Position Indexing" $ do
        it "indexed" testIndexed
        it "indexed empty" testIndexedEmpty
        it "indexedR" testIndexedR

    describe "Time Indexing" $ do
        it "timestampWith produces elements" testTimestampWith
        it "timestamped produces elements" testTimestamped
        it "timeIndexWith produces elements" testTimeIndexWith
        it "timeIndexed produces elements" testTimeIndexed

    describe "Rolling Map" $ do
        it "rollingMap" testRollingMap
        it "rollingMapM" testRollingMapM
        it "rollingMap2" testRollingMap2
        it "rollingMap single" testRollingMapSingle

    describe "Maybe Streams" $ do
        it "catMaybes" testCatMaybes
        it "catMaybes empty" testCatMaybesEmpty

    describe "Either Streams" $ do
        it "catLefts" testCatLefts
        it "catRights" testCatRights
        it "catEithers" testCatEithers

    describe "splitSepBy_" $ do
        it "splits on separator" testSplitSepBy_
        it "empty stream" testSplitSepBy_Empty
        it "consecutive separators" testSplitSepBy_Consecutive
        it "leading separator" testSplitSepBy_Leading
        it "trailing separator" testSplitSepBy_Trailing

    describe "Delay" $ do
        it "delay introduces inter-element delay" testDelay
        it "delayPre introduces pre-element delay" testDelayPre
