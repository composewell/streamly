-- |
-- Module      : Streamly.Test.Data.Stream.Type
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Type (main) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Streamly.Internal.Data.Stream (Stream)
import Test.QuickCheck (Property, choose)
import Test.QuickCheck.Monadic (monadicIO, pick)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Stream as Stream
import Streamly.Internal.Data.Unfold (Unfold)
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Hspec as H
import Test.Hspec.QuickCheck

toList :: Monad m => Stream m a -> m [a]
toList = Stream.toList

max_length :: Int
max_length = 1000

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = Unfold.supplySecond b Unfold.enumerateFromToIntegral
    ls <- toList $ Stream.unfold unf a
    return $ ls == [a..b]

testgroupsOf ::  Expectation
testgroupsOf =
    Stream.toList
        (Stream.groupsOf 2 Fold.sum (Stream.enumerateFromTo 1 10))
        `shouldReturn` [3::Int, 7, 11, 15, 19]

testAppendUnfoldLastNonEmpty :: Expectation
testAppendUnfoldLastNonEmpty =
    Stream.toList
        (Stream.appendUnfoldLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3, 30, 300]

    where

    trailer = Unfold.lmap (maybe [-1] (\x -> [x * 10, x * 100])) Unfold.fromList

testAppendUnfoldLastEmpty :: Expectation
testAppendUnfoldLastEmpty =
    Stream.toList
        (Stream.appendUnfoldLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` [-1]

    where

    trailer = Unfold.lmap (maybe [-1] (\x -> [x * 10, x * 100])) Unfold.fromList

testAppendMapLastNonEmpty :: Expectation
testAppendMapLastNonEmpty =
    Stream.toList
        (Stream.appendMapLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3, 30, 300]

    where

    trailer =
        maybe (Stream.fromList [-1]) (\x -> Stream.fromList [x * 10, x * 100])

testAppendMapLastEmpty :: Expectation
testAppendMapLastEmpty =
    Stream.toList
        (Stream.appendMapLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` [-1]

    where

    trailer =
        maybe (Stream.fromList [-1]) (\x -> Stream.fromList [x * 10, x * 100])

testUnfoldLastNonEmpty :: Expectation
testUnfoldLastNonEmpty =
    Stream.toList
        (Stream.unfoldLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 30, 300]

    where

    trailer = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testUnfoldLastEmpty :: Expectation
testUnfoldLastEmpty =
    Stream.toList
        (Stream.unfoldLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    trailer = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testConcatMapLastNonEmpty :: Expectation
testConcatMapLastNonEmpty =
    Stream.toList
        (Stream.concatMapLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 30, 300]

    where

    trailer =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testConcatMapLastEmpty :: Expectation
testConcatMapLastEmpty =
    Stream.toList
        (Stream.concatMapLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    trailer =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testAppendIfEmptyNonEmpty :: Expectation
testAppendIfEmptyNonEmpty =
    Stream.toList
        (Stream.appendIfEmpty
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4]))
        `shouldReturn` [1, 2]

testAppendIfEmptyEmpty :: Expectation
testAppendIfEmptyEmpty =
    Stream.toList
        (Stream.appendIfEmpty
            (Stream.fromList ([] :: [Int]))
            (Stream.fromList [3, 4]))
        `shouldReturn` [3, 4]

testUnfoldFirstNonEmpty :: Expectation
testUnfoldFirstNonEmpty =
    Stream.toList
        (Stream.unfoldFirst header (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [10, 100, 2, 3]

    where

    header = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testUnfoldFirstEmpty :: Expectation
testUnfoldFirstEmpty =
    Stream.toList
        (Stream.unfoldFirst header (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    header = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testConcatMapFirstNonEmpty :: Expectation
testConcatMapFirstNonEmpty =
    Stream.toList
        (Stream.concatMapFirst header (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [10, 100, 2, 3]

    where

    header =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testConcatMapFirstEmpty :: Expectation
testConcatMapFirstEmpty =
    Stream.toList
        (Stream.concatMapFirst header (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    header =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testCrossWithM :: Expectation
testCrossWithM =
    Stream.toList
        (Stream.crossWithM (\a b -> return (a, b))
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int]))
        `shouldReturn` [(1, 3), (1, 4), (2, 3), (2, 4)]

testCrossWith :: Expectation
testCrossWith =
    Stream.toList
        (Stream.crossWith (,)
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int]))
        `shouldReturn` [(1, 3), (1, 4), (2, 3), (2, 4)]

testFairCrossWithM :: Expectation
testFairCrossWithM = do
    result <- fmap sort $ Stream.toList $
        Stream.fairCrossWithM (\a b -> return (a, b))
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int])
    result `shouldBe` sort [(1, 3), (1, 4), (2, 3), (2, 4)]

testInterleave :: Expectation
testInterleave =
    Stream.toList
        (Stream.interleave
            (Stream.fromList [1, 3, 5 :: Int])
            (Stream.fromList [2, 4, 6]))
        `shouldReturn` [1, 2, 3, 4, 5, 6]

testAltBfsUnfoldEach :: Expectation
testAltBfsUnfoldEach = do
    result <- fmap sort $ Stream.toList $
        Stream.altBfsUnfoldEach
            (Unfold.lmap (\n -> (1, n)) Unfold.enumerateFromToIntegral)
            (Stream.fromList [2, 3 :: Int])
    result `shouldBe` sort [1, 2, 1, 2, 3]

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

testNilM :: Expectation
testNilM = do
    ref <- newIORef (0 :: Int)
    Stream.toList (Stream.nilM (writeIORef ref 1))
        `shouldReturn` ([] :: [Int])
    readIORef ref `shouldReturn` 1

testConsM :: Expectation
testConsM =
    Stream.toList (Stream.consM (return 1) (Stream.fromList [2, 3 :: Int]))
        `shouldReturn` [1, 2, 3]

testFromTuple :: Expectation
testFromTuple =
    Stream.toList (Stream.fromTuple (1, 2 :: Int))
        `shouldReturn` [1, 2]

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

testUncons :: Expectation
testUncons = do
    r <- Stream.uncons (Stream.fromList [1, 2, 3 :: Int])
    case r of
        Nothing -> expectationFailure "expected non-empty stream"
        Just (h, t) -> do
            h `shouldBe` 1
            Stream.toList t `shouldReturn` [2, 3]

testUnconsEmpty :: Expectation
testUnconsEmpty = do
    r <- Stream.uncons (Stream.fromList ([] :: [Int]))
    case r of
        Nothing -> return ()
        Just _ -> expectationFailure "expected empty stream"

testFoldAddLazy :: Expectation
testFoldAddLazy = do
    let streams = [Stream.fromList [1..5], Stream.fromList [6..10 :: Int]]
    let f = foldl Stream.foldAddLazy Fold.sum streams
    Stream.fold f Stream.nil `shouldReturn` 55

testFoldAdd :: Expectation
testFoldAdd = do
    f <- Stream.foldAdd Fold.sum (Stream.fromList [1..5 :: Int])
    Stream.fold f (Stream.fromList [6..10]) `shouldReturn` 55

testFoldBreakEitherLeft :: Expectation
testFoldBreakEitherLeft = do
    r <- Stream.foldBreakEither Fold.sum (Stream.fromList [1..5 :: Int])
    case r of
        Left _ -> return ()
        Right _ -> expectationFailure "expected Left (fold incomplete)"

testFoldBreakEitherRight :: Expectation
testFoldBreakEitherRight = do
    r <- Stream.foldBreakEither (Fold.take 2 Fold.sum) (Stream.fromList [1..5 :: Int])
    case r of
        Right (s, rest) -> do
            s `shouldBe` (3 :: Int)
            Stream.toList rest `shouldReturn` [3, 4, 5]
        Left _ -> expectationFailure "expected Right (fold completed)"

testFoldlM :: Expectation
testFoldlM =
    Stream.foldlM' (\acc x -> return (acc + x)) (return 0)
        (Stream.fromList [1..5 :: Int])
        `shouldReturn` 15

testFoldlx :: Expectation
testFoldlx =
    Stream.foldlx' (+) (0 :: Int) id (Stream.fromList [1..5 :: Int])
        `shouldReturn` 15

testFoldlMx :: Expectation
testFoldlMx =
    Stream.foldlMx' (\acc x -> return (acc + x)) (return 0) return
        (Stream.fromList [1..5 :: Int])
        `shouldReturn` (15 :: Int)

testFoldrM :: Expectation
testFoldrM =
    Stream.foldrM (\x acc -> (x:) <$> acc) (return [])
        (Stream.fromList [1..5 :: Int])
        `shouldReturn` [1, 2, 3, 4, 5]

testFoldrMx :: Expectation
testFoldrMx =
    Stream.foldrMx (\x acc -> (x:) <$> acc) (return []) id
        (Stream.fromList [1..5 :: Int])
        `shouldReturn` [1, 2, 3, 4, 5]

testFoldr :: Expectation
testFoldr =
    Stream.foldr (:) [] (Stream.fromList [1..5 :: Int])
        `shouldReturn` [1, 2, 3, 4, 5]

testFoldrS :: Expectation
testFoldrS =
    Stream.toList
        (Stream.foldrS Stream.cons Stream.nil (Stream.fromList [1..5 :: Int]))
        `shouldReturn` [1, 2, 3, 4, 5]

testHeadElse :: Expectation
testHeadElse = do
    Stream.headElse 0 (Stream.fromList [1, 2, 3 :: Int]) `shouldReturn` 1
    Stream.headElse 0 (Stream.fromList []) `shouldReturn` (0 :: Int)

-------------------------------------------------------------------------------
-- Stateful Filters
-------------------------------------------------------------------------------

testTakeEndBy_ :: Expectation
testTakeEndBy_ =
    Stream.toList (Stream.takeEndBy_ (== 3) (Stream.fromList [1..5 :: Int]))
        `shouldReturn` [1, 2]

testTakeEndByM :: Expectation
testTakeEndByM =
    Stream.toList
        (Stream.takeEndByM (return . (== 3)) (Stream.fromList [1..5 :: Int]))
        `shouldReturn` [1, 2, 3]

-------------------------------------------------------------------------------
-- Combining Two Streams
-------------------------------------------------------------------------------

testCross :: Expectation
testCross =
    Stream.toList
        (Stream.cross
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int]))
        `shouldReturn` [(1, 3), (1, 4), (2, 3), (2, 4)]

testFairCrossWith :: Expectation
testFairCrossWith = do
    result <- fmap sort $ Stream.toList $
        Stream.fairCrossWith (,)
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int])
    result `shouldBe` sort [(1, 3), (1, 4), (2, 3), (2, 4)]

testFairCross :: Expectation
testFairCross = do
    result <- fmap sort $ Stream.toList $
        Stream.fairCross
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int])
    result `shouldBe` sort [(1, 3), (1, 4), (2, 3), (2, 4)]

testLoop :: Expectation
testLoop =
    Stream.toList
        (Stream.loop
            (Stream.fromList [3, 4 :: Int])
            (Stream.fromList [1, 2 :: Int]))
        `shouldReturn` [(1, 3), (2, 3), (1, 4), (2, 4)]

testLoopBy :: Expectation
testLoopBy =
    Stream.toList
        (Stream.loopBy
            Unfold.fromList
            [3, 4 :: Int]
            (Stream.fromList [1, 2 :: Int]))
        `shouldReturn` [(1, 3), (1, 4), (2, 3), (2, 4)]

-------------------------------------------------------------------------------
-- UnfoldCross
-------------------------------------------------------------------------------

testUnfoldCross :: Expectation
testUnfoldCross =
    Stream.toList
        (Stream.unfoldCross
            Unfold.identity
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4 :: Int]))
        `shouldReturn` [(1, 3), (1, 4), (2, 3), (2, 4)]

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

testConcatEffect :: Expectation
testConcatEffect =
    Stream.toList
        (Stream.concatEffect (return (Stream.fromList [1..5 :: Int])))
        `shouldReturn` [1..5]

testConcat :: Expectation
testConcat =
    Stream.toList
        (Stream.concat
            (Stream.fromList
                [ Stream.fromList [1, 2]
                , Stream.fromList [3, 4 :: Int]
                ]))
        `shouldReturn` [1, 2, 3, 4]

testConcatForM :: Expectation
testConcatForM =
    Stream.toList
        (Stream.concatForM
            (Stream.fromList [1, 2, 3 :: Int])
            (\x -> return (Stream.fromPure x)))
        `shouldReturn` [1, 2, 3]

-------------------------------------------------------------------------------
-- Unfold Iterate / Concat Iterate (DFS/BFS tree traversal)
--
-- Tree used: 1->[2,3], 2->[4,5], 3->[6,7], leaves have no children.
-- DFS order:               1, 2, 4, 5, 3, 6, 7
-- BFS order:               1, 2, 3, 4, 5, 6, 7
-- altBfs order (reversed): 1, 2, 3, 6, 7, 4, 5
-------------------------------------------------------------------------------

treeChildren :: Int -> [Int]
treeChildren n
    | n == 1    = [2, 3]
    | n == 2    = [4, 5]
    | n == 3    = [6, 7]
    | otherwise = []

treeUnfold :: Unfold IO Int Int
treeUnfold = Unfold.lmap treeChildren Unfold.fromList

treeConcatF :: Int -> Maybe (Stream IO Int)
treeConcatF n =
    let cs = treeChildren n
     in if null cs then Nothing else Just (Stream.fromList cs)

testUnfoldIterate :: Expectation
testUnfoldIterate = do
    result <- Stream.toList $
        Stream.unfoldIterate treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 4, 5, 3, 6, 7]

_testBfsUnfoldIterate :: Expectation
_testBfsUnfoldIterate = do
    result <- Stream.toList $
        Stream.bfsUnfoldIterate treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 4, 5, 6, 7]

_testAltBfsUnfoldIterate :: Expectation
_testAltBfsUnfoldIterate = do
    result <- Stream.toList $
        Stream.altBfsUnfoldIterate treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 6, 7, 4, 5]

_testConcatIterate :: Expectation
_testConcatIterate = do
    result <- Stream.toList $
        Stream.concatIterate treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 4, 5, 3, 6, 7]

_testBfsConcatIterate :: Expectation
_testBfsConcatIterate = do
    result <- Stream.toList $
        Stream.bfsConcatIterate treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 4, 5, 6, 7]

_testAltBfsConcatIterate :: Expectation
_testAltBfsConcatIterate = do
    result <- Stream.toList $
        Stream.altBfsConcatIterate treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 6, 7, 4, 5]

testConcatIterateScan :: Expectation
testConcatIterateScan = do
    -- generate: from acc, produce stream [acc+1, acc+2] while acc < 6
    -- scanner:  acc + element
    -- acc=0 -> stream [1,2], scan: 0+1=1, 1+2=3  (yields 1, 2)
    -- acc=3 -> stream [4,5], scan: 3+4=7, 7+5=12 (yields 4, 5)
    -- acc=12 >= 6 -> stop
    let generate acc =
            if acc >= 6
            then return Nothing
            else return $ Just (acc, Stream.fromList [acc + 1, acc + 2])
    result <- Stream.toList $
        Stream.concatIterateScan
            (\acc x -> return (acc + x))
            generate
            (0 :: Int)
    result `shouldBe` [1, 2, 4, 5]

-------------------------------------------------------------------------------
-- Fold Many
-------------------------------------------------------------------------------

testFoldManyPost :: Expectation
testFoldManyPost = do
    let f = Fold.take 2 Fold.toList
    Stream.toList (Stream.foldManyPost f (Stream.fromList ([] :: [Int])))
        `shouldReturn` [[]]
    Stream.toList (Stream.foldManyPost f (Stream.fromList [1..4 :: Int]))
        `shouldReturn` [[1, 2], [3, 4], []]
    Stream.toList (Stream.foldManyPost f (Stream.fromList [1..5 :: Int]))
        `shouldReturn` [[1, 2], [3, 4], [5]]

testRefoldMany :: Expectation
testRefoldMany = do
    -- Groups of 2, summed from 0 each time: [1+2=3, 3+4=7, 5+6=11]
    let rf = Refold.take 2 (Refold.foldl' (+))
    result <- Stream.toList $
        Stream.refoldMany rf (return (0 :: Int)) (Stream.fromList [1..6 :: Int])
    result `shouldBe` [3, 7, 11]

testRefoldIterateM :: Expectation
testRefoldIterateM = do
    -- Takes 2, feeds output as next initial value:
    -- init=0: 0+1+2=3 (yield 3)
    -- init=3: 3+3+4=10 (yield 10)
    -- init=10: 10+5+6=21 (yield 21)
    let rf = Refold.take 2 (Refold.foldl' (+))
    result <- Stream.toList $
        Stream.refoldIterateM rf (return (0 :: Int)) (Stream.fromList [1..6 :: Int])
    result `shouldBe` [3, 10, 21]

-------------------------------------------------------------------------------
-- Fold Iterate
-------------------------------------------------------------------------------

testBfsReduceIterate :: Expectation
testBfsReduceIterate = do
    r1 <- Stream.bfsReduceIterate (\a b -> return (a + b :: Int))
            (Stream.fromList [1..4 :: Int])
    r1 `shouldBe` Just 10
    r2 <- Stream.bfsReduceIterate (\a b -> return (a + b :: Int))
            (Stream.fromList ([] :: [Int]))
    r2 `shouldBe` Nothing

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

testIndexEndBy_ :: Expectation
testIndexEndBy_ =
    Stream.toList
        (Stream.indexEndBy_ (== '/') (Stream.fromList "/home/harendra"))
        `shouldReturn` [(0, 0), (1, 4), (6, 8)]

testIndexEndBy :: Expectation
testIndexEndBy =
    Stream.toList
        (Stream.indexEndBy (== '/') (Stream.fromList "/home/harendra"))
        `shouldReturn` [(0, 1), (1, 5), (6, 8)]

-------------------------------------------------------------------------------
-- Multi-stream folds
-------------------------------------------------------------------------------

testTakeCommonPrefixBy :: Expectation
testTakeCommonPrefixBy = do
    Stream.toList
        (Stream.takeCommonPrefixBy (==)
            (Stream.fromList [1, 2, 3, 4 :: Int])
            (Stream.fromList [1, 2, 5, 6 :: Int]))
        `shouldReturn` [1, 2]
    Stream.toList
        (Stream.takeCommonPrefixBy (==)
            (Stream.fromList [1, 2, 3 :: Int])
            (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3]
    Stream.toList
        (Stream.takeCommonPrefixBy (==)
            (Stream.fromList [1, 2, 3 :: Int])
            (Stream.fromList [4, 5, 6 :: Int]))
        `shouldReturn` []

moduleName :: String
moduleName = "Data.Stream"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    describe "Runners" $ do
        -- XXX use an IORef to store and check the side effects
        it "simple serially" $
            Stream.fold Fold.drain
            (Stream.fromPure (0 :: Int)) `shouldReturn` ()
        it "simple serially with IO" $
            Stream.fold Fold.drain
            (Stream.fromEffect $ putStrLn "hello") `shouldReturn` ()

    describe "Construction" $ do
    {-
        -- XXX To be fixed. Copied from old stream Prelude tests.
        -- Add all the construction tests for all stream types.
        serialOps   $ prop "serially repeat" . constructWithRepeat
        serialOps   $ prop "serially repeatM" . constructWithRepeatM
        serialOps   $ prop "serially replicate" . constructWithReplicate
        serialOps   $ prop "serially replicateM" . constructWithReplicateM
        serialOps   $ prop "serially intFromThenTo" .
                            constructWithIntFromThenTo
        serialOps   $ prop "serially DoubleFromThenTo" .
                            constructWithDoubleFromThenTo
        serialOps   $ prop "serially iterate" . constructWithIterate
        -- XXX test for all types of streams
        serialOps   $ prop "serially iterateM" . constructWithIterateM
        serialOps $ prop "serially enumerate" . constructWithEnumerate id
        serialOps $ prop "serially enumerateTo" . constructWithEnumerateTo id
        serialOps $ prop "serially fromIndices" . constructWithFromIndices
        serialOps $ prop "serially fromIndicesM" . constructWithFromIndicesM
        serialOps $ prop "serially fromList" . constructWithFromList id
        serialOps $ prop "serially fromListM" . constructWithFromListM id
        serialOps $ prop "serially unfoldr" . constructWithUnfoldr id
        serialOps $ prop "serially fromPure" . constructWithFromPure id
        serialOps $ prop "serially fromEffect" . constructWithFromEffect id
        serialOps $ prop "serially cons" . constructWithCons Stream.cons
        serialOps $ prop "serially consM" . constructWithConsM Stream.consM id
        -}

        describe "From Generators" $ do
            prop "unfold" unfold

    {-
    describe "Simple Operations" $ serialOps simpleOps

    describe "Functor operations" $ do
        serialOps    $ functorOps (Stream.fromFoldable) "serially" (==)
        serialOps    $ functorOps folded "serially folded" (==)

    describe "Monoid operations" $ do
        serialOps $ monoidOps "serially" mempty (==)

    describe "Serial loops" $ loops fromSerial id reverse

    describe "Bind and Monoidal composition combinations" $ do
        -- XXX Taking a long time when serialOps is used.
        bindAndComposeSimpleOps "Serial" sortEq fromSerial
        bindAndComposeHierarchyOps "Serial" fromSerial
        serialOps $ nestTwoStreams "Serial" id id
        serialOps $ nestTwoStreamsApp "Serial" id id
        composeAndComposeSimpleSerially "Serial <> " (repeat [1..9]) fromSerial
        composeAndComposeSimpleAheadly "Serial <> " (repeat [1 .. 9]) fromSerial
        composeAndComposeSimpleWSerially
            "Serial <> "
            [[1..9], [1..9], [1,3,2,4,6,5,7,9,8], [1,3,2,4,6,5,7,9,8]]
            fromSerial

    describe "Semigroup operations" $ do
        serialOps $ semigroupOps "serially" (==)
        serialOps $ associativityCheck "serial == <>"

    describe "Applicative operations" $ do
        -- The tests using sorted equality are weaker tests
        -- We need to have stronger unit tests for all those
        -- XXX applicative with three arguments
        serialOps $ applicativeOps (Stream.fromFoldable) "serially" (==)
        serialOps $ applicativeOps folded "serially folded" (==)
        serialOps $ applicativeOps1 (Stream.fromFoldable) "serially" (==)
        serialOps $ applicativeOps1 (Stream.fromFoldable) "serially folded" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        serialOps   $ prop "zip monadic serially" . zipMonadic (Stream.fromFoldable) (==)
        serialOps   $ prop "zip monadic serially folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        serialOps   $ prop "serially monad then" . monadThen (Stream.fromFoldable) (==)
        serialOps   $ prop "serially monad then folded" . monadThen folded (==)
        serialOps   $ prop "serially monad bind" . monadBind (Stream.fromFoldable) (==)
        serialOps   $ prop "serially monad bind folded"  . monadBind folded (==)

    describe "Stream transform and combine operations" $ do
        serialOps    $ transformCombineOpsCommon (Stream.fromFoldable) "serially" (==)
        serialOps    $ transformCombineOpsCommon folded "serially" (==)
        serialOps    $ transformCombineOpsOrdered (Stream.fromFoldable) "serially" (==)
        serialOps    $ transformCombineOpsOrdered folded "serially" (==)
    -}

    -- Construction
    it "nilM" testNilM
    it "consM" testConsM
    it "fromTuple" testFromTuple

    -- scans
    it "indexEndBy_" testIndexEndBy_
    it "indexEndBy" testIndexEndBy
    it "takeEndBy_" testTakeEndBy_
    it "takeEndByM" testTakeEndByM
    it "takeCommonPrefixBy" testTakeCommonPrefixBy

    -- Elimination
    describe "uncons" $ do
        it "non-empty" testUncons
        it "empty" testUnconsEmpty

    it "foldAddLazy" testFoldAddLazy
    it "foldAdd" testFoldAdd

    describe "foldBreakEither" $ do
        it "stream ends first returns Left" testFoldBreakEitherLeft
        it "fold completes returns Right" testFoldBreakEitherRight

    it "foldlM'" testFoldlM
    it "foldlx'" testFoldlx
    it "foldlMx'" testFoldlMx
    it "foldrM" testFoldrM
    it "foldrMx" testFoldrMx
    it "foldr" testFoldr
    it "foldrS" testFoldrS
    it "headElse" testHeadElse

    -- Selective Appends
    describe "appendIfEmpty" $ do
        it "non-empty" testAppendIfEmptyNonEmpty
        it "empty" testAppendIfEmptyEmpty

    describe "appendUnfoldLast" $ do
        it "non-empty" testAppendUnfoldLastNonEmpty
        it "empty" testAppendUnfoldLastEmpty

    describe "appendMapLast" $ do
        it "non-empty" testAppendMapLastNonEmpty
        it "empty" testAppendMapLastEmpty

    -- Selective Concat/Unfold
    describe "unfoldLast" $ do
        it "non-empty" testUnfoldLastNonEmpty
        it "empty" testUnfoldLastEmpty

    describe "concatMapLast" $ do
        it "non-empty" testConcatMapLastNonEmpty
        it "empty" testConcatMapLastEmpty

    describe "unfoldFirst" $ do
        it "non-empty" testUnfoldFirstNonEmpty
        it "empty" testUnfoldFirstEmpty

    describe "concatMapFirst" $ do
        it "non-empty" testConcatMapFirstNonEmpty
        it "empty" testConcatMapFirstEmpty

    -- Concat/Unfold
    it "cross" testCross
    it "crossWith" testCrossWith
    it "crossWithM" testCrossWithM

    it "fairCross" testFairCross
    it "fairCrossWith" testFairCrossWith
    it "fairCrossWithM" testFairCrossWithM
    it "interleave" testInterleave

    it "loop" testLoop
    it "loopBy" testLoopBy

    it "unfoldCross" testUnfoldCross
    it "altBfsUnfoldEach" testAltBfsUnfoldEach

    it "concatEffect" testConcatEffect
    it "concat" testConcat
    it "concatForM" testConcatForM

    -- Splitting
    it "foldManyPost" testFoldManyPost
    it "refoldMany" testRefoldMany
    it "groupsOf" testgroupsOf

    -- tree/graph traversal
    it "unfoldIterate" testUnfoldIterate
    -- XXX Compiling these gets stuck with lot of memory and cpu use
    -- need to investigate. Even though compiling the function themselves is
    -- fine, using them here is what causes the problem.
    {-
    it "concatIterate" _testConcatIterate
    it "bfsConcatIterate" _testBfsConcatIterate
    it "altBfsConcatIterate" _testAltBfsConcatIterate
    it "bfsUnfoldIterate" _testBfsUnfoldIterate
    it "altBfsUnfoldIterate" _testAltBfsUnfoldIterate
    -}

    it "concatIterateScan" testConcatIterateScan
    it "refoldIterateM" testRefoldIterateM
    it "bfsReduceIterate" testBfsReduceIterate
