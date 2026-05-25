-- |
-- Module      : Streamly.Test.Data.Stream.Type
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Type (main) where

import Streamly.Internal.Data.Stream (Stream)
import Test.QuickCheck (Property, choose)
import Test.QuickCheck.Monadic (monadicIO, pick)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
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
    let unf = Unfold.second b Unfold.enumerateFromToIntegral
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

    describe "Tests for Stream.groupsOf" $ do
        prop "testgroupsOf" testgroupsOf

    describe "Tests for Stream.appendUnfoldLast" $ do
        prop "testAppendUnfoldLastNonEmpty" testAppendUnfoldLastNonEmpty
        prop "testAppendUnfoldLastEmpty" testAppendUnfoldLastEmpty

    describe "Tests for Stream.appendMapLast" $ do
        prop "testAppendMapLastNonEmpty" testAppendMapLastNonEmpty
        prop "testAppendMapLastEmpty" testAppendMapLastEmpty
