{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Prelude.Async
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Async (main) where

import Control.Concurrent (threadDelay)
import Data.List (sort)
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.Hspec as H

import Streamly.Prelude
import qualified Streamly.Prelude as S

import Data.IORef
import Streamly.Test.Common
import Streamly.Test.Prelude.Common


moduleName :: String
moduleName = "Prelude.Async"

constructfromAsyncSingleThread ::
    S.AsyncT IO Int -> S.AsyncT IO Int-> [Int] -> Property
constructfromAsyncSingleThread s1 s2 res =
    withMaxSuccess maxTestCount $
    monadicIO $ do
        x <-  run
            $ S.toList
            $ S.fromAsync
            $ S.maxThreads 1
            $ s1 `S.async` s2
        equals (==) x res

concurrentApplicative :: IO ()
concurrentApplicative = do
    ref <- newIORef []
    let action i = modifyIORef ref (++ [i]) >> return (i :: Int)
        s1 = S.fromEffect (threadDelay 2000000 >> action 1)
        s2 = S.fromEffect (threadDelay 1000000 >> action 2)
    res <- S.toList $ S.fromZipAsync $ (,) <$> s1 <*> s2
    refVal <- readIORef ref
    res `shouldBe` [(1, 2)]
    refVal `shouldBe` [2, 1]

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    let asyncOps :: IsStream t => ((AsyncT IO a -> t IO a) -> Spec) -> Spec
        asyncOps spec = mapOps spec $ makeOps fromAsync
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", fromAsync . maxBuffer (-1))]
#endif

    describe "Construction" $ do
        asyncOps    $ prop "asyncly replicateM" . constructWithReplicateM
        asyncOps $ prop "asyncly cons" . constructWithCons S.cons
        asyncOps $ prop "asyncly consM" . constructWithConsM S.consM sort
        asyncOps $ prop "asyncly (.:)" . constructWithCons (S..:)
        asyncOps $ prop "asyncly (|:)" . constructWithConsM (S.|:) sort
        prop "asyncSingleThreaded" $
            constructfromAsyncSingleThread
            (S.fromList [1,2,3,4,5])
            (S.fromList [6,7,8,9,10])
            [1,2,3,4,5,6,7,8,9,10]

    describe "Functor operations" $ do
        asyncOps     $ functorOps S.fromFoldable "asyncly" sortEq
        asyncOps     $ functorOps folded "asyncly folded" sortEq

    describe "Monoid operations" $ do
        asyncOps     $ monoidOps "asyncly" mempty sortEq

    describe "Async loops" $ loops fromAsync sort sort

    describe "Bind and Monoidal composition combinations" $ do
        asyncOps $ bindAndComposeSimpleOps "Async" sortEq
        asyncOps $ bindAndComposeHierarchyOps "Async"
        asyncOps $ nestTwoStreams "Async" sort sort
        asyncOps $ nestTwoStreamsApp "Async" sort sort

    describe "Semigroup operations" $ do
        asyncOps     $ semigroupOps "asyncly" sortEq

    describe "Applicative operations" $ do
        asyncOps $ applicativeOps S.fromFoldable "asyncly applicative" sortEq
        asyncOps $ applicativeOps folded "asyncly applicative folded" sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        asyncOps    $ prop "zip applicative asyncly" . zipAsyncApplicative S.fromFoldable (==)
        asyncOps    $ prop "zip applicative asyncly folded" . zipAsyncApplicative folded (==)
        asyncOps    $ prop "zip monadic asyncly" . zipAsyncMonadic S.fromFoldable (==)
        asyncOps    $ prop "zip monadic asyncly folded" . zipAsyncMonadic folded (==)
        it "zip monadic asyncly order" concurrentApplicative

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        asyncOps    $ prop "asyncly monad then" . monadThen S.fromFoldable sortEq
        asyncOps    $ prop "asyncly monad then folded" . monadThen folded sortEq
        asyncOps    $ prop "asyncly monad bind" . monadBind S.fromFoldable sortEq
        asyncOps    $ prop "asyncly monad bind folded"   . monadBind folded sortEq

    describe "Stream transform and combine operations" $ do
        asyncOps     $ transformCombineOpsCommon S.fromFoldable "asyncly" sortEq
        asyncOps     $ transformCombineOpsCommon folded "asyncly" sortEq

    describe "Stream elimination operations" $ do
        asyncOps     $ eliminationOps S.fromFoldable "asyncly"
        asyncOps     $ eliminationOps folded "asyncly folded"
        asyncOps     $ eliminationOpsWord8 S.fromFoldable "asyncly"
        asyncOps     $ eliminationOpsWord8 folded "asyncly folded"

    -- test both (<>) and mappend to make sure we are using correct instance
    -- for Monoid that is using the right version of semigroup. Instance
    -- deriving can cause us to pick wrong instances sometimes.

#ifdef DEVBUILD
    describe "Async (<>) time order check" $ parallelCheck fromAsync (<>)
    describe "Async mappend time order check" $ parallelCheck fromAsync mappend
#endif

    describe "Tests for exceptions" $ asyncOps $ exceptionOps "asyncly"
    describe "Composed MonadThrow asyncly" $ composeWithMonadThrow fromAsync

    -- Ad-hoc tests
    it "takes n from stream of streams" $ takeCombined 2 fromAsync
