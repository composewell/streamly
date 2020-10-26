-- |
-- Module      : Streamly.Test.Prelude.Parallel
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Parallel where

import Data.List (sort)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Test.Hspec.QuickCheck
import Test.Hspec as H

import Streamly.Prelude
import qualified Streamly.Prelude as S

import Streamly.Test.Prelude.Common

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let parallelCommonOps :: IsStream t => [(String, ParallelT m a -> t m a)]
        parallelCommonOps = []
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", parallely . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", parallely . maxBuffer (-1))]
#endif
    let parallelOps :: IsStream t
            => ((ParallelT IO a -> t IO a) -> Spec) -> Spec
        parallelOps spec = mapOps spec $ makeOps parallely <> parallelCommonOps

    describe "Construction" $ do
        parallelOps $ prop "parallely replicateM" . constructWithReplicateM

    describe "Functor operations" $ do
        parallelOps $ functorOps S.fromFoldable "parallely" sortEq
        parallelOps $ functorOps folded "parallely folded" sortEq

    describe "Monoid operations" $ do
        parallelOps $ monoidOps "parallely" mempty sortEq

    describe "Parallel loops" $ loops parallely sort sort

    describe "Bind and Monoidal composition combinations" $ do
        -- XXX Taking a long time when parallelOps is used.
        bindAndComposeSimpleOps "Parallel" sortEq parallely
        bindAndComposeHierarchyOps "Parallel" parallely
        parallelOps $ nestTwoStreams "Parallel" sort sort
        parallelOps $ nestTwoStreamsApp "Parallel" sort sort

    describe "Semigroup operations" $ do
        parallelOps $ semigroupOps "parallely" sortEq

    describe "Applicative operations" $ do
        parallelOps $ applicativeOps folded "parallely applicative folded" sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
        parallelOps $ prop "zip monadic parallely" . zipMonadic S.fromFoldable (==)
        parallelOps $ prop "zip monadic parallely folded" . zipMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        parallelOps $ prop "parallely monad then" . monadThen S.fromFoldable sortEq
        parallelOps $ prop "parallely monad then folded" . monadThen folded sortEq
        parallelOps $ prop "parallely monad bind" . monadBind S.fromFoldable sortEq
        parallelOps $ prop "parallely monad bind folded" . monadBind folded sortEq

    describe "Stream transform and combine operations" $ do
        parallelOps $ transformCombineOpsCommon S.fromFoldable "parallely" sortEq
        parallelOps $ transformCombineOpsCommon folded "parallely" sortEq

    describe "Stream elimination operations" $ do
        parallelOps $ eliminationOps S.fromFoldable "parallely"
        parallelOps $ eliminationOps folded "parallely folded"
        parallelOps $ eliminationOpsWord8 S.fromFoldable "parallely"
        parallelOps $ eliminationOpsWord8 folded "parallely folded"

    -- test both (<>) and mappend to make sure we are using correct instance
    -- for Monoid that is using the right version of semigroup. Instance
    -- deriving can cause us to pick wrong instances sometimes.

#ifdef DEVBUILD
    describe "Parallel (<>) time order check" $ parallelCheck parallely (<>)
    describe "Parallel mappend time order check" $ parallelCheck parallely mappend
#endif

    describe "Composed MonadThrow parallely" $ composeWithMonadThrow parallely

#ifdef DEVBUILD
    -- parallely fails on CI machines, may need more difference in times of
    -- the events, but that would make tests even slower.
    it "take 1 parallely" $ checkCleanup 3 parallely (S.take 1)
    it "takeWhile (< 0) parallely" $ checkCleanup 3 parallely (S.takeWhile (< 0))
#endif
