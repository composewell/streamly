-- |
-- Module      : Streamly.Test.Prelude.WAsync
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.WAsync where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Test.Hspec.QuickCheck
import Test.Hspec as H

import Streamly
import qualified Streamly.Prelude as S

import Streamly.Test.Prelude

main :: IO ()
main = hspec
    $ H.parallel
#ifdef COVERAGE_BUILD
    $ modifyMaxSuccess (const 10)
#endif
    $ do
    let wAsyncOps :: IsStream t => ((WAsyncT IO a -> t IO a) -> Spec) -> Spec
        wAsyncOps spec = mapOps spec $ makeOps wAsyncly
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", wAsyncly . maxBuffer (-1))]
#endif

    describe "Construction" $ do
        wAsyncOps $ prop "wAsyncly replicateM" . constructWithReplicateM
        -- XXX add tests for fromIndices

    describe "Functor operations" $ do
        wAsyncOps $ functorOps S.fromFoldable "wAsyncly" sortEq
        wAsyncOps $ functorOps folded "wAsyncly folded" sortEq

    describe "Monoid operations" $ do
        wAsyncOps $ monoidOps "wAsyncly" mempty sortEq

    describe "Semigroup operations" $ do
        wAsyncOps $ semigroupOps "wAsyncly" sortEq

    describe "Applicative operations" $ do
        wAsyncOps $ applicativeOps S.fromFoldable "wAsyncly applicative" sortEq
        wAsyncOps $ applicativeOps folded "wAsyncly applicative folded" sortEq

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $
        -- We test only the serial zip with serial streams and the parallel
        -- stream, because the rate setting in these streams can slow down
        -- zipAsync.
     do
        wAsyncOps $
            prop "zip monadic wAsyncly" . zipAsyncMonadic S.fromFoldable (==)
        wAsyncOps $ prop "zip monadic wAsyncly folded" . zipAsyncMonadic folded (==)

    -- XXX add merge tests like zip tests
    -- for mergeBy, we can split a list randomly into two lists and
    -- then merge them, it should result in original list
    -- describe "Merge operations" $ do

    describe "Monad operations" $ do
        wAsyncOps $ prop "wAsyncly monad then" . monadThen S.fromFoldable sortEq
        wAsyncOps $ prop "wAsyncly monad then folded" . monadThen folded sortEq
        wAsyncOps $ prop "wAsyncly monad bind" . monadBind S.fromFoldable sortEq
        wAsyncOps $ prop "wAsyncly monad bind folded" . monadBind folded sortEq

    describe "Stream transform and combine operations" $ do
        wAsyncOps $ transformCombineOpsCommon S.fromFoldable "wAsyncly" sortEq
        wAsyncOps $ transformCombineOpsCommon folded "wAsyncly" sortEq

    describe "Stream elimination operations" $ do
        wAsyncOps $ eliminationOps S.fromFoldable "wAsyncly"
        wAsyncOps $ eliminationOps folded "wAsyncly folded"
        wAsyncOps $ eliminationOpsWord8 S.fromFoldable "wAsyncly"
        wAsyncOps $ eliminationOpsWord8 folded "wAsyncly folded"
