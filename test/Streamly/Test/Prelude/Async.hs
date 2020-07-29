-- |
-- Module      : Streamly.Test.Prelude.Async
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Async where

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
    let asyncOps :: IsStream t => ((AsyncT IO a -> t IO a) -> Spec) -> Spec
        asyncOps spec = mapOps spec $ makeOps asyncly
#ifndef COVERAGE_BUILD
            <> [("maxBuffer (-1)", asyncly . maxBuffer (-1))]
#endif

    describe "Construction" $ do
        asyncOps    $ prop "asyncly replicateM" . constructWithReplicateM

    describe "Functor operations" $ do
        asyncOps     $ functorOps S.fromFoldable "asyncly" sortEq
        asyncOps     $ functorOps folded "asyncly folded" sortEq

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
        asyncOps    $ prop "zip monadic asyncly" . zipAsyncMonadic S.fromFoldable (==)
        asyncOps    $ prop "zip monadic asyncly folded" . zipAsyncMonadic folded (==)

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
