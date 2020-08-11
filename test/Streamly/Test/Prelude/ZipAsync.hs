-- |
-- Module      : Streamly.Test.Prelude.ZipAsync
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.ZipAsync where

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
    -- Note, the "pure" of applicative Zip streams generates and infinite
    -- stream and therefore maxBuffer (-1) must not be used for that case.
    let zipAsyncOps :: IsStream t => ((ZipAsyncM IO a -> t IO a) -> Spec) -> Spec
        zipAsyncOps spec = mapOps spec $ makeOps zipAsyncly

    describe "Functor operations" $ do
        zipAsyncOps  $ functorOps S.fromFoldable "zipAsyncly" (==)
        zipAsyncOps  $ functorOps folded "zipAsyncly folded" (==)

    describe "Monoid operations" $ do
        zipAsyncOps  $ monoidOps "zipAsyncly" mempty (==)

    describe "Semigroup operations" $ do
        zipAsyncOps  $ semigroupOps "zipAsyncly" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        zipAsyncOps $
            prop "zipAsyncly applicative" . zipApplicative S.fromFoldable (==)
        zipAsyncOps $
            prop "zipAsyncly applicative folded" . zipApplicative folded (==)

    describe "Stream transform and combine operations" $ do
        zipAsyncOps  $ transformCombineOpsCommon S.fromFoldable "zipAsyncly" (==)
        zipAsyncOps  $ transformCombineOpsCommon folded "zipAsyncly" (==)
        zipAsyncOps  $ transformCombineOpsOrdered S.fromFoldable "zipAsyncly" (==)
        zipAsyncOps  $ transformCombineOpsOrdered folded "zipAsyncly" (==)

    describe "Stream elimination operations" $ do
        zipAsyncOps  $ eliminationOps S.fromFoldable "zipAsyncly"
        zipAsyncOps  $ eliminationOps folded "zipAsyncly folded"
        zipAsyncOps  $ eliminationOpsWord8 S.fromFoldable "zipAsyncly"
        zipAsyncOps  $ eliminationOpsWord8 folded "zipAsyncly folded"

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        zipAsyncOps  $ eliminationOpsOrdered S.fromFoldable "zipAsyncly"
        zipAsyncOps  $ eliminationOpsOrdered folded "zipAsyncly folded"
