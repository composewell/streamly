-- |
-- Module      : Streamly.Test.Prelude.ZipSerial
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.ZipSerial where

#if __GLASGOW_HASKELL__ < 808
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
    let zipSerialOps :: IsStream t
            => ((ZipSerialM IO a -> t IO a) -> Spec) -> Spec
        zipSerialOps spec = mapOps spec $ makeOps zipSerially
#ifndef COVERAGE_BUILD
            <> [("rate AvgRate 0.00000001", zipSerially . avgRate 0.00000001)]
            <> [("maxBuffer (-1)", zipSerially . maxBuffer (-1))]
#endif

    describe "Functor operations" $ do
        zipSerialOps $ functorOps S.fromFoldable "zipSerially" (==)
        zipSerialOps $ functorOps folded "zipSerially folded" (==)

    describe "Monoid operations" $ do
        zipSerialOps $ monoidOps "zipSerially" mempty (==)

    describe "Semigroup operations" $ do
        zipSerialOps $ semigroupOps "zipSerially" (==)

    -- XXX add tests for indexed/indexedR
    describe "Zip operations" $ do
        zipSerialOps $
            prop "zipSerially applicative" . zipApplicative S.fromFoldable (==)
        zipSerialOps $
            prop "zipSerially applicative folded" . zipApplicative folded (==)

    describe "Stream transform and combine operations" $ do
        zipSerialOps $ transformCombineOpsCommon S.fromFoldable "zipSerially" (==)
        zipSerialOps $ transformCombineOpsCommon folded "zipSerially" (==)
        zipSerialOps $ transformCombineOpsOrdered S.fromFoldable "zipSerially" (==)
        zipSerialOps $ transformCombineOpsOrdered folded "zipSerially" (==)

    describe "Stream elimination operations" $ do
        zipSerialOps $ eliminationOps S.fromFoldable "zipSerially"
        zipSerialOps $ eliminationOps folded "zipSerially folded"
        zipSerialOps $ eliminationOpsWord8 S.fromFoldable "zipSerially"
        zipSerialOps $ eliminationOpsWord8 folded "zipSerially folded"

    -- XXX Add a test where we chain all transformation APIs and make sure that
    -- the state is being passed through all of them.
    describe "Stream serial elimination operations" $ do
        zipSerialOps $ eliminationOpsOrdered S.fromFoldable "zipSerially"
        zipSerialOps $ eliminationOpsOrdered folded "zipSerially folded"
