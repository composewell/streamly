{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Scanl.Combinators
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Combinators (main) where

import Data.Int (Int64)
import Data.Semigroup (Sum(..))
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Scanl as F
import qualified Streamly.Internal.Data.Stream as Stream

import qualified Prelude
import Prelude hiding (maximum, minimum, product, sum, mconcat, foldMap, maybe)

import Streamly.Test.Common (withNumTests)
import Streamly.Test.Data.Scanl.Type (check, checkApprox, checkPostscanl)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, forAll, listOf1)

#include "Streamly/Test/Data/Scanl/CommonCombinators.hs"

moduleName :: String
moduleName = "Data.Scanl.Combinators"

main :: IO ()
main = hspec $
    describe moduleName $
        describe "common" commonCombinatorsSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
