{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Scanl.Container
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Container (main) where

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Scanl as F

import Streamly.Test.Data.Scanl.Type (check, checkPostscanl)
import Test.Hspec

#include "Streamly/Test/Data/Scanl/CommonContainer.hs"

moduleName :: String
moduleName = "Data.Scanl.Container"

main :: IO ()
main = hspec $
    describe moduleName $
        describe "common" commonContainerSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
