-- |
-- Module      : Streamly.Test.Data.MutArray.Generic
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.MutArray.Generic (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isLower)
import Test.Hspec as H
import Test.Hspec.QuickCheck

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray.Generic as MArray
import qualified Streamly.Internal.Data.Stream as Stream

moduleName :: String
moduleName = "Data.MutArray.Generic"

#include "Streamly/Test/Data/MutArray/Common.hs"

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            commonMain
            -- IMPORTANT NOTE: Before adding any test here first consider if it
            -- can be added to the MutArray/Common test module. Only those tests
            -- which are specific to the Generic MutArray module and do not
            -- apply to the Unboxed MutArray module should be added here.
