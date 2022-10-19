{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Data.SmallArray
-- Copyright   : (c) 2020 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.SmallArray (main) where

#include "Streamly/Test/Data/Array/CommonImports.hs"

import qualified Streamly.Internal.Data.SmallArray as A
type Array = A.SmallArray

moduleName :: String
moduleName = "Data.SmallArray"

#include "Streamly/Test/Data/Array/Common.hs"

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        commonMain
