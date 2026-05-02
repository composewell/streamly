-- |
-- Module      : Streamly.Test.Data.Stream
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream (main) where

import qualified Streamly.Test.Data.Stream.Parse as Parse
import qualified Streamly.Test.Data.Stream.Type as Type

main :: IO ()
main = do
    Type.main
    Parse.main
