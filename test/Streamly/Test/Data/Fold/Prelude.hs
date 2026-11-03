-- |
-- Module      : Streamly.Test.Data.Fold.Prelude
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Prelude (main) where

import qualified Streamly.Test.Data.Fold.Concurrent as Concurrent
import qualified Streamly.Test.Data.Fold.Time as Time

main :: IO ()
main = do
    Concurrent.main
    Time.main
