-- |
-- Module      : Streamly.Test.Data.Stream.Prelude
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Prelude (main) where

import qualified Streamly.Test.Data.Stream.Prelude.Concurrent as Concurrent
import qualified Streamly.Test.Data.Stream.Prelude.Exception as Exception
import qualified Streamly.Test.Data.Stream.Prelude.Time as Time

main :: IO ()
main = do
    Concurrent.main
    Exception.main
    Time.main
