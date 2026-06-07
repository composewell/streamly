-- |
-- Module      : Streamly.Test.Data.Stream.MkType
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.MkType (main) where

import qualified Streamly.Test.Data.Stream.MkType.Ahead as Ahead
import qualified Streamly.Test.Data.Stream.MkType.ZipAsync as ZipAsync
import qualified Streamly.Test.Data.Stream.MkType.ZipSerial as ZipSerial

main :: IO ()
main = do
    Ahead.main
    ZipAsync.main
    ZipSerial.main
