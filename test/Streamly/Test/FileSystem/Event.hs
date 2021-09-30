-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Main (main) where

import System.IO (BufferMode(..), hSetBuffering, stdout)
#if defined(CABAL_OS_LINUX)
import qualified Streamly.Test.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Test.FileSystem.Event.Windows as Event
#endif

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    Event.testCommonEvents
    