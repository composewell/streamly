-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Test.FileSystem.Event (main) where

import qualified Streamly.Internal.FileSystem.Event as Event
import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FileSystem.Event"

main :: IO ()
main = do
    let run = runTests moduleName "non-recursive" Event.watch
    run DirType commonTests
    run
#if defined(CABAL_OS_DARWIN)
        SymLinkResolvedPath
#else
        SymLinkOrigPath
#endif
        commonTests
