-- |
-- Module      : Streamly.Test.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Windows (main) where

import qualified Streamly.Internal.FileSystem.Event.Windows as Event
import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FileSystem.Event.Windows"

-- TODO Test isModified event for parent directories when a file is created or
-- deleted.

main :: IO ()
main = do
    let regularRootTests =
            commonTests
        -- The following watch-root deletion test results in:
        -- C:\tmp\fsevent_dir-e061f0b0a00e1696\watch-root:
        -- removePathForcibly:RemoveDirectory
        -- "\\\\?\\C:\\tmp\\fsevent_dir-e061f0b0a00e1696\\watch-root":
        -- permission denied (The process cannot access the file because it is
        -- being used by another process.)
        --
        -- ++ dirDelete "" (\dir -> [(dir, Event.isDeleted)])

    let w = Event.watchWith (Event.setAllEvents True)
        run = runTests moduleName "non-recursive" w

    run DirType regularRootTests
    run SymLinkOrigPath commonTests

    let recTests =
            [ dirCreateWithParent "subdir\\dir"
                (\dir -> [(dir, dirEvent Event.isCreated)])
            , fileCreateWithParent "subdir\\file"
                (\file -> [(file, fileEvent Event.isCreated)])
            ] ++ commonRecTests

    let recw = Event.watchWith
                (Event.setAllEvents True . Event.setRecursiveMode True)
        runRec = runTests moduleName "recursive" recw

    runRec DirType (regularRootTests ++ recTests)
    runRec SymLinkOrigPath (commonTests ++ recTests)
