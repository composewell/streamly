-- |
-- Module      : Streamly.Test.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Darwin (main) where

import qualified Streamly.Internal.FileSystem.Event.Darwin as Event

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FileSystem.Event.Darwin"

-- TODO:
-- Creation/deletion/move of hard links
-- Deletion of last hard link
-- If a directory is a symlink is it followed in recursive mode?

main :: IO ()
main = do
    let symLinkRootTests =
              dirCreateWithParent "subdir/dir"
                (\dir -> [(dir, dirEvent Event.isCreated)])
            : fileCreateWithParent "subdir/file"
                (\file ->
                    [ (file, fileEvent Event.isCreated)
                    , (file, fileEvent Event.isAttrsModified)
                    ]
                )
            : fileCreate "file-create-attrs"
                (\file ->
                    [ (file, fileEvent Event.isCreated)
                    , (file, fileEvent Event.isAttrsModified)
                    ]
                )
            : fileModify
                    "file-mod-attrs"
                    (\file -> [(file, fileEvent Event.isAttrsModified)])
            : dirDelete "dir-sec" (\dir ->
                [ (dir, dirEvent Event.isDeleted)
                , (dir, dirEvent Event.isSecurityModified)
                ]
                )
            : commonTests
            ++ commonRecTests
    let regularRootTests =
              dirDelete "" (\dir ->
                [ (dir, dirEvent Event.isDeleted)
                , (dir, dirEvent Event.isSecurityModified)
                , (dir, Event.isRootPathEvent)
                ]
                )
            -- The watch root create event always seems to come even though the
            -- root is created before the watch is started. That may be because
            -- of batching?
            -- XXX Need to create watch root after adding the watch, otherwise
            -- it fails intermittently.
            -- : dirCreate "" (\dir -> [(dir, dirEvent Event.isCreated)])
            : rootDirMove "moved" (\src ->
                [ (src, Event.isRootPathEvent)
                , (src, Event.isMoved)
                ])
            : symLinkRootTests

    let fileRootTests =
            [ fileDelete "" (\path ->
                [ (path, Event.isRootPathEvent)
                , (path, Event.isDeleted)
                ])
            , rootFileMove "moved" (\path ->
                [(path, Event.isRootPathEvent)
                , (path, Event.isMoved)
                ])
            , fileModify "" (const [])
            ]

    let w = Event.watchWith (Event.setAllEvents True)
        run = runTests moduleName "recursive" w

    run DirType regularRootTests
    run SymLinkResolvedPath symLinkRootTests
    run FileType fileRootTests
