-- |
-- Module      : Streamly.Test.FileSystem.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Linux (main) where

import Streamly.Internal.FileSystem.Event.Linux (Event)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FileSystem.Event.Linux"

dirTouchEvents :: String -> [([Char], Event -> Bool)]
dirTouchEvents dir =
    [ (dir, dirEvent Event.isOpened)
    , (dir, dirEvent Event.isAccessed)
    , (dir, dirEvent Event.isNonWriteClosed)
    ]

dirDelEvents :: String -> [([Char], Event -> Bool)]
dirDelEvents dir =
      (dir, dirEvent Event.isDeleted)
    : (dir, dirEvent Event.isAttrsModified)
    : dirTouchEvents dir

rootDirDelEvents :: String -> [([Char], Event -> Bool)]
rootDirDelEvents root =
      (root, Event.isRootUnwatched)
    : (root, Event.isRootDeleted)
    : (root, dirEvent Event.isAttrsModified)
    : dirTouchEvents root

dirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
dirMoveEvents src dst =
    [ (src, dirEvent Event.isMoved)
    , (src, dirEvent Event.isMovedFrom)
    , (dst, dirEvent Event.isMoved)
    , (dst, dirEvent Event.isMovedTo)
    ]

#ifdef DEVBUILD
-- In recursive mode all subdirectories are roots therefore they will generate
-- isRootMoved.
rootDirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
rootDirMoveEvents root _ =
      (root, Event.isRootMoved)
    : dirTouchEvents root

recDirMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
recDirMoveEvents src dst = dirMoveEvents src dst ++ rootDirMoveEvents src dst
#endif

fileTouchEvents :: String -> [([Char], Event -> Bool)]
fileTouchEvents file =
    [ (file, fileEvent Event.isOpened)
    , (file, fileEvent Event.isModified)
    , (file, fileEvent Event.isWriteClosed)
    ]

fileMoveEvents :: [Char] -> [Char] -> [([Char], Event -> Bool)]
fileMoveEvents src dst =
    [ (src, fileEvent Event.isMoved)
    , (src, fileEvent Event.isMovedFrom)
    , (dst, fileEvent Event.isMoved)
    , (dst, fileEvent Event.isMovedTo)
    ]

-- TODO: add fileRoot tests from macOS test suite

main :: IO ()
main = do
    -- We ignore the events on root/parent dir during regular non-root dir/file
    -- tests.

    -- Tests common to regular root and symlink root cases
    let regSymTests =
              fileCreate "file" fileTouchEvents
            : fileMove "file1" "file2" fileMoveEvents
            : dirMove "dir1" "dir2" dirMoveEvents
            : dirDelete "dir" dirDelEvents
            : commonTests

    let regTests =
              dirDelete "" rootDirDelEvents
            : rootDirMove "moved" (\src -> [(src, Event.isRootMoved)])
            : regSymTests

    let symTests =
             -- when root is a symlinked dir, it does not recv touch, isDeleted
             -- or rootDeleted, rootUnwatched events.
              dirDelete "" (\dir -> [(dir, dirEvent Event.isAttrsModified)])
            -- No events occur when a symlink root is moved
            : regSymTests

    let w = Event.watchWith (Event.setAllEvents True)
        run = runTests moduleName "non-recursive" w

    run DirType regTests
    run SymLinkOrigPath symTests

    let fileRootTests =
            [ fileDelete "" (\path ->
                [ (path, Event.isAttrsModified)
                , (path, Event.isRootDeleted)
                , (path, Event.isRootUnwatched)
                ])
            , rootFileMove "moved" (\path -> [(path, Event.isRootMoved)])
            , fileModify "" (\path -> [(path, Event.isOpened)])
            ]

    run FileType fileRootTests

    let recw = Event.watchWith
                (Event.setAllEvents True . Event.setRecursiveMode True)
        runRec = runTests moduleName "recursive" recw

#ifdef DEVBUILD
    -- In recursive mode all subdirectories are roots therefore they will
    -- generate isRootDeleted/isRootUnwatched. Also, for subdirectories
    -- multiple events are generated, once in the parent watch and once in the
    -- self watch as a root of the watch. Therefore, additional touchEvents are
    -- generated in this case.
    --
    -- XXX We can possibly filter out the duplicate events either from the
    -- parent or self.
    let regSymRecTests =
            -- XXX Nested file create misses the create event due to a race
            -- : fileCreateWithParent "subdir/file" fileTouchEvents
              fileCreate "subdir/file" fileTouchEvents
            : fileMove "subdir/file1" "subdir/file2" fileMoveEvents
            : dirMove "dir1" "dir2" recDirMoveEvents
            : dirMove "subdir/dir1" "subdir/dir2" recDirMoveEvents
            : dirDelete "dir" (\d -> rootDirDelEvents d ++ dirDelEvents d)
            : dirDelete "subdir/dir" (\d -> rootDirDelEvents d ++ dirDelEvents d)
            -- XXX Nested dir create misses the create event due to a race
            -- : dirCreateWithParent "subdir/dir" dirTouchEvents
            : dirCreate "subdir/dir"
                (\dir -> (dir, dirEvent Event.isCreated) : dirTouchEvents dir)
            : dirCreate "dir"
                (\dir -> (dir, dirEvent Event.isCreated) : dirTouchEvents dir)
            : commonRecTests
        recRegTests = regTests ++ regSymRecTests
        recSymTests = symTests ++ regSymRecTests

    -- XXX these tests fails intermittently for recursive case
    -- FileSystem.Event.Linux.recursive, Root type SymLinkOrigPath, File deleted (subdir/file1)
    -- FileSystem.Event.Linux.recursive, Root type SymLinkOrigPath, File modified (subdir/file1)
    -- FileSystem.Event.Linux.recursive, Root type SymLinkOrigPath, File moved (subdir/file1 subdir/file2)
    -- FileSystem.Event.Linux.recursive, Root type DirType, File moved (file1 file2)
    -- FileSystem.Event.Linux.recursive, Root type DirType, File created (file)
    --      uncaught exception: IOException of type ResourceBusy
    --      /tmp/fsevent_dir-a5bd0df64c44ab27/watch-root/file: openFile: resource busy (file is locked)

    runRec DirType recRegTests
    runRec SymLinkOrigPath recSymTests
#endif
    runRec FileType fileRootTests
