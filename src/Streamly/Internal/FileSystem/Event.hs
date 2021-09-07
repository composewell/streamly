-- |
-- Module      : Streamly.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- File system event notification API portable across Linux, macOS and Windows
-- platforms.
--
-- For platform specific API please see the following modules:
--
-- * "Streamly.Internal.FileSystem.Event.Darwin"
-- * "Streamly.Internal.FileSystem.Event.Linux"
-- * "Streamly.Internal.FileSystem.Event.Windows"

module Streamly.Internal.FileSystem.Event
    (
    -- * Creating a Watch

      watch
    , watchRecursive

    -- * Handling Events
    , Event
    , getAbsPath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified

    -- ** Exception Conditions
    , isEventsLost

    -- ** Debugging
    , showEvent
    )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign (Array)
import Streamly.Prelude (SerialT)

#if defined(CABAL_OS_DARWIN)
import Streamly.Internal.FileSystem.Event.Darwin (Event)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import Streamly.Internal.FileSystem.Event.Linux (Event)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import Streamly.Internal.FileSystem.Event.Windows (Event)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform"
#endif

-- XXX Ensure that defaultConfig is the same for all platforms.
--
-- XXX Ensure that equivalent events are generated on all platforms when same
-- operations are done. Document any differences in behavior on different
-- platforms. Any events that are not common to all platforms should be
-- filtered out.
--
-- XXX Only need to set common event types for macOS/Linux/Windows
-- XXX write a test, that generates all types of events that can possibly
-- occur on each platform. Then create a common test that only checks
-- for the common subset of those.

-------------------------------------------------------------------------------
-- Creating a watch
-------------------------------------------------------------------------------

-- macOS and Windows seem to do a path level watch i.e. they watch the path
-- rather than the underlying inode, possibly they can map the inode back to a
-- path?.  On Linux we cannot keep watching a watch root if the inode it is
-- pointing to has moved. However, the inode continues to be watched in this
-- case instead of the path, which is not the case for macOS.
--
-- To make the behavior same for all platforms, for the Linux case we will have
-- to stop watching the inode if it moves from under the path
-- (setUnwatchMoved).  And on macOS we can stop watching  a path if the inode
-- under it changes i.e. just filter out events for that path.
--
-- Also, to make the behavior of Linux same as macOS we need to set
-- setFollowSymLinks on Linux.

-- XXX Verify all the cases mentioned below using test cases.

-- | Start monitoring a list of file system paths for file system events.  The
-- paths could be files or directories.  Monitoring starts from the current
-- time onwards. The paths are specified as UTF-8 encoded 'Array' of 'Word8'.
--
-- If a watch root is a symbolic link then the target of the link is watched.
-- Fails if the watched path does not exist. If the user does not have
-- permissions (specific permissions?) on the watch root then no events are
-- generated.  No events are generated if the watch root itself is renamed or
-- deleted.
--
-- Note: not yet implemented on macOS, use watchRecursive instead.
--
-- /Pre-release/
--
watch :: NonEmpty (Array Word8) -> SerialT IO Event
#if defined(CABAL_OS_WINDOWS)
watch = Event.watch
#elif defined(CABAL_OS_LINUX)
watch = Event.watchWithFlags
    [ 0x00000002    -- File was modified.
    , 0x00000004    -- Metadata changed.
    , 0x00000100    -- Subfile was created.
    , 0x00000200    -- Subfile was deleted.
    , 0x00000400    -- Root was deleted.
    , 0x00000040    -- File was moved from X.
    , 0x00000080    -- File was moved to Y.
    ]
#elif defined(CABAL_OS_DARWIN)
watch = Event.watch
#endif

-- | Like 'watch' except that if a watched path is a directory the whole
-- directory tree under it is watched recursively.
--
-- On Linux 'watchRecursive' may be more expensive than 'watch'.
--
-- /Pre-release/
--
watchRecursive :: NonEmpty (Array Word8) -> SerialT IO Event
#if defined(CABAL_OS_WINDOWS)
watchRecursive = Event.watchRecursive
#elif defined(CABAL_OS_LINUX)
watchRecursive = Event.watchRecursiveWithFlags
    [ 0x00000002    -- File was modified.
    , 0x00000004    -- Metadata changed.
    , 0x00000100    -- Subfile was created.
    , 0x00000200    -- Subfile was deleted.
    , 0x00000400    -- Root was deleted.
    , 0x00000040    -- File was moved from X.
    , 0x00000080    -- File was moved to Y.
    ]
#elif defined(CABAL_OS_DARWIN)
watchRecursive = Event.watchRecursive
#endif

-------------------------------------------------------------------------------
-- Handling Events
-------------------------------------------------------------------------------

-- | Get the absolute path of the file system object for which the event is
-- generated. The path is a UTF-8 encoded array of bytes.
--
-- /Pre-release/
--
getAbsPath :: Event -> Array Word8
getAbsPath = Event.getAbsPath

-- XXX Test and document the hard linking behavior on each platform.
--
-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object other
-- than a hard link is created.
-- Hard link behaviours:
-- On Linux and Windows hard lnik creation generates 'Created' event.
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = Event.isCreated

-- XXX Test and document the hard link deletion on each platform.
--
-- | Determine whether the event indicates deletion of an object within the
-- monitored path. This is true when a file or a hardlink is deleted.--
-- Hard link behaviours:
-- On Linux and Windows hard lnik deletion generates 'Deleted' event
-- @
-- Deletion of Root path:
--
-- In 'Linux' the deletion of root path only generates 'RootDeleted' event for
-- the root path, whereas the nested directory inside the root path gets
-- 'Deleted' and 'RootDeleted' events.
--
-- In Windows from GUI the deletion of root path is not allowed , from CLI the
-- content inside the root path is deleted and 'Deleted' event is generated
-- for each nested paths.There is no event generated for root path itself.
-- @
-- /Pre-release/
--
isDeleted :: Event -> Bool
isDeleted = Event.isDeleted

-- XXX Test and document the hard link move on each platform.
--
-- | Determine whether the event indicates rename of an object within the
-- monitored path. This event is generated when an object is renamed within the
-- watched directory or if it is moved out of or in the watched directory.
--
-- Hard link behaviours:
-- On Linux and Windows hard lnik move generates 'MovedFrom' and ' MovedTo' events
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved = Event.isMoved

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
-- Unlike Linux in Windows in recursive mode when a file is created or deleted the Modified
-- event is generated for the parent directory as well.
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = Event.isModified

-- | An event that indicates that some events before this may have been lost,
-- therefore, we need to take some recovery action.
--
-- /Pre-release/
--
isEventsLost :: Event -> Bool
isEventsLost = Event.isEventsLost

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation. Note that the output
-- of this function may be different on different platforms because it may
-- contain platform specific details.
--
-- /Internal/
--
showEvent :: Event -> String
showEvent = Event.showEvent
