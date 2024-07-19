-- |
-- Module      : Streamly.Internal.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- =Overview
--
-- Use 'watchRecursive' with a list of file system paths you want to watch as
-- argument. It returns a stream of 'Event' representing the file system events
-- occurring under the watched paths.
--
-- @
-- {-\# LANGUAGE MagicHash #-}
-- Stream.mapM_ (putStrLn . 'showEvent') $ 'watchRecursive' [Array.fromList "path"]
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'showEvent' above)
-- provided in this module are used to determine the attributes of the event.
--
-- =Design notes
--
-- For reference documentation see:
--
-- * "<https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html Apple FS Events Programming Guide>"
-- * "<https://developer.apple.com/documentation/coreservices/file_system_events?language=objc Apple FS Events Reference>"
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =BUGS
--
-- XXX Check the behavior for Windows/Linux where applicable. Mention these in
-- the common module so that users are aware of the portability issues.
--
-- There may be some idiosyncrasies in event reporting.
--
-- 1. Multiple events may be coalesced into a single event having multiple
-- flags set.  For example, on macOS 10.15.6, "touch x; rm x" or "touch x; mv x
-- y" produces an event with both "Created" and "Deleted/Moved" flags set.
--
-- 2. The same event can occur multiple times. For example, "touch x; sleep 1;
-- rm x" generates a "Created" event followed by an event with both "Created"
-- and "Deleted" flags set. Similarly, a cloned event can also occur multiple
-- times.
--
-- 3. Some events can go missing and only the latest one is reported. See
-- 'isCreated' for one such case.
--
-- XXX write a test case for this, and check on all platforms.
--
-- 4. When @rm -rf@ is used on the watched root directory, only one 'isDeleted'
-- event occurs and that is for the root. However, @rm -rf@ on a directory
-- inside the watch root produces 'isDeleted' events for all files.
--
-- XXX write a test case for this, and check on all platforms.
--
-- 5. When a file is moved, a move event occurs for both the source and the
-- destination files. There seems to be no way to distinguish the source and
-- the destination. Perhaps the lower eventId can be considered as source.
--
-- XXX cloned event is specific to macOS.  Translate it to the same events as
-- on copying.
--
-- 6. When a file is cloned both source and destination generate a cloned
-- event, however the source has lower eventId and destination may have
-- OwnerGroupModeChanged, InodeAttrsChanged, Created flags as well.
--
-- You may have to stat the path of the event to know more information about
-- what might have happened.  It may be a good idea to watch the behavior of
-- events you are handling before you can rely on it. See the individual event
-- APIs for some of the known issues marked with `BUGS`. Also see the "Handling
-- Events" section in the "Apple FS Events Programming Guide".
--
-- * "<https://stackoverflow.com/questions/18415285/osx-fseventstreameventflags-not-working-correctly Stack overflow question on event coalescing>"
--
-- =TODO
--
-- APIs for persistent event-id related functionality, to get events from
-- specific event-id etc are not implemented.

#include <config.h>

-- macOS 10.7+
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS

module Streamly.Internal.FileSystem.Event.Darwin
    (
    -- * Creating a Watch

    -- ** Default configuration
      Config (..)
    , defaultConfig

    -- ** Watch Behavior
    , BatchInfo (..)
    , setEventBatching

    -- ** Events of Interest
    , setRootPathEvents
    , setFileEvents
    , setIgnoreSelf
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
    , setFullHistory
#endif
    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- * Handling Events
    , Event
    , getEventId
    , getAbsPath

    -- ** Root Level Events
    -- | Events that belong to the root path as a whole and not to specific
    -- items contained in it.
    , isMounted
    , isUnmounted
    , isHistoryDone
    , isRootPathEvent

    -- ** Item Level Metadata change
    , isAttrsModified
    , isSecurityModified
    , isXAttrsModified
    , isFinderInfoModified

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMoved
    , isModified
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
    , isCloned
#endif

    -- ** Item Path info
    , isDir
    , isFile
    , isSymLink
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
    , isHardLink
    , isLastHardLink
#endif

    -- ** Exception Conditions
    , isEventIdWrapped
    , isEventsLost
    , isKernelDropped
    , isUserDropped

    -- * Debugging
    , showEvent
    )
where

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Control.Monad (when)
import Data.Bits ((.|.), (.&.), complement)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..), CDouble(..), CSize(..), CUChar(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.IO.Handle.FD (fdToHandle)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Cont (contListMap)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array (Array(..))
import System.IO (Handle, hClose)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array as A

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- XXX Add a recursive option to Config
-- Keep setRecursiveMode False as undefined for now until we implement it.
--
-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Pre-release/
--
data Config = Config
    { latency :: Double
    , createFlags   :: Word32
    }

-------------------------------------------------------------------------------
-- Batching Events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FileSystem/Event/Darwin.h FSEventStreamCreateFlagNoDefer"
    kFSEventStreamCreateFlagNoDefer :: Word32

-- | Determines how multiple events are batched or throttled.
data BatchInfo =
      Throttle Double -- ^ Deliver an event immediately but suppress the
                      -- following events upto the specified time.
    | Batch Double    -- ^ Collapse all events that occurred in the specified
                      -- time window (in seconds) and deliver them as a single
                      -- batch.

-- | Set how the events should be batched. See 'BatchInfo' for details.  A
-- negative value for time is treated as 0.
--
-- /default: Batch 0.0/
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
setEventBatching :: BatchInfo -> Config -> Config
setEventBatching batchInfo cfg =
    let (t, status) =
            case batchInfo of
                Throttle sec | sec < 0 -> (0, True)
                Throttle sec -> (sec, True)
                Batch sec | sec < 0 -> (0, False)
                Batch sec -> (sec, False)
    in setFlag kFSEventStreamCreateFlagNoDefer status $ cfg {latency = t}

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

setFlag :: Word32 -> Bool -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            if status
            then createFlags .|. mask
            else createFlags .&. complement mask
    in cfg {createFlags = flags}

foreign import ccall safe
    "FSEventStreamCreateFlagWatchRoot"
    kFSEventStreamCreateFlagWatchRoot :: Word32

-- XXX write a test case for this, and check on all platforms. The behavior on
-- macOS is slightly different as a create event is also generated.
--
-- | Any changes to the path (delete, rename, or create) of an object are
-- reported as events for its parent directory. Such events are not reported
-- for the watch root itself because its parent is not being watched. When
-- 'setRootPathEvents' is 'True', an 'isRootPathEvent' event is generated with an
-- eventId 0 if the watch root is deleted, renamed or created.
--
-- Note: We have observed that an 'isDeleted' event occurs for the root path
-- (when it is a regular dir and not symlink) even when this option is off.
--
-- /default: False/
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
setRootPathEvents :: Bool -> Config -> Config
setRootPathEvents = setFlag kFSEventStreamCreateFlagWatchRoot

foreign import ccall safe
    "FSEventStreamCreateFlagFileEvents"
    kFSEventStreamCreateFlagFileEvents :: Word32

-- | When this is 'False' only events for the watched directories are reported.
-- For example, when a file is created inside a directory it is reported as an
-- event, the path name in the event is the path of the directory being watched
-- and not of the specific item for which the event occurred.  Events cannot be
-- distinguished based on the type of event.  For example, we cannot determine
-- if an event is a "create" or "delete", we only know that some event ocurred
-- under the watched directory hierarchy.
--
-- When it is 'True' the path reported in the event is the path of the item and
-- not the of the directory in which it is contained. We can use the event
-- accessor functions to determine the event type and the path type etc.
--
-- /default: True/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
setFileEvents :: Bool -> Config -> Config
setFileEvents = setFlag kFSEventStreamCreateFlagFileEvents

foreign import ccall safe
    "FSEventStreamCreateFlagIgnoreSelf"
    kFSEventStreamCreateFlagIgnoreSelf :: Word32

-- | When this is 'True' events generated by the current process are not
-- reported.
--
-- /default: False/
--
-- /macOS 10.6+/
--
-- /Pre-release/
--
setIgnoreSelf :: Bool -> Config -> Config
setIgnoreSelf = setFlag kFSEventStreamCreateFlagIgnoreSelf

#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
foreign import ccall safe
    "FSEventStreamCreateFlagFullHistory"
    kFSEventStreamCreateFlagFullHistory :: Word32

-- | When this is 'True' all events since the beginning of time are reported.
--
-- /default: False/
--
-- /macOS 10.15+/
--
-- /Pre-release/
--
setFullHistory :: Bool -> Config -> Config
setFullHistory = setFlag kFSEventStreamCreateFlagFullHistory
#endif

-- | Set all tunable events 'True' or 'False'. Equivalent to setting:
--
-- * setRootPathEvents
-- * setFileEvents
--
-- /Pre-release/
--
setAllEvents :: Bool -> Config -> Config
setAllEvents s =
      setRootPathEvents s
    . setFileEvents s

-------------------------------------------------------------------------------
-- Default config
-------------------------------------------------------------------------------

-- | The default settings are:
--
-- * 'setEventBatching' ('Batch' 0.0)
-- * 'setFileEvents' 'True'
-- * 'setRootPathEvents' 'False'
-- * 'setIgnoreSelf' 'False'
#if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFULLHISTORY
-- * 'setFullHistory' 'False'
#endif
--
-- /Pre-release/
--
defaultConfig :: Config
defaultConfig = setFileEvents True $ Config
    { latency = 0.0
    , createFlags = 0
    }

-------------------------------------------------------------------------------
-- Open an event stream (C FFI)
-------------------------------------------------------------------------------

-- See FileSystem/Event/Darwin.h

data CWatch

-- Representation of "struct pathName" in "FileSystem/Event/Darwin.h"
data PathName = PathName
    { pathBytes :: Ptr CUChar
    , pathLen :: CSize
    }

instance Storable PathName where
    alignment _ = sizeOf (undefined :: Ptr a)
    sizeOf _ = sizeOf (undefined :: Ptr CUChar) + sizeOf (undefined :: CSize)
    peek ptr = do
        bptr <- peekByteOff ptr 0
        len <- peekByteOff ptr (sizeOf (undefined :: Ptr CUChar))
        return $ PathName bptr len
    poke ptr PathName{..} = do
        pokeByteOff ptr 0 pathBytes
        pokeByteOff ptr (sizeOf (undefined :: Ptr CUChar)) pathLen

foreign import ccall safe "FileSystem/Event/Darwin.h createWatch" createWatch
    :: Ptr PathName
    -> CInt
    -> Word32
    -> Word64
    -> CDouble
    -> Ptr CInt
    -> Ptr (Ptr CWatch)
    -> IO CInt

foreign import ccall safe "FileSystem/Event/Darwin.h destroyWatch" destroyWatch
    :: Ptr CWatch -> IO ()

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A handle for a watch.
data Watch = Watch Handle (Ptr CWatch) (MVar Bool)

-- | Given a 'Config' and @paths@ start monitoring the paths for file system
-- events. Returns a 'Watch' handle which can then be used to read the event
-- stream or to close the watch.
--
-- Implementation Note: This API creates an OS level thread where an event loop
-- runs and writes the events to the write side fd of a POSIX pipe. Event
-- stream APIs read the events from the read end of this pipe.  Since an OS
-- level thread is forked, creating a new watch stream is expensive. It is
-- advised to minimize the number of watches.
--
-- /Pre-release/
--
openWatch :: Config -> NonEmpty (Array Word8) -> IO Watch
openWatch Config{..} paths = do
    -- XXX write a test case when path does not exist
    -- XXX write a test case when there are no permissions
    -- XXX check whether the path exists. If the path does not exist and
    -- is created later it cannot be properly monitored. Also a user may
    -- inadvertently provide a path for which the user may not have permission
    -- and then later wonder why events are not being reported.
    withPathNames (NonEmpty.toList paths) $ \arraysPtr ->
        alloca $ \fdPtr -> do
        alloca $ \watchPPtr -> do
            let nArrays = fromIntegral (NonEmpty.length paths)
                seconds = realToFrac latency
            r <- createWatch
                    arraysPtr nArrays createFlags 0 seconds fdPtr watchPPtr
            when (r /= 0) $
                ioError (userError "openWatch: failed to create watch.")
            fd <- peek fdPtr
            h <- fdToHandle fd
            watchPtr <- peek watchPPtr
            closeLock <- newMVar False
            return $ Watch h watchPtr closeLock

    where

    withPathName :: Array Word8 -> (PathName -> IO a) -> IO a
    withPathName arr act = do
        A.unsafePinnedAsPtr arr $ \ptr byteLen ->
            let pname = PathName (castPtr ptr) (fromIntegral byteLen)
            in act pname

    withPathNames = contListMap withPathName withArray

-- | Close a 'Watch' handle.
--
-- /Pre-release/
--
closeWatch :: Watch -> IO ()
closeWatch (Watch h watchPtr mvar) = do
    hClose h
    closed <- takeMVar mvar
    when (not closed) $ destroyWatch watchPtr
    putMVar mvar True

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
--
-- /Pre-release/
--
data Event = Event
   { eventId :: Word64
   , eventFlags :: Word32
   , eventAbsPath :: Array Word8
   } deriving (Show, Ord, Eq)

-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
--
-- XXX should we use a magic in the header to avoid any issues due to
-- misalignment of records? Can that ever happen?
--
readOneEvent :: Parser Word8 IO Event
readOneEvent = do
    arr <- PR.takeEQ 24 (A.createOf 24)
    let arr1 = A.castUnsafe arr :: Array Word64
        eid = A.getIndexUnsafe 0 arr1
        eflags = A.getIndexUnsafe 1 arr1
        pathLen = fromIntegral $ A.getIndexUnsafe 2 arr1
    path <- PR.takeEQ pathLen (A.createOf pathLen)
    return $ Event
        { eventId = eid
        , eventFlags = fromIntegral eflags
        , eventAbsPath = path
        }

watchToStream :: Watch -> Stream IO Event
watchToStream (Watch handle _ _) =
    S.catRights $ S.parseMany readOneEvent $ S.unfold FH.reader handle

-- XXX Write tests for all the points in macOS specific behavior.
--
-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The paths
-- could be files or directories.  When the path is a directory and recursive
-- mode is set, the whole directory tree under it is watched recursively.
-- Monitoring starts from the current time onwards. The paths are specified as
-- UTF-8 encoded 'Array' of 'Word8'.
--
-- Important notes for macOS specific behavior:
--
-- From the observed behavior it seems macOS watches the paths, whatever they
-- are pointing to at any given point of time:
--
-- /Watch root deletion:/ If the the watch root is deleted and then recreated,
-- the newly created file or directory is automatically watched.
--
-- /Watch root moved:/ If the watch root is moved to a new path, the object
-- will no longer be watched unless the new path is also being watched and was
-- pointing to an existing file at the time of starting the watch (see notes
-- about non-existing paths below).
--
-- /Symbolic link watch root:/ If the path name to be watched is a symbolic
-- link then the target of the link is watched instead of the symbolic link
-- itself. It is equivalent to as if the target of the symbolic link itself was
-- directly added to the watch API. That is, the symbolic link is resolved at
-- the time of adding the watch.
--
-- Note that if a watched path is deleted and recreated as a symbolic link
-- pointing to another path then the symbolic link file itself is watched, it
-- won't be resolved. The symbolic link resolution happens only at the time of
-- adding the watch.
--
-- /Non-existing Paths:/ If a watch is started on a non-existing path then the
-- path is not watched even if it is created later.  The macOS API does not
-- fail for a non-existing path.
--
-- If a non-existing path is watched with 'setRootPathEvents' then an
-- 'isRootPathEvent' event is reported if the path is created later and the
-- "path" field in the event is set to the dirname of the path rather than the
-- full absolute path. This is the observed behavior on macOS 10.15.1.
--
-- @
-- {-\# LANGUAGE MagicHash #-}
-- watchWith
--      ('setIgnoreSelf' 'True' . 'setRootPathEvents' 'True')
--      [Array.fromList "path"]
-- @
--
-- /Pre-release/
--
watchWith ::
    (Config -> Config) -> NonEmpty (Array Word8) -> Stream IO Event
watchWith f paths = S.bracketIO before after watchToStream

    where

    before = openWatch (f defaultConfig) paths
    after = closeWatch

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith id
--
watchRecursive :: NonEmpty (Array Word8) -> Stream IO Event
watchRecursive = watchWith id

-- | Same as 'watchWith' using defaultConfig and non-recursive mode.
--
-- /Unimplemented/
--
watch :: NonEmpty (Array Word8) -> Stream IO Event
watch _paths = undefined

-------------------------------------------------------------------------------
-- Examine the event stream
-------------------------------------------------------------------------------

-- | Get the event id of an 'Event'.  Event-id is a monotonically increasing
-- 64-bit integer identifying an event uniquely.
--
-- /Pre-release/
--
getEventId :: Event -> Word64
getEventId Event{..} = eventId

foreign import ccall safe
    "FSEventStreamEventFlagEventIdsWrapped"
    kFSEventStreamEventFlagEventIdsWrapped :: Word32

-- | Determine whether the event id has wrapped. This is impossible on any
-- traditional hardware in any reasonable amount of time unless the event-id
-- starts from a very high value, because the event-id is a 64-bit integer.
-- However, apple still recommends to check and handle this flag.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isEventIdWrapped :: Event -> Bool
isEventIdWrapped = getFlag kFSEventStreamEventFlagEventIdsWrapped

-- | Get the absolute path of the file system object for which the event is
-- generated. The path is a UTF-8 encoded array of bytes.
--
-- When the watch root is a symlink, the absolute path returned is via the real
-- path of the root after resolving the symlink.
--
-- /Pre-release/
--
getAbsPath :: Event -> Array Word8
getAbsPath Event{..} = eventAbsPath

-------------------------------------------------------------------------------
-- Event types
-------------------------------------------------------------------------------

getFlag :: Word32 -> Event -> Bool
getFlag mask Event{..} = eventFlags .&. mask /= 0

-------------------------------------------------------------------------------
-- Error events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagMustScanSubDirs"
    kFSEventStreamEventFlagMustScanSubDirs :: Word32

-- | Apple documentation says that "If an event in a directory occurs at about
-- the same time as one or more events in a subdirectory of that directory, the
-- events may be coalesced into a single event." In that case you will recieve
-- 'isEventsLost' event. In that case the path listed in the event is
-- invalidated and you must rescan it to know the current state.
--
-- This event can also occur if a communication error (overflow) occurs in
-- sending the event and the event gets dropped. In that case 'isKernelDropped'
-- and/or 'isUserDropped' attributes will also be set.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isEventsLost :: Event -> Bool
isEventsLost = getFlag kFSEventStreamEventFlagMustScanSubDirs

foreign import ccall safe
    "FSEventStreamEventFlagKernelDropped"
    kFSEventStreamEventFlagKernelDropped :: Word32

-- | Did an event get dropped due to a kernel processing issue? Set only when
-- 'isEventsLost' is also true.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isKernelDropped :: Event -> Bool
isKernelDropped = getFlag kFSEventStreamEventFlagKernelDropped

foreign import ccall safe
    "FSEventStreamEventFlagUserDropped"
    kFSEventStreamEventFlagUserDropped :: Word32

-- | Did an event get dropped due to a user process issue? Set only when
-- 'isEventsLost' is also true.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isUserDropped :: Event -> Bool
isUserDropped = getFlag kFSEventStreamEventFlagUserDropped

-------------------------------------------------------------------------------
-- Global Sentinel Events
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagHistoryDone"
    kFSEventStreamEventFlagHistoryDone :: Word32

-- | Determine whether the event is a history done marker event. A history done
-- event is generated when the historical events from before the current time
-- are done. Historical events are generated when the "since" parameter in the
-- watch config is set to before the current time.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isHistoryDone :: Event -> Bool
isHistoryDone = getFlag kFSEventStreamEventFlagHistoryDone

-------------------------------------------------------------------------------
-- Events affecting the watched path only
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagRootChanged"
    kFSEventStreamEventFlagRootChanged :: Word32

-- | Determine whether the event indicates a change of path of the monitored
-- object itself. Note that the object may become unreachable or deleted after
-- a change of path.
--
-- /Applicable only when 'setRootPathEvents' is 'True'/
--
-- /Occurs only for the watched path/
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isRootPathEvent :: Event -> Bool
isRootPathEvent = getFlag kFSEventStreamEventFlagRootChanged

-------------------------------------------------------------------------------
-- Global events under the watched path
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagMount"
    kFSEventStreamEventFlagMount :: Word32

-- | Determine whether the event is a mount event. A mount event is generated
-- if a volume is mounted under the path being watched.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isMounted :: Event -> Bool
isMounted = getFlag kFSEventStreamEventFlagMount

foreign import ccall safe
    "FSEventStreamEventFlagUnmount"
    kFSEventStreamEventFlagUnmount :: Word32

-- | Determine whether the event is an unmount event. An unmount event is
-- generated if a volume mounted under the path being watched is unmounted.
--
-- /macOS 10.5+/
--
-- /Pre-release/
--
isUnmounted :: Event -> Bool
isUnmounted = getFlag kFSEventStreamEventFlagUnmount

-------------------------------------------------------------------------------
-- Metadata change Events (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemChangeOwner"
    kFSEventStreamEventFlagItemChangeOwner :: Word32

-- | Determine whether the event is ownership, group, permissions or ACL change
-- event of an object contained within the monitored path.
--
-- Note that this event may be generated even if the metadata is changed to the
-- same value again.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isSecurityModified :: Event -> Bool
isSecurityModified = getFlag kFSEventStreamEventFlagItemChangeOwner

foreign import ccall safe
    "FSEventStreamEventFlagItemInodeMetaMod"
    kFSEventStreamEventFlagItemInodeMetaMod :: Word32

-- | Determine whether the event indicates inode metadata change for an object
-- contained within the monitored path. This event is generated when inode
-- attributes other than the owner, group or permissions are changed e.g. file
-- modification time. It does not occur when the link count of a file changes
-- (as of macOS 10.15.1).
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isAttrsModified :: Event -> Bool
isAttrsModified = getFlag kFSEventStreamEventFlagItemInodeMetaMod

foreign import ccall safe
    "FSEventStreamEventFlagItemFinderInfoMod"
    kFSEventStreamEventFlagItemFinderInfoMod :: Word32

-- | Determine whether the event indicates finder information metadata change
-- for an object contained within the monitored path.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isFinderInfoModified :: Event -> Bool
isFinderInfoModified = getFlag kFSEventStreamEventFlagItemFinderInfoMod

foreign import ccall safe
    "FSEventStreamEventFlagItemXattrMod"
    kFSEventStreamEventFlagItemXattrMod :: Word32

-- | Determine whether the event indicates extended attributes metadata change
-- for an object contained within the monitored path.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isXAttrsModified :: Event -> Bool
isXAttrsModified = getFlag kFSEventStreamEventFlagItemXattrMod

-------------------------------------------------------------------------------
-- CRUD Events (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemCreated"
    kFSEventStreamEventFlagItemCreated :: Word32

-- | Determine whether the event indicates creation of an object within the
-- monitored path. This event is generated when any file system object other
-- than a hard link is created.  On hard linking only an 'isAttrsModified'
-- event on the directory is generated, it is not a create event. However, when
-- a hard link is deleted 'isDeleted' and 'isHardLink' both are true.
--
-- This event can occur for the watched path if the path was deleted/moved and
-- created again (tested on macOS 10.15.1). However, if a watch is started on a
-- non-existing path and the path is created later, then this event is not
-- generated, the path is not watched.
--
-- BUGS: On 10.15.1 when we use a "touch x" to create a file for the first time
-- only an 'isAttrsModified' event occurs and there is no 'isCreated'
-- event. However, this seems to have been fixed on 10.15.6.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = getFlag kFSEventStreamEventFlagItemCreated

foreign import ccall safe
    "FSEventStreamEventFlagItemRemoved"
    kFSEventStreamEventFlagItemRemoved :: Word32

-- | Determine whether the event indicates deletion of an object within the
-- monitored path. This is true when a file or a hardlink is deleted.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isDeleted :: Event -> Bool
isDeleted = getFlag kFSEventStreamEventFlagItemRemoved

foreign import ccall safe
    "FSEventStreamEventFlagItemRenamed"
    kFSEventStreamEventFlagItemRenamed :: Word32

-- | Determine whether the event indicates rename of an object within the
-- monitored path. This event is generated when a file is renamed within the
-- watched directory or if it is moved out of or in the watched directory.
--
-- /Applicable only when 'setFileEvents' is 'True'/
--
-- /Can occur for watched path or a file inside it/
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved = getFlag kFSEventStreamEventFlagItemRenamed

foreign import ccall safe
    "FSEventStreamEventFlagItemModified"
    kFSEventStreamEventFlagItemModified :: Word32

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = getFlag kFSEventStreamEventFlagItemModified

#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
foreign import ccall safe
    "FSEventStreamEventFlagItemCloned"
    kFSEventStreamEventFlagItemCloned :: Word32

-- | Determine whether the event indicates cloning of an object within the
-- monitored path. The "Duplicate" command in the "File" menu of the "Finder"
-- application generates a "clone" event.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.13+/
--
-- /Pre-release/
--
isCloned :: Event -> Bool
isCloned = getFlag kFSEventStreamEventFlagItemCloned
#endif

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import ccall safe
    "FSEventStreamEventFlagItemIsDir"
    kFSEventStreamEventFlagItemIsDir :: Word32

-- | Determine whether the event is for a directory path.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isDir :: Event -> Bool
isDir = getFlag kFSEventStreamEventFlagItemIsDir

foreign import ccall safe
    "FSEventStreamEventFlagItemIsFile"
    kFSEventStreamEventFlagItemIsFile :: Word32

-- | Determine whether the event is for a file path.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isFile :: Event -> Bool
isFile = getFlag kFSEventStreamEventFlagItemIsFile

foreign import ccall safe
    "FSEventStreamEventFlagItemIsSymlink"
    kFSEventStreamEventFlagItemIsSymlink :: Word32

-- | Determine whether the event is for a symbolic link path.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.7+/
--
-- /Pre-release/
--
isSymLink :: Event -> Bool
isSymLink = getFlag kFSEventStreamEventFlagItemIsSymlink

#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
foreign import ccall safe
    "FSEventStreamEventFlagItemIsHardlink"
    kFSEventStreamEventFlagItemIsHardlink :: Word32

-- | Determine whether the event is for a file with more than one hard link.
-- When 'isFile' is true we can check for 'isHardLink'. Note that 'isHardLink'
-- is not true when a hard link is created, however, it is true when a file
-- which has or had more than one link in the past is removed. This is not true
-- if a file never had more than one link.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.10+/
--
-- /Pre-release/
--
isHardLink :: Event -> Bool
isHardLink = getFlag kFSEventStreamEventFlagItemIsHardlink

foreign import ccall safe
    "FSEventStreamEventFlagItemIsLastHardlink"
     kFSEventStreamEventFlagItemIsLastHardlink :: Word32

-- | Determine whether the event is for a hard link path with only one hard
-- link.  If 'isHardLink' is true then we can check for 'isLastHardLink'.  This
-- is true when a file has had more than one link and now the last link is
-- removed. In that case both 'isHardLink' and 'isLastHardLink" would be true.
--
-- Applicable only when 'setFileEvents' is 'True'.
--
-- /macOS 10.10+/
--
-- /Pre-release/
--
isLastHardLink :: Event -> Bool
isLastHardLink = getFlag kFSEventStreamEventFlagItemIsLastHardlink
#endif

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
    let path = runIdentity $ S.fold FL.toList $ U.decodeUtf8' $ A.toStream eventAbsPath
    in "--------------------------"
        ++ "\nId = " ++ show eventId
        ++ "\nPath = " ++ show path
        ++ "\nFlags " ++ show eventFlags

        ++ showev isEventsLost "MustScanSubdirs"
        ++ showev isKernelDropped "KernelDropped"
        ++ showev isUserDropped "UserDropped"

        ++ showev isRootPathEvent "RootPathEvent"
        ++ showev isMounted "Mounted"
        ++ showev isUnmounted "Unmounted"
        ++ showev isHistoryDone "HistoryDone"

        ++ showev isSecurityModified "SecurityModified"
        ++ showev isAttrsModified "AttrsModified"
        ++ showev isFinderInfoModified "FinderInfoChanged"
        ++ showev isXAttrsModified "XAttrsModified"

        ++ showev isCreated "Created"
        ++ showev isDeleted "Deleted"
        ++ showev isModified "Modified"
        ++ showev isMoved "Moved"
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMCLONED
        ++ showev isCloned "Cloned"
#endif

        ++ showev isDir "Dir"
        ++ showev isFile "File"
        ++ showev isSymLink "SymLink"
#if HAVE_DECL_KFSEVENTSTREAMEVENTFLAGITEMISHARDLINK
        ++ showev isHardLink "HardLink"
        ++ showev isLastHardLink "LastHardLink"
#endif
        ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""
#else
module Streamly.Internal.FileSystem.Event.Darwin () where
#warning "Autoconf did not find the definition \
kFSEventStreamCreateFlagFileEvents in Darwin header files.\
Do you have Cocoa framework header files installed?\
Not compiling the Streamly.Internal.FileSystem.Event.Darwin module. \
Programs depending on this module may not compile. \
Check if HAVE_DECL_KFSEVENTSTREAMCREATEFLAGFILEEVENTS is defined in config.h \
generated from src/config.h.in"
#endif
