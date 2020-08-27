-- |
-- Module      : Streamly.Internal.FileSystem.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- =Overview
--
-- Use 'watchPaths' with a list of file system paths you want to watch as
-- argument. It returns a stream of 'Event' representing the file system events
-- occurring under the watched paths.
--
-- @
-- Stream.mapM_ (putStrLn . 'showEvent') $ 'watchPaths' [Array.fromCString\# "dir"#]
-- @
--
-- 'Event' is an opaque type. Accessor functions (e.g. 'showEvent' above)
-- provided in this module are used to determine the attributes of the event.
--
-- Identical successive events may be coalesced into a single event.
--
-- =Design notes
--
-- For reference documentation see:
--
-- * <https://man7.org/linux/man-pages/man7/inotify.7.html inotify man page>
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =BUGs
--
-- When testing on Linux Kernel version @5.3.0-53-generic #47-Ubuntu@, the last
-- event for the root path seems to be delayed until one more event occurs.
--
-- = Differences between macOS and Linux APIs:
--
-- 1. macOS watch is based on the path provided to it, if the path is
-- deleted and recreated it will still be watched, if the path moves to another
-- path it won't be watched anymore. Whereas Linux watch is based on a handle
-- to the path, if the path is deleted and recreated it won't be watched, if
-- the path moves to another it can still be watched (though this is
-- configurable).
--
-- 2. macOS watches the directory hierarchy recursively, Linux watches only one
-- level of dir, recursive watch has to be built in user space by watching for
-- create events and adding the new directories to the watch. Not sure how this
-- will scale for too many paths.
--
-- 3. In macOS the path of the subject of the event is absolute, in Linux the
-- path is the name of the object inside the dir being watched.
--
-- 4. On Linux 'watchPaths' fails if a path does not exist, on macOS it does
-- not fail.

#include "config.h"

#if HAVE_DECL_IN_EXCL_UNLINK
module Streamly.Internal.FileSystem.Event.Linux
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config
    , Toggle (..)
    , defaultConfig

    -- ** Watch Behavior
    , setFollowSymLinks
    , setUnwatchMoved
    , setOneShot
    , setOnlyDir
    , WhenExists (..)
    , setWhenExists

    -- ** Events of Interest
    -- *** Root Level Events
    , setRootDeleted
    , setRootMoved

    -- *** Item Level Metadata change
    , setMetadataChanged

    -- *** Item Level Access
    , setAccessed
    , setOpened
    , setWriteClosed
    , setNonWriteClosed

    -- *** Item CRUD events
    , setCreated
    , setDeleted
    , setMovedFrom
    , setMovedTo
    , setModified

    , setAllEvents

    -- ** Watch APIs
    , watchPathsWith
    , watchPaths
    , addToWatch
    , removeFromWatch

    -- * Handling Events
    , Event
    , getRoot
    , getRelPath
    , getCookie

    -- ** Exception Conditions
    , isOverflow

    -- ** Root Level Events
    , isRootUnwatched
    , isRootDeleted
    , isRootMoved
    , isRootUnmounted

    -- ** Item Level Metadata change
    , isMetadataChanged

    -- ** Item Level Access
    , isAccessed
    , isOpened
    , isWriteClosed
    , isNonWriteClosed

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isMovedFrom
    , isMovedTo
    , isModified

    -- ** Item Path info
    , isDir

    -- * Debugging
    , showEvent
    )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.), (.&.), complement)
import Data.Foldable (foldlM)
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Lazy (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8, Word32)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, peekByteOff, sizeOf)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.FD (fdFD, mkFD)
import GHC.IO.Handle.FD (mkHandleFromFD)
import Streamly (SerialT)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array(..))
import System.IO (Handle, hClose, IOMode(ReadMode))
#if !MIN_VERSION_base(4,10,0)
import Control.Concurrent.MVar (readMVar)
import Data.Typeable (cast)
import GHC.IO.Exception (IOException(..), IOErrorType(..), ioException)
import GHC.IO.FD (FD)
import GHC.IO.Handle.Types (Handle__(..), Handle(FileHandle, DuplexHandle))
#else
import GHC.IO.Handle.FD (handleToFd)
#endif

import qualified Data.IntMap.Lazy as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.Array as A

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Internal/
--
data Config = Config
    { createFlags   :: Word32 }

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

-- | Whether a setting is 'On' or 'Off'.
--
-- /Internal/
--
data Toggle = On | Off

toggle :: Toggle -> Toggle
toggle On = Off
toggle Off = On

setFlag :: Word32 -> Toggle -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            case status of
                On -> createFlags .|. mask
                Off -> createFlags .&. complement mask
    in cfg {createFlags = flags}

-------------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------------


foreign import capi
    "sys/inotify.h value IN_DONT_FOLLOW" iN_DONT_FOLLOW :: Word32

-- | If the pathname to be watched is a symbolic link then watch the target of
-- the symbolic link instead of the symbolic link itself.
--
-- /default: On/
--
-- /Internal/
--
setFollowSymLinks :: Toggle -> Config -> Config
setFollowSymLinks s = setFlag iN_DONT_FOLLOW (toggle s)

foreign import capi
    "sys/inotify.h value IN_EXCL_UNLINK" iN_EXCL_UNLINK :: Word32

-- | If an object moves out of the directory being watched then stop watching
-- it.
--
-- /default: On/
--
-- /Internal/
--
setUnwatchMoved :: Toggle -> Config -> Config
setUnwatchMoved = setFlag iN_EXCL_UNLINK

#if HAVE_DECL_IN_MASK_CREATE
foreign import capi
    "sys/inotify.h value IN_MASK_CREATE" iN_MASK_CREATE :: Word32
#endif

foreign import capi
    "sys/inotify.h value IN_MASK_ADD" iN_MASK_ADD :: Word32

-- | What to do if a watch already exists when 'openWatch' or 'addToWatch' is
-- called for a path.
--
-- /Internal/
--
data WhenExists =
      AddIfExists -- ^ Do not set an existing setting to 'Off' only set to 'On'
    | ReplaceIfExists -- ^ Replace the existing settings with new settings
#if HAVE_DECL_IN_MASK_CREATE
    | FailIfExists -- ^ Fail the API
#endif

-- | When adding a new path to the watch, specify what to do if a watch already
-- exists on that path.
--
-- /default: FailIfExists/
--
-- /Internal/
--
setWhenExists :: WhenExists -> Config -> Config
setWhenExists val cfg =
    case val of
        AddIfExists -> setFlag iN_MASK_ADD On cfg
        ReplaceIfExists -> setFlag iN_MASK_ADD Off cfg
#if HAVE_DECL_IN_MASK_CREATE
        FailIfExists -> setFlag iN_MASK_CREATE On cfg
#endif

foreign import capi
    "sys/inotify.h value IN_ONESHOT" iN_ONESHOT :: Word32

-- | Watch the object only for one event and then remove it from the watch.
--
-- /default: Off/
--
-- /Internal/
--
setOneShot :: Toggle -> Config -> Config
setOneShot = setFlag iN_ONESHOT

foreign import capi
    "sys/inotify.h value IN_ONLYDIR" iN_ONLYDIR :: Word32

-- | Watch the object only if it is a directory. This provides a race-free way
-- to ensure that the watched object is a directory.
--
-- /default: Off/
--
-- /Internal/
--
setOnlyDir :: Toggle -> Config -> Config
setOnlyDir = setFlag iN_ONLYDIR

-------------------------------------------------------------------------------
-- Event types that can occur
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_DELETE_SELF" iN_DELETE_SELF :: Word32

-- | Report when the watched path itself gets deleted.
--
-- /default: On/
--
-- /Internal/
--
setRootDeleted :: Toggle -> Config -> Config
setRootDeleted = setFlag iN_DELETE_SELF

foreign import capi
    "sys/inotify.h value IN_MOVE_SELF" iN_MOVE_SELF :: Word32

-- | Report when the watched root path itself gets renamed.
--
-- /default: On/
--
-- /Internal/
--
setRootMoved :: Toggle -> Config -> Config
setRootMoved = setFlag iN_MOVE_SELF

foreign import capi
    "sys/inotify.h value IN_ATTRIB" iN_ATTRIB :: Word32

-- | Report when the metadata e.g. owner, permission modes, modifications times
-- of an object changes.
--
-- /default: On/
--
-- /Internal/
--
setMetadataChanged :: Toggle -> Config -> Config
setMetadataChanged = setFlag iN_ATTRIB

foreign import capi
    "sys/inotify.h value IN_ACCESS" iN_ACCESS :: Word32

-- | Report when a file is accessed.
--
-- /default: On/
--
-- /Internal/
--
setAccessed :: Toggle -> Config -> Config
setAccessed = setFlag iN_ACCESS

foreign import capi
    "sys/inotify.h value IN_OPEN" iN_OPEN :: Word32

-- | Report when a file is opened.
--
-- /default: On/
--
-- /Internal/
--
setOpened :: Toggle -> Config -> Config
setOpened = setFlag iN_OPEN

foreign import capi
    "sys/inotify.h value IN_CLOSE_WRITE" iN_CLOSE_WRITE :: Word32

-- | Report when a file that was opened for writes is closed.
--
-- /default: On/
--
-- /Internal/
--
setWriteClosed :: Toggle -> Config -> Config
setWriteClosed = setFlag iN_CLOSE_WRITE

foreign import capi
    "sys/inotify.h value IN_CLOSE_NOWRITE" iN_CLOSE_NOWRITE :: Word32

-- | Report when a file that was opened for not writing is closed.
--
-- /default: On/
--
-- /Internal/
--
setNonWriteClosed :: Toggle -> Config -> Config
setNonWriteClosed = setFlag iN_CLOSE_NOWRITE

foreign import capi
    "sys/inotify.h value IN_CREATE" iN_CREATE :: Word32

-- | Report when a file is created.
--
-- /default: On/
--
-- /Internal/
--
setCreated :: Toggle -> Config -> Config
setCreated = setFlag iN_CREATE

foreign import capi
    "sys/inotify.h value IN_DELETE" iN_DELETE :: Word32

-- | Report when a file is deleted.
--
-- /default: On/
--
-- /Internal/
--
setDeleted :: Toggle -> Config -> Config
setDeleted = setFlag iN_DELETE

foreign import capi
    "sys/inotify.h value IN_MOVED_FROM" iN_MOVED_FROM :: Word32

-- | Report the source of a move.
--
-- /default: On/
--
-- /Internal/
--
setMovedFrom :: Toggle -> Config -> Config
setMovedFrom = setFlag iN_MOVED_FROM

foreign import capi
    "sys/inotify.h value IN_MOVED_TO" iN_MOVED_TO :: Word32

-- | Report the target of a move.
--
-- /default: On/
--
-- /Internal/
--
setMovedTo :: Toggle -> Config -> Config
setMovedTo = setFlag iN_MOVED_TO

foreign import capi
    "sys/inotify.h value IN_MODIFY" iN_MODIFY :: Word32

-- | Report when a file is modified.
--
-- /default: On/
--
-- /Internal/
--
setModified :: Toggle -> Config -> Config
setModified = setFlag iN_MODIFY

-- | Set all events 'On' or 'Off'.
--
-- /default: On/
--
-- /Internal/
--
setAllEvents :: Toggle -> Config -> Config
setAllEvents s cfg =
    ( setRootDeleted s
    . setRootMoved s
    . setMetadataChanged s
    . setAccessed s
    . setOpened s
    . setWriteClosed s
    . setNonWriteClosed s
    . setCreated s
    . setDeleted s
    . setMovedFrom s
    . setMovedTo s
    . setModified s
    ) cfg

-------------------------------------------------------------------------------
-- Default config
-------------------------------------------------------------------------------

-- The defaults are set in such a way that the behavior on macOS and Linux is
-- as much compatible as possible.
--
-- | The default is:
--
-- * 'setFollowSymLinks' 'On'
-- * 'setUnwatchMoved' 'On'
-- * 'setOneShot' 'Off'
-- * 'setOnlyDir' 'Off'
-- * 'setWhenExists' 'AddIfExists'
-- * 'setAllEvents' 'On'
--
-- /Internal/
--
defaultConfig :: Config
defaultConfig =
      setWhenExists AddIfExists
    $ setAllEvents On
    $ Config { createFlags = 0 }

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A handle for a watch.
data Watch = Watch Handle (IntMap (Array Word8))

-- Instead of using the watch descriptor we can provide APIs that use the path
-- itself to identify the watch. That will require us to maintain a map from wd
-- to path in the Watch handle.

newtype WD = WD CInt deriving Show

foreign import ccall unsafe
    "sys/inotify.h inotify_init" c_inotify_init :: IO CInt

-- | Create a 'Watch' handle. 'addToWatch' can be used to add paths being
-- monitored by this watch.
--
-- /Internal/
--
createWatch :: IO Watch
createWatch = do
    rawfd <- throwErrnoIfMinus1 "createWatch" c_inotify_init
    -- we could use fdToHandle but it cannot determine the fd type
    -- automatically for the inotify fd
    (fd, fdType) <-
        mkFD
            rawfd
            ReadMode
            (Just (Stream, 0, 0))  -- (IODeviceType, CDev, CIno)
            False                  -- not a socket
            False                  -- non-blocking is false
    let fdString = "<createWatch file descriptor: " ++ show fd ++ ">"
    h <-
        mkHandleFromFD
           fd
           fdType
           fdString
           ReadMode
           True    -- use non-blocking IO
           Nothing -- TextEncoding (binary)
    return $ Watch h Map.empty

foreign import ccall unsafe
    "sys/inotify.h inotify_add_watch" c_inotify_add_watch
        :: CInt -> CString -> CUInt -> IO CInt

utf8ToString :: Array Word8 -> String
utf8ToString = runIdentity . S.toList . U.decodeUtf8 . A.toStream

#if !MIN_VERSION_base(4,10,0)
-- | Turn an existing Handle into a file descriptor. This function throws an
-- IOError if the Handle does not reference a file descriptor.
handleToFd :: Handle -> IO FD
handleToFd h = case h of
    FileHandle _ mv -> do
      Handle__{haDevice = dev} <- readMVar mv
      case cast dev of
        Just fd -> return fd
        Nothing -> throwErr "not a file descriptor"
    DuplexHandle{} -> throwErr "not a file handle"

    where

    throwErr msg = ioException $ IOError (Just h)
      InappropriateType "handleToFd" msg Nothing Nothing
#endif

-- | Given a 'Config' and a @path@ supplied as a UTF-8 encoded byte array, add
-- the path to the list of paths being monitored by the given watch 'Handle'.
-- Returns a "watch descriptor" which is used as an idenitfier in the events
-- generated for this particular path and also to remove the path from this
-- watch.
--
-- /Internal/
--
addToWatch :: Config -> Watch -> Array Word8 -> IO Watch
addToWatch Config{..} (Watch handle wdMap) path = do
    fd <- handleToFd handle
    wd <- A.asCString path $ \pathPtr ->
            throwErrnoIfMinus1 ("addToWatch " ++ utf8ToString path) $
                c_inotify_add_watch (fdFD fd) pathPtr (CUInt createFlags)
    return $ Watch handle (Map.insert (fromIntegral wd) path wdMap)

foreign import ccall unsafe
    "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch
        :: CInt -> CInt -> IO CInt

-- | Remove a path from a 'Watch', if a path was moved after adding you need to
-- need to provide the original path which was used to add the Watch.
--
-- /Internal/
--
removeFromWatch :: Watch -> Array Word8 -> IO Watch
removeFromWatch (Watch handle wdMap) path = do
    fd <- handleToFd handle
    wdMap1 <- foldlM (step fd) Map.empty (Map.toList wdMap)
    return $ Watch handle wdMap1

    where

    step fd newMap (wd, v) = do
        if v == path
        then do
            let err = "removeFromWatch " ++ show (utf8ToString path)
                rm = c_inotify_rm_watch (fdFD fd) (fromIntegral wd)
            void $ throwErrnoIfMinus1 err rm
            return newMap
        else return $ Map.insert wd v newMap

-- | Given a 'Config' and list of @paths@ (UTF-8 encoded byte arrays) start
-- monitoring the paths for file system events. Returns a 'Watch' handle which
-- can then be used to read the event stream or to close the watch.
--
-- /Internal/
--
openWatch :: Config -> NonEmpty (Array Word8) -> IO Watch
openWatch cfg paths = do
    w <- createWatch
    foldlM (\w1 pth -> addToWatch cfg w1 pth) w (NonEmpty.toList paths)

-- | Close a 'Watch' handle.
--
-- /Internal/
--
closeWatch :: Watch -> IO ()
closeWatch (Watch h _) = hClose h

-------------------------------------------------------------------------------
-- Raw events read from the watch file handle
-------------------------------------------------------------------------------

newtype Cookie = Cookie Word32 deriving (Show, Eq)

-- | An Event generated by the file system. Use the accessor functions to
-- examine the event.
--
-- /Internal/
--
data Event = Event
   { eventWd :: CInt
   , eventFlags :: Word32
   , eventCookie :: Word32
   , eventRelPath :: Array Word8
   , eventMap :: IntMap (Array Word8)
   } deriving (Show, Ord, Eq)

-- The inotify event struct from the man page/header file:
--
--            struct inotify_event {
--                int      wd;       /* Watch descriptor */
--                uint32_t mask;     /* Mask describing event */
--                uint32_t cookie;   /* Unique cookie associating related
--                                      events (for rename(2)) */
--                uint32_t len;      /* Size of name field */
--                char     name[];   /* Optional null-terminated name */
--            };
--
-- XXX We can perhaps use parseD monad instance for fusing with parseMany? Need
-- to measure the perf.
--
readOneEvent :: IntMap (Array Word8) -> Parser IO Word8 Event
readOneEvent wdMap = do
    let headerLen = (sizeOf (undefined :: CInt)) + 12
    arr <- PR.takeEQ headerLen (A.writeN headerLen)
    (ewd, eflags, cookie, pathLen) <- PR.yieldM $ A.asPtr arr readHeader
    -- XXX need the "initial" in parsers to return a step type so that "take 0"
    -- can return without an input. otherwise if pathLen is 0 we will keep
    -- waiting to read one more char before we return this event.
    path <-
        if pathLen /= 0
        then do
            -- XXX sliceSepByMax drops the separator so assumes a null
            -- terminated path, we should use a takeWhile nested inside a
            -- takeP
            pth <- PR.sliceSepByMax (== 0) pathLen (A.writeN pathLen)
            let remaining = pathLen - A.length pth - 1
            when (remaining /= 0) $ PR.takeEQ remaining FL.drain
            return pth
        else return $ A.fromList []
    return $ Event
        { eventWd = (fromIntegral ewd)
        , eventFlags = eflags
        , eventCookie = cookie
        , eventRelPath = path
        , eventMap = wdMap
        }

    where

    readHeader (ptr :: Ptr Word8) = do
        let len = sizeOf (undefined :: CInt)
        ewd <- peek ptr
        eflags <- peekByteOff ptr len
        cookie <- peekByteOff ptr (len + 4)
        pathLen :: Word32 <- peekByteOff ptr (len + 8)
        return (ewd, eflags, cookie, fromIntegral pathLen)

watchToStream :: Watch -> SerialT IO Event
watchToStream (Watch handle wdMap) =
    -- Do not use too small a buffer. As per inotify man page:
    --
    -- The behavior when the buffer given to read(2) is too small to return
    -- information about the next event depends on the kernel version: in
    -- kernels before 2.6.21, read(2) returns 0; since kernel 2.6.21, read(2)
    -- fails with the error EINVAL.  Specifying a buffer of size
    --
    --          sizeof(struct inotify_event) + NAME_MAX + 1
    --
    -- will be sufficient to read at least one event.
    S.parseMany (readOneEvent wdMap) $ S.unfold FH.read handle

-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories. When the path is a directory, only the
-- files and directories directly under the watched directory are monitored,
-- contents of subdirectories are not monitored.  Monitoring starts from the
-- current time onwards. The paths are specified as UTF-8 encoded 'Array' of
-- 'Word8'.
--
-- @
-- watchPathsWith
--  ('setFollowSymLinks' On . 'setUnwatchMoved' Off)
--  [Array.fromCString\# "dir"#]
-- @
--
-- /Internal/
--
watchPathsWith ::
    (Config -> Config) -> NonEmpty (Array Word8) -> SerialT IO Event
watchPathsWith f paths = S.bracket before after watchToStream

    where

    before = liftIO $ openWatch (f defaultConfig) paths
    after = liftIO . closeWatch

-- | Like 'watchPathsWith' but uses the 'defaultConfig' options.
--
-- @
-- watchPaths = watchPathsWith id
-- @
--
-- /Internal/
--
watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event
watchPaths = watchPathsWith id

-------------------------------------------------------------------------------
-- Examine event stream
-------------------------------------------------------------------------------

-- | Get the watch descriptor corresponding to the 'Event'. A watch descriptor
-- identifies which of the paths being monitored has generated this event.
--
-- Note that if a path was moved after adding to the watch, this will give the
-- original path and not the new path after moving.
--
-- /Internal/
--
getRoot :: Event -> Array Word8
getRoot Event{..} =
    if (eventWd >= 1)
    then
        case Map.lookup (fromIntegral eventWd) eventMap of
            Just path -> path
            Nothing ->
                error $ "Bug: getRoot: No path found corresponding to the "
                    ++ "watch descriptor " ++ show eventWd
    else A.fromList []


-- XXX should we use a Maybe here?
-- | Get the file system object path for which the event is generated, relative
-- to the watched root. The path is a UTF-8 encoded array of bytes.
--
-- /Internal/
--
getRelPath :: Event -> Array Word8
getRelPath Event{..} = eventRelPath

-- XXX should we use a Maybe?
-- | Cookie is set when a rename occurs. The cookie value can be used to
-- connect the 'isMovedFrom' and 'isMovedTo' events, if both the events belong
-- to the same move operation then they will have the same cookie value.
--
-- /Internal/
--
getCookie :: Event -> Cookie
getCookie Event{..} = Cookie eventCookie

-------------------------------------------------------------------------------
-- Event types
-------------------------------------------------------------------------------

getFlag :: Word32 -> Event -> Bool
getFlag mask Event{..} = eventFlags .&. mask /= 0

-------------------------------------------------------------------------------
-- Error events
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_Q_OVERFLOW" iN_Q_OVERFLOW :: Word32

-- XXX rename to isQOverflowed or hasOverflowed?
--
-- macOS overflow API is more specific, it tells which paths have lost the
-- events due to overflow.
--
-- | Event queue overflowed (WD is invalid for this event) and we may have lost
-- some events..  The user application must scan everything under the watched
-- paths to know the current state.
--
-- /Internal/
--
isOverflow :: Event -> Bool
isOverflow = getFlag iN_Q_OVERFLOW

-------------------------------------------------------------------------------
-- Events affecting the watched path only
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_IGNORED" iN_IGNORED :: Word32

-- Compare with isRootChanged on macOS. isRootChanged includes all these cases.
--
-- | A path was removed from the watch explicitly using 'removeFromWatch' or
-- automatically (file was deleted, or filesystem was unmounted).
--
-- /Occurs only for a watched path/
--
-- /Internal/
--
isRootUnwatched :: Event -> Bool
isRootUnwatched = getFlag iN_IGNORED

-- | Watched file/directory was itself deleted.  (This event also occurs if an
-- object is moved to another filesystem, since mv(1) in effect copies the file
-- to the other filesystem and then deletes it from the original filesystem.)
-- In addition, an 'isRootUnwatched' event will subsequently be generated
-- for the watch descriptor.
--
-- /Occurs only for a watched path/
--
-- /Internal/
--
isRootDeleted :: Event -> Bool
isRootDeleted = getFlag iN_DELETE_SELF

-- | Watched file/directory was itself moved within the file system.
--
-- /Occurs only for a watched path/
--
-- /Internal/
--
isRootMoved :: Event -> Bool
isRootMoved = getFlag iN_MOVE_SELF

foreign import capi
    "sys/inotify.h value IN_UNMOUNT" iN_UNMOUNT :: Word32

-- | Filesystem containing watched object was unmounted.  In addition, an
-- 'isRootUnwatched' event will subsequently be generated for the watch
-- descriptor.
--
-- /Occurs only for a watched path/
--
-- /Internal/
--
isRootUnmounted :: Event -> Bool
isRootUnmounted = getFlag iN_UNMOUNT

-------------------------------------------------------------------------------
-- Metadata change Events
-------------------------------------------------------------------------------

-- macOS has multiple APIs for metadata change for different metadata.
--
-- | Determine whether the event indicates inode metadata change for an object
-- contained within the monitored path.
--
-- Metadata change may include, permissions (e.g., chmod(2)), timestamps
-- (e.g., utimensat(2)), extended attributes (setxattr(2)), link count (since
-- Linux 2.6.25; e.g., for the target of link(2) and for unlink(2)), and
-- user/group ID (e.g., chown(2)).
--
-- /Can occur for watched path or a file inside it/
--
-- /Internal/
--
isMetadataChanged :: Event -> Bool
isMetadataChanged = getFlag iN_ATTRIB

-------------------------------------------------------------------------------
-- Access
-------------------------------------------------------------------------------

-- | File was accessed (e.g. read, execve).
--
-- /Occurs only for a file inside the watched directory/
--
-- /Internal/
--
isAccessed :: Event -> Bool
isAccessed = getFlag iN_ACCESS

-- | File or directory was opened.
--
-- /Occurs only for a file inside the watched directory/
--
-- /Internal/
--
isOpened :: Event -> Bool
isOpened = getFlag iN_OPEN

-- | File opened for writing was closed.
--
-- /Occurs only for a file inside the watched directory/
--
-- /Internal/
--
isWriteClosed :: Event -> Bool
isWriteClosed = getFlag iN_CLOSE_WRITE

-- XXX what if it was opened for append? Does NOWRITE mean all cases where the
-- mode was not write? A dir open comes in this category?
--
-- | File or directory opened for read but not write was closed.
--
-- /Can occur for watched path or a file inside it/
--
-- /Internal/
--
isNonWriteClosed :: Event -> Bool
isNonWriteClosed = getFlag iN_CLOSE_NOWRITE

-------------------------------------------------------------------------------
-- CRUD Events
-------------------------------------------------------------------------------

-- On macOS this is not generated on hard linking but on Linux it is.
--
-- | File/directory created in watched directory (e.g., open(2) O_CREAT,
-- mkdir(2), link(2), symlink(2), bind(2) on a UNIX domain socket).
--
-- /Occurs only for an object inside the watched directory/
--
-- /Internal/
--
isCreated :: Event -> Bool
isCreated = getFlag iN_CREATE

-- | File/directory deleted from watched directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Internal/
--
isDeleted :: Event -> Bool
isDeleted = getFlag iN_DELETE

-- XXX what if an object is moved in from outside or moved out of the monitored
-- dir?
--
-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Internal/
--
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag iN_MOVED_FROM

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Internal/
--
isMovedTo :: Event -> Bool
isMovedTo = getFlag iN_MOVED_TO

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Internal/
--
isModified :: Event -> Bool
isModified = getFlag iN_MODIFY

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'On')
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_ISDIR" iN_ISDIR :: Word32

-- | Determine whether the event is for a directory path.
--
-- /Internal/
--
isDir :: Event -> Bool
isDir = getFlag iN_ISDIR

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
       "--------------------------"
    ++ "\nWd = " ++ show eventWd
    ++ "\nRoot = " ++ show (utf8ToString $ getRoot ev)
    ++ "\nPath = " ++ show (utf8ToString $ getRelPath ev)
    ++ "\nCookie = " ++ show (getCookie ev)
    ++ "\nFlags " ++ show eventFlags

    ++ showev isOverflow "Overflow"

    ++ showev isRootUnwatched "RootUnwatched"
    ++ showev isRootDeleted "RootDeleted"
    ++ showev isRootMoved "RootMoved"
    ++ showev isRootUnmounted "RootUnmounted"

    ++ showev isMetadataChanged "MetadataChanged"

    ++ showev isAccessed "Accessed"
    ++ showev isOpened "Opened"
    ++ showev isWriteClosed "WriteClosed"
    ++ showev isNonWriteClosed "NonWriteClosed"

    ++ showev isCreated "Created"
    ++ showev isDeleted "Deleted"
    ++ showev isModified "Modified"
    ++ showev isMovedFrom "MovedFrom"
    ++ showev isMovedTo "MovedTo"

    ++ showev isDir "Dir"
    ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""
#else
#warning "Disabling module Streamly.Internal.FileSystem.Event.Linux. Does not support kernels older than 2.6.36."
module Streamly.Internal.FileSystem.Event.Linux () where
#endif
