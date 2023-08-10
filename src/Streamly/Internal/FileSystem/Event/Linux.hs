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
-- Stream.mapM_ (putStrLn . 'showEvent') $ 'watchPaths' [Array.fromList "dir"]
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
      Config (..)
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode
    , setFollowSymLinks
    , setUnwatchMoved
    , setOneShot
    , setOnlyDir
    , WhenExists (..)
    , setWhenExists

    -- ** Events of Interest
    -- *** Root Path Events
    , setRootDeleted
    , setRootMoved
    , setRootPathEvents

    -- *** Item Level Metadata change
    , setAttrsModified

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
    , watch
    , watchRecursive
    , watchWith

    -- Low level watch APIs
    , addToWatch
    , removeFromWatch

    -- * Handling Events
    , Event(..)
    , getRoot
    , getRelPath
    , getAbsPath
    , getCookie

    -- ** Root Level Events
    , isRootPathEvent
    , isRootUnwatched
    , isRootDeleted
    , isRootMoved
    , isRootUnmounted

    -- ** Item Level Metadata change
    , isAttrsModified

    -- ** Item Level Access
    , isAccessed
    , isOpened
    , isWriteClosed
    , isNonWriteClosed

    -- ** Item Level CRUD events
    , isCreated
    , isDeleted
    , isMovedFrom
    , isMovedTo
    , isMoved
    , isModified

    -- ** Item Path info
    , isDir

    -- ** Exception Conditions
    , isEventsLost

    -- * Debugging
    , showEvent
    )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Lazy (IntMap)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List.NonEmpty (NonEmpty)
#if !(MIN_VERSION_base(4,13,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8, Word32)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, peekByteOff, sizeOf)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.FD (fdFD, mkFD)
import GHC.IO.Handle.FD (mkHandleFromFD)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array (Array(..), byteLength)
import System.Directory (doesDirectoryExist)
import System.IO (Handle, hClose, IOMode(ReadMode))
import GHC.IO.Handle.FD (handleToFd)

import qualified Data.IntMap.Lazy as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
    (takeEQ, fromEffect, fromFold)
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.FileSystem.Dir as Dir
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Unicode.Stream as U

-------------------------------------------------------------------------------
-- Subscription to events
-------------------------------------------------------------------------------

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Pre-release/
--
data Config = Config
    { watchRec :: Bool
    , createFlags :: Word32
    }

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

-------------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------------

-- | Watch the whole directory tree recursively instead of watching just one
-- level of directory.
--
-- /default: False/
--
-- /Pre-release/
--
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

foreign import capi
    "sys/inotify.h value IN_DONT_FOLLOW" iN_DONT_FOLLOW :: Word32

-- | If the pathname to be watched is a symbolic link then watch the target of
-- the symbolic link instead of the symbolic link itself.
--
-- Note that the path location in the events is through the original symbolic
-- link path rather than the resolved path.
--
-- /default: True/
--
-- /Pre-release/
--
setFollowSymLinks :: Bool -> Config -> Config
setFollowSymLinks s = setFlag iN_DONT_FOLLOW (not s)

foreign import capi
    "sys/inotify.h value IN_EXCL_UNLINK" iN_EXCL_UNLINK :: Word32

-- | If an object moves out of the directory being watched then stop watching
-- it.
--
-- /default: True/
--
-- /Pre-release/
--
setUnwatchMoved :: Bool -> Config -> Config
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
-- /Pre-release/
--
data WhenExists =
      AddIfExists -- ^ Do not set an existing setting to 'False' only set to 'True'
    | ReplaceIfExists -- ^ Replace the existing settings with new settings
#if HAVE_DECL_IN_MASK_CREATE
    | FailIfExists -- ^ Fail the API
#endif

-- | When adding a new path to the watch, specify what to do if a watch already
-- exists on that path.
--
-- /default: FailIfExists/
--
-- /Pre-release/
--
setWhenExists :: WhenExists -> Config -> Config
setWhenExists val cfg =
    case val of
        AddIfExists -> setFlag iN_MASK_ADD True cfg
        ReplaceIfExists -> setFlag iN_MASK_ADD False cfg
#if HAVE_DECL_IN_MASK_CREATE
        FailIfExists -> setFlag iN_MASK_CREATE True cfg
#endif

foreign import capi
    "sys/inotify.h value IN_ONESHOT" iN_ONESHOT :: Word32

-- | Watch the object only for one event and then remove it from the watch.
--
-- /default: False/
--
-- /Pre-release/
--
setOneShot :: Bool -> Config -> Config
setOneShot = setFlag iN_ONESHOT

foreign import capi
    "sys/inotify.h value IN_ONLYDIR" iN_ONLYDIR :: Word32

-- | Watch the object only if it is a directory. This provides a race-free way
-- to ensure that the watched object is a directory.
--
-- /default: False/
--
-- /Pre-release/
--
setOnlyDir :: Bool -> Config -> Config
setOnlyDir = setFlag iN_ONLYDIR

-------------------------------------------------------------------------------
-- Event types that can occur
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_DELETE_SELF" iN_DELETE_SELF :: Word32

-- | Report when the watched path itself gets deleted.
--
-- /default: True/
--
-- /Pre-release/
--
setRootDeleted :: Bool -> Config -> Config
setRootDeleted = setFlag iN_DELETE_SELF

foreign import capi
    "sys/inotify.h value IN_MOVE_SELF" iN_MOVE_SELF :: Word32

-- | Report when the watched root path itself gets renamed.
--
-- /default: True/
--
-- /Pre-release/
--
setRootMoved :: Bool -> Config -> Config
setRootMoved = setFlag iN_MOVE_SELF

-- | Report when the watched root path itself gets deleted or renamed.
--
-- /default: True/
--
-- /Pre-release/
--
setRootPathEvents :: Bool -> Config -> Config
setRootPathEvents = setFlag (iN_DELETE_SELF .|. iN_MOVE_SELF)

foreign import capi
    "sys/inotify.h value IN_ATTRIB" iN_ATTRIB :: Word32

-- | Report when the metadata e.g. owner, permission modes, modifications times
-- of an object changes.
--
-- /default: True/
--
-- /Pre-release/
--
setAttrsModified :: Bool -> Config -> Config
setAttrsModified = setFlag iN_ATTRIB

foreign import capi
    "sys/inotify.h value IN_ACCESS" iN_ACCESS :: Word32

-- | Report when a file is accessed.
--
-- /default: True/
--
-- /Pre-release/
--
setAccessed :: Bool -> Config -> Config
setAccessed = setFlag iN_ACCESS

foreign import capi
    "sys/inotify.h value IN_OPEN" iN_OPEN :: Word32

-- | Report when a file is opened.
--
-- /default: True/
--
-- /Pre-release/
--
setOpened :: Bool -> Config -> Config
setOpened = setFlag iN_OPEN

foreign import capi
    "sys/inotify.h value IN_CLOSE_WRITE" iN_CLOSE_WRITE :: Word32

-- | Report when a file that was opened for writes is closed.
--
-- /default: True/
--
-- /Pre-release/
--
setWriteClosed :: Bool -> Config -> Config
setWriteClosed = setFlag iN_CLOSE_WRITE

foreign import capi
    "sys/inotify.h value IN_CLOSE_NOWRITE" iN_CLOSE_NOWRITE :: Word32

-- | Report when a file that was opened for not writing is closed.
--
-- /default: True/
--
-- /Pre-release/
--
setNonWriteClosed :: Bool -> Config -> Config
setNonWriteClosed = setFlag iN_CLOSE_NOWRITE

foreign import capi
    "sys/inotify.h value IN_CREATE" iN_CREATE :: Word32

-- | Report when a file is created.
--
-- /default: True/
--
-- /Pre-release/
--
setCreated :: Bool -> Config -> Config
setCreated = setFlag iN_CREATE

foreign import capi
    "sys/inotify.h value IN_DELETE" iN_DELETE :: Word32

-- | Report when a file is deleted.
--
-- /default: True/
--
-- /Pre-release/
--
setDeleted :: Bool -> Config -> Config
setDeleted = setFlag iN_DELETE

foreign import capi
    "sys/inotify.h value IN_MOVED_FROM" iN_MOVED_FROM :: Word32

-- | Report the source of a move.
--
-- /default: True/
--
-- /Pre-release/
--
setMovedFrom :: Bool -> Config -> Config
setMovedFrom = setFlag iN_MOVED_FROM

foreign import capi
    "sys/inotify.h value IN_MOVED_TO" iN_MOVED_TO :: Word32

-- | Report the target of a move.
--
-- /default: True/
--
-- /Pre-release/
--
setMovedTo :: Bool -> Config -> Config
setMovedTo = setFlag iN_MOVED_TO

foreign import capi
    "sys/inotify.h value IN_MODIFY" iN_MODIFY :: Word32

-- | Report when a file is modified.
--
-- /default: True/
--
-- /Pre-release/
--
setModified :: Bool -> Config -> Config
setModified = setFlag iN_MODIFY

-- | Set all tunable events 'True' or 'False'. Equivalent to setting:
--
-- * setRootDeleted
-- * setRootMoved
-- * setAttrsModified
-- * setAccessed
-- * setOpened
-- * setWriteClosed
-- * setNonWriteClosed
-- * setCreated
-- * setDeleted
-- * setMovedFrom
-- * setMovedTo
-- * setModified
--
-- /Pre-release/
--
setAllEvents :: Bool -> Config -> Config
setAllEvents s =
      setRootDeleted s
    . setRootMoved s
    . setAttrsModified s
    . setAccessed s
    . setOpened s
    . setWriteClosed s
    . setNonWriteClosed s
    . setCreated s
    . setDeleted s
    . setMovedFrom s
    . setMovedTo s
    . setModified s

-------------------------------------------------------------------------------
-- Default config
-------------------------------------------------------------------------------

-- The defaults are set in such a way that the behavior on macOS and Linux is
-- as much compatible as possible.
--
-- | The default configuration settings are:
--
-- * 'setFollowSymLinks' 'True'
-- * 'setUnwatchMoved' 'True'
-- * 'setOneShot' 'False'
-- * 'setOnlyDir' 'False'
-- * 'setWhenExists' 'AddIfExists'
--
-- The tunable events enabled by default are:
--
-- * setCreated True
-- * setDeleted True
-- * setMovedFrom True
-- * setMovedTo True
-- * setModified True
--
-- /Pre-release/
--
defaultConfig :: Config
defaultConfig =
      setWhenExists AddIfExists
    $ setCreated True
    $ setDeleted True
    $ setMovedFrom True
    $ setMovedTo True
    $ setModified True
    $ Config
        { watchRec = False
        , createFlags = 0
        }

-------------------------------------------------------------------------------
-- Open an event stream
-------------------------------------------------------------------------------

-- | A handle for a watch.
data Watch =
    Watch
        Handle                  -- File handle for the watch
        (IORef
            (IntMap             -- Key is the watch descriptor
                ( Array Word8   -- Absolute path of the watch root
                , Array Word8   -- Path of subdir relative to watch root
                )
            )
        )

-- Instead of using the watch descriptor we can provide APIs that use the path
-- itself to identify the watch. That will require us to maintain a map from wd
-- to path in the Watch handle.

newtype WD = WD CInt deriving Show

foreign import ccall unsafe
    "sys/inotify.h inotify_init" c_inotify_init :: IO CInt

-- | Create a 'Watch' handle. 'addToWatch' can be used to add paths being
-- monitored by this watch.
--
-- /Pre-release/
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
    emptyMapRef <- newIORef Map.empty
    return $ Watch h emptyMapRef

foreign import ccall unsafe
    "sys/inotify.h inotify_add_watch" c_inotify_add_watch
        :: CInt -> CString -> CUInt -> IO CInt

-- XXX we really do not know the path encoding, all we know is that it is "/"
-- separated bytes. So these may fail or convert the path in an unexpected
-- manner. We should ultimately remove all usage of these.

toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = A.fromStream . U.encodeUtf8 . S.fromList

utf8ToString :: Array Word8 -> String
utf8ToString = runIdentity . S.fold FL.toList . U.decodeUtf8' . A.read

-- | Add a trailing "/" at the end of the path if there is none. Do not add a
-- "/" if the path is empty.
--
ensureTrailingSlash :: Array Word8 -> Array Word8
ensureTrailingSlash path =
    if byteLength path /= 0
    then
        let mx = A.getIndex (byteLength path - 1) path
         in case mx of
            Nothing -> error "ensureTrailingSlash: Bug: Invalid index"
            Just x ->
                if x /= forwardSlashByte
                then path <> forwardSlash
                else path
    else path
    where forwardSlash = A.fromList [ forwardSlashByte ]
          forwardSlashByte = fromIntegral (ord '/')

removeTrailingSlash :: Array Word8 -> Array Word8
removeTrailingSlash path =
    if byteLength path /= 0
    then
        let n = byteLength path - 1
            mx = A.getIndex n path
         in case mx of
            Nothing -> error "removeTrailingSlash: Bug: Invalid index"
            Just x ->
                if x == fromIntegral (ord '/')
                then A.getSliceUnsafe 0 n path
                else path
    else path

appendPaths :: Array Word8 -> Array Word8 -> Array Word8
appendPaths a b
  | byteLength a == 0 = b
  | byteLength b == 0 = a
  | otherwise = ensureTrailingSlash a <> b

-- | @addToWatch cfg watch root subpath@ adds @subpath@ to the list of paths
-- being monitored under @root@ via the watch handle @watch@.  @root@ must be
-- an absolute path and @subpath@ must be relative to @root@.
--
-- /Pre-release/
--
addToWatch :: Config -> Watch -> Array Word8 -> Array Word8 -> IO ()
addToWatch cfg@Config{..} watch0@(Watch handle wdMap) root0 path0 = do
    -- XXX do not add if the path is already added
    -- XXX if the watch is added by the scan and not via an event we can
    -- generate a create event assuming that the create may have been lost. We
    -- can also mark in the map that this entry was added by the scan. So if an
    -- actual create event later comes and tries to add this again then we can
    -- ignore that and drop the create event to avoid duplicate create, because
    -- we have already emitted it.
    --
    -- When a directory is added by the scan we should also emit create events
    -- for files that may have got added to the dir. However, such create
    -- events may get duplicated because of a race between the scan generated
    -- versus real events.
    --
    -- Or we may distinguish between scan generated events and real events so
    -- that the application can assume that other events may been lost and
    -- handle it. For example, if it is a dir create the application can read
    -- the dir to scan the files in it.
    --
    let root = removeTrailingSlash root0
        path = removeTrailingSlash path0
        absPath = appendPaths root path
    putStrLn $ "root = " ++ utf8ToString root ++ " path = " ++ utf8ToString path ++ " absPath = " ++ utf8ToString absPath

    fd <- handleToFd handle

    -- XXX we need to tolerate an error where we are adding a watch for a
    -- non-existing file because the file may have got deleted by the time we
    -- added the watch. Perhaps we can have a flag in config for this and keep
    -- the default value to tolerate the error.
    --
    -- XXX The file may have even got deleted and then recreated which we will
    -- never get to know, document this.
    wd <- A.asCStringUnsafe absPath $ \pathPtr ->
            throwErrnoIfMinus1 ("addToWatch: " ++ utf8ToString absPath) $
                c_inotify_add_watch (fdFD fd) pathPtr (CUInt createFlags)

    -- We add the parent first so that we start getting events for any new
    -- creates and add the new subdirectories on creates while we are adding
    -- the children.
    modifyIORef wdMap (Map.insert (fromIntegral wd) (root, path))

    -- Now add the children. If we missed any creates while we were adding the
    -- parent, this will make sure they are added too.
    --
    -- XXX Ensure that we generate events that we may have missed while we were
    -- adding the dirs. That may generate spurious events though.
    --
    -- XXX readDirs currently uses paths as String, we need to convert it
    -- to "/" separated by byte arrays.
    pathIsDir <- doesDirectoryExist $ utf8ToString absPath
    when (watchRec && pathIsDir) $ do
        let f = addToWatch cfg watch0 root . appendPaths path
            in S.fold (FL.drainMapM f)
                $ S.mapM toUtf8
                $ Dir.readDirs $ utf8ToString absPath

foreign import ccall unsafe
    "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch
        :: CInt -> CInt -> IO CInt

-- | Remove an absolute root path from a 'Watch', if a path was moved after
-- adding you need to provide the original path which was used to add the
-- Watch.
--
-- /Pre-release/
--
removeFromWatch :: Watch -> Array Word8 -> IO ()
removeFromWatch (Watch handle wdMap) path = do
    fd <- handleToFd handle
    km <- readIORef wdMap
    wdMap1 <- foldlM (step fd) Map.empty (Map.toList km)
    writeIORef wdMap wdMap1

    where

    step fd newMap (wd, v) = do
        if fst v == path
        then do
            let err = "removeFromWatch: " ++ show (utf8ToString path)
                rm = c_inotify_rm_watch (fdFD fd) (fromIntegral wd)
            void $ throwErrnoIfMinus1 err rm
            return newMap
        else return $ Map.insert wd v newMap

-- | Given a 'Config' and list of @paths@ ("/" separated byte arrays) start
-- monitoring the paths for file system events. Returns a 'Watch' handle which
-- can then be used to read the event stream or to close the watch.
--
-- /Pre-release/
--
openWatch :: Config -> NonEmpty (Array Word8) -> IO Watch
openWatch cfg paths = do
    w <- createWatch
    mapM_
        (\root -> addToWatch cfg w root (A.fromList []))
        $ NonEmpty.toList paths
    return w

-- | Close a 'Watch' handle.
--
-- /Pre-release/
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
-- /Pre-release/
--
data Event = Event
   { eventWd :: CInt
   , eventFlags :: Word32
   , eventCookie :: Word32
   , eventRelPath :: Array Word8
   , eventMap :: IntMap (Array Word8, Array Word8)
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
readOneEvent :: Config -> Watch -> Parser Word8 IO Event
readOneEvent cfg  wt@(Watch _ wdMap) = do
    let headerLen = sizeOf (undefined :: CInt) + 12
    arr <- PR.takeEQ headerLen (A.writeN headerLen)
    (ewd, eflags, cookie, pathLen) <- PR.fromEffect $ A.asPtrUnsafe arr readHeader
    -- XXX need the "initial" in parsers to return a step type so that "take 0"
    -- can return without an input. otherwise if pathLen is 0 we will keep
    -- waiting to read one more char before we return this event.
    path <-
        if pathLen /= 0
        then do
            -- XXX takeEndBy_ drops the separator so assumes a null
            -- terminated path, we should use a takeWhile nested inside a
            -- takeP
            pth <-
                PR.fromFold
                    $ FL.takeEndBy_ (== 0)
                    $ FL.take pathLen (A.writeN pathLen)
            let remaining = pathLen - byteLength pth - 1
            when (remaining /= 0) $ PR.takeEQ remaining FL.drain
            return pth
        else return $ A.fromList []
    wdm <- PR.fromEffect $ readIORef wdMap
    let (root, sub) =
            case Map.lookup (fromIntegral ewd) wdm of
                    Just pair -> pair
                    Nothing ->
                        error $ "readOneEvent: "
                                  <> "Unknown watch descriptor: "
                                  <> show ewd
    let sub1 = appendPaths sub path
        -- Check for "ISDIR" first because it is less likely
        isDirCreate = eflags .&. iN_ISDIR /= 0 && eflags .&. iN_CREATE /= 0
    when (watchRec cfg && isDirCreate)
        $ PR.fromEffect $ addToWatch cfg wt root sub1
    -- XXX Handle IN_DELETE, IN_DELETE_SELF, IN_MOVE_SELF, IN_MOVED_FROM,
    -- IN_MOVED_TO
    -- What if a large dir tree gets moved in to our hierarchy? Do we get a
    -- single event for the top level dir in this case?
    return $ Event
        { eventWd = fromIntegral ewd
        , eventFlags = eflags
        , eventCookie = cookie
        , eventRelPath = sub1
        , eventMap = wdm
        }

    where

    readHeader (ptr :: Ptr Word8) = do
        let len = sizeOf (undefined :: CInt)
        ewd <- peek ptr
        eflags <- peekByteOff ptr len
        cookie <- peekByteOff ptr (len + 4)
        pathLen :: Word32 <- peekByteOff ptr (len + 8)
        return (ewd, eflags, cookie, fromIntegral pathLen)

watchToStream :: Config -> Watch -> Stream IO Event
watchToStream cfg wt@(Watch handle _) = do
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
    S.catRights $ S.parseMany (readOneEvent cfg wt) $ S.unfold FH.reader handle

-- XXX We should not go across the mount points of network file systems or file
-- systems that are known to not generate any events.
--
-- | Start monitoring a list of file system paths for file system events with
-- the supplied configuration operation over the 'defaultConfig'. The
-- paths could be files or directories.  When recursive mode is set and the
-- path is a directory, the whole directory tree under it is watched
-- recursively. Monitoring starts from the current time onwards.  The paths are
-- specified as UTF-8 encoded 'Array' of 'Word8'.
--
-- /Non-existing Paths:/ the API fails if a watch is started on a non-exsting
-- path.
--
-- /Performance:/ Note that recursive watch on a large directory tree could be
-- expensive. When starting a watch, the whole tree must be read and watches
-- are started on each directory in the tree. The initial time to start the
-- watch as well as the memory required is proportional to the number of
-- directories in the tree.
--
-- /Bugs:/ When new directories are created under the tree they are added to
-- the watch on receiving the directory create event. However, the creation of
-- a dir and adding a watch for it is not atomic.  The implementation takes
-- care of this and makes sure that watches are added for all directories.
-- However, In the mean time, the directory may have received more events which
-- may get lost.  Handling of any such lost events is yet to be implemented.
--
-- See the Linux __inotify__ man page for more details.
--
-- @
-- watchwith
--      ('setFollowSymLinks' True . 'setUnwatchMoved' False)
--      [Array.fromList "dir"]
-- @
--
-- /Pre-release/
--
watchWith :: (Config -> Config) -> NonEmpty (Array Word8) -> Stream IO Event
watchWith f paths = S.bracketIO before after (watchToStream cfg)

    where

    cfg = f defaultConfig
    before = openWatch cfg paths
    after = closeWatch

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
--
-- See 'watchWith' for pitfalls and bugs when using recursive watch on Linux.
--
-- /Pre-release/
--
watchRecursive :: NonEmpty (Array Word8) -> Stream IO Event
watchRecursive = watchWith (setRecursiveMode True)

-- | Same as 'watchWith' using defaultConfig and non-recursive mode.
--
-- >>> watch = watchWith id
--
-- /Pre-release/
--
watch :: NonEmpty (Array Word8) -> Stream IO Event
watch = watchWith id

-------------------------------------------------------------------------------
-- Examine event stream
-------------------------------------------------------------------------------

-- | Get the watch root corresponding to the 'Event'.
--
-- Note that if a path was moved after adding to the watch, this will give the
-- original path and not the new path after moving.
--
-- TBD: we can possibly update the watch root on a move self event.
--
-- /Pre-release/
--
getRoot :: Event -> Array Word8
getRoot Event{..} =
    if eventWd >= 1
    then
        case Map.lookup (fromIntegral eventWd) eventMap of
            Just path -> fst path
            Nothing ->
                error $ "Bug: getRoot: No path found corresponding to the "
                    ++ "watch descriptor " ++ show eventWd
    else A.fromList []

-- XXX should we use a Maybe here?
-- | Get the file system object path for which the event is generated, relative
-- to the watched root. The path is a "/" separated array of bytes.
--
-- /Pre-release/
--
getRelPath :: Event -> Array Word8
getRelPath Event{..} = eventRelPath

-- | Get the absolute file system object path for which the event is generated.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- /Pre-release/
--
getAbsPath :: Event -> Array Word8
getAbsPath ev =
    let relpath = getRelPath ev
        root = getRoot ev
    in  if byteLength relpath /= 0
        then ensureTrailingSlash root <> relpath
        else removeTrailingSlash root

-- XXX should we use a Maybe?
-- | Cookie is set when a rename occurs. The cookie value can be used to
-- connect the 'isMovedFrom' and 'isMovedTo' events, if both the events belong
-- to the same move operation then they will have the same cookie value.
--
-- /Pre-release/
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
-- /Pre-release/
--
isEventsLost :: Event -> Bool
isEventsLost = getFlag iN_Q_OVERFLOW

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
-- Note that in recursive watch mode all the subdirectories are watch roots,
-- therefore, they will all generate this event.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
--
isRootUnwatched :: Event -> Bool
isRootUnwatched = getFlag iN_IGNORED

-- | Watched file/directory was itself deleted.  (This event also occurs if an
-- object is moved to another filesystem, since mv(1) in effect copies the file
-- to the other filesystem and then deletes it from the original filesystem.)
-- In addition, an 'isRootUnwatched' event will subsequently be generated
-- for the watch descriptor.
--
-- Note that in recursive watch mode all the subdirectories are watch roots,
-- therefore, they will all generate this event.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
--
isRootDeleted :: Event -> Bool
isRootDeleted = getFlag iN_DELETE_SELF

-- | Watched file/directory was itself moved within the file system.
--
-- Note that in recursive watch mode all the subdirectories are watch roots,
-- therefore, they will all generate this event.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
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
-- /Pre-release/
--
isRootUnmounted :: Event -> Bool
isRootUnmounted = getFlag iN_UNMOUNT

-- | Determine whether the event indicates a change of path of the monitored
-- object itself. Note that the object may become unreachable or deleted after
-- a change of path.
--
-- /Occurs only for a watched path/
--
-- /Pre-release/
--
isRootPathEvent :: Event -> Bool
isRootPathEvent = getFlag (iN_DELETE_SELF .|. iN_MOVE_SELF .|. iN_UNMOUNT)

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
-- /Pre-release/
--
isAttrsModified :: Event -> Bool
isAttrsModified = getFlag iN_ATTRIB

-------------------------------------------------------------------------------
-- Access
-------------------------------------------------------------------------------

-- | File was accessed (e.g. read, execve).
--
-- /Occurs only for a file inside the watched directory/
--
-- /Pre-release/
--
isAccessed :: Event -> Bool
isAccessed = getFlag iN_ACCESS

-- | File or directory was opened.
--
-- /Occurs only for a file inside the watched directory/
--
-- /Pre-release/
--
isOpened :: Event -> Bool
isOpened = getFlag iN_OPEN

-- | File opened for writing was closed.
--
-- /Occurs only for a file inside the watched directory/
--
-- /Pre-release/
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
-- /Pre-release/
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
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = getFlag iN_CREATE

-- | File/directory deleted from watched directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
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
-- /Pre-release/
--
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag iN_MOVED_FROM

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isMovedTo :: Event -> Bool
isMovedTo = getFlag iN_MOVED_TO

-- | Generated for a path that is moved from or moved to the monitored
-- directory.
--
-- >>> isMoved ev = isMovedFrom ev || isMovedTo ev
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved ev = isMovedFrom ev || isMovedTo ev

-- | Determine whether the event indicates modification of an object within the
-- monitored path. This event is generated only for files and not directories.
--
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = getFlag iN_MODIFY

-------------------------------------------------------------------------------
-- Information about path type (applicable only when 'setFileEvents' is 'True')
-------------------------------------------------------------------------------

foreign import capi
    "sys/inotify.h value IN_ISDIR" iN_ISDIR :: Word32

-- | Determine whether the event is for a directory path.
--
-- /Pre-release/
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

    ++ showev isEventsLost "Overflow"

    ++ showev isRootUnwatched "RootUnwatched"
    ++ showev isRootDeleted "RootDeleted"
    ++ showev isRootMoved "RootMoved"
    ++ showev isRootUnmounted "RootUnmounted"

    ++ showev isAttrsModified "AttrsModified"

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
