-- Some code snippets are adapted from the fsnotify package.
-- http://hackage.haskell.org/package/fsnotify-0.3.0.1/
--
-- |
-- Module      : Streamly.Internal.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
--               (c) 2012, Mark Dittmer
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- =Overview
--
-- Use 'watchRecursive'or 'watch' with a list of file system dir paths you
-- want to watch as argument. It returns a stream of 'Event' representing the
-- file system events occurring under the watched paths.
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
-- For Windows reference documentation see:
--
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information file notify information>
-- * <https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw read directory changes>
--
-- We try to keep the macOS\/Linux/Windows event handling APIs and defaults
-- semantically and syntactically as close as possible.
--
-- =Availability
--
-- As per the Windows reference docs, the fs event notification API is
-- available in:
--
-- * Minimum supported client: Windows XP [desktop apps | UWP apps]
-- * Minimum supported server: Windows Server 2003 [desktop apps | UWP apps

module Streamly.Internal.FileSystem.Event.Windows
    (
    -- * Subscribing to events

    -- ** Default configuration
      Config
    , defaultConfig

    -- ** Watch Behavior
    , setRecursiveMode

    -- ** Events of Interest
    , setFileNameEvents
    , setDirNameEvents
    , setAttrsModified
    , setSecurityModified
    , setSizeModified
    , setLastWriteTimeModified
    , setAllEvents

    -- ** Watch APIs
    , watch
    , watchRecursive
    , watchWith

    -- * Handling Events
    , Event(..)
    , getRoot
    , getRelPath
    , getAbsPath

    -- ** Item CRUD events
    , isCreated
    , isDeleted
    , isModified
    , isMoved
    , isMovedFrom
    , isMovedTo

    -- ** Exception Conditions
    , isEventsLost

    -- * Debugging
    , showEvent
    )
where

import Data.Bits ((.|.), (.&.), complement)
import Data.Char (ord)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Foreign.C.String (peekCWStringLen)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peekByteOff)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr, nullFunPtr, plusPtr)
import System.Win32.File
    ( FileNotificationFlag
    , LPOVERLAPPED
    , closeHandle
    , createFile
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_LIST_DIRECTORY
    , fILE_NOTIFY_CHANGE_FILE_NAME
    , fILE_NOTIFY_CHANGE_DIR_NAME
    , fILE_NOTIFY_CHANGE_ATTRIBUTES
    , fILE_NOTIFY_CHANGE_SIZE
    , fILE_NOTIFY_CHANGE_LAST_WRITE
    , fILE_NOTIFY_CHANGE_SECURITY
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , oPEN_EXISTING
    )
import System.Win32.Types (BOOL, DWORD, HANDLE, LPVOID, LPDWORD, failIfFalse_)

import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude (eager)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Array as A (fromList)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Unicode.Stream as U
import qualified Streamly.Internal.Unicode.Utf8 as UTF8 (pack, toArray)
import qualified Streamly.Internal.Data.Array as A (read)

-- | Watch configuration, used to specify the events of interest and the
-- behavior of the watch.
--
-- /Pre-release/
--
data Config = Config
    { watchRec :: BOOL
    , createFlags :: DWORD
    }

-------------------------------------------------------------------------------
-- Boolean settings
-------------------------------------------------------------------------------

setFlag :: DWORD -> Bool -> Config -> Config
setFlag mask status cfg@Config{..} =
    let flags =
            if status
            then createFlags .|. mask
            else createFlags .&. complement mask
    in cfg {createFlags = flags}

-- | Set watch event on directory recursively.
--
-- /default: False/
--
-- /Pre-release/
--
setRecursiveMode :: Bool -> Config -> Config
setRecursiveMode recursive cfg@Config{} = cfg {watchRec = recursive}

-- | Generate notify events on file create, rename or delete.
--
-- From Windows API documentation: Any file name change in the watched
-- directory or subtree causes a change notification wait operation to return.
-- Changes include renaming, creating, or deleting a file.
--
-- /default: True/
--
-- /Pre-release/
--
setFileNameEvents :: Bool -> Config -> Config
setFileNameEvents = setFlag fILE_NOTIFY_CHANGE_FILE_NAME

-- | Generate notify events on directory create, rename or delete.
--
-- From Windows API documentaiton: Any directory-name change in the watched
-- directory or subtree causes a change notification wait operation to return.
-- Changes include creating or deleting a directory.
--
-- /default: True/
--
-- /Pre-release/
--
setDirNameEvents :: Bool -> Config -> Config
setDirNameEvents = setFlag fILE_NOTIFY_CHANGE_DIR_NAME

-- | Generate an 'isModified' event on any attribute change in the watched
-- directory or subtree.
--
-- /default: False/
--
-- /Pre-release/
--
setAttrsModified :: Bool -> Config -> Config
setAttrsModified = setFlag fILE_NOTIFY_CHANGE_ATTRIBUTES

-- | Generate an 'isModified' event when the file size is changed.
--
-- From Windows API documentation: Any file-size change in the watched
-- directory or subtree causes a change notification wait operation to return.
-- The operating system detects a change in file size only when the file is
-- written to the disk. For operating systems that use extensive caching,
-- detection occurs only when the cache is sufficiently flushed.
--
-- /default: False/
--
-- /Pre-release/
--
setSizeModified :: Bool -> Config -> Config
setSizeModified = setFlag fILE_NOTIFY_CHANGE_SIZE

-- | Generate an 'isModified' event when the last write timestamp of the file
-- inode is changed.
--
-- From Windows API documentation: Any change to the last write-time of files
-- in the watched directory or subtree causes a change notification wait
-- operation to return. The operating system detects a change to the last
-- write-time only when the file is written to the disk. For operating systems
-- that use extensive caching, detection occurs only when the cache is
-- sufficiently flushed.
--
-- /default: False/
--
-- /Pre-release/
--
setLastWriteTimeModified :: Bool -> Config -> Config
setLastWriteTimeModified = setFlag fILE_NOTIFY_CHANGE_LAST_WRITE

-- | Generate an 'isModified' event when any security-descriptor change occurs
-- in the watched directory or subtree.
--
-- /default: False/
--
-- /Pre-release/
--
setSecurityModified :: Bool -> Config -> Config
setSecurityModified = setFlag fILE_NOTIFY_CHANGE_SECURITY

-- | Set all tunable events 'True' or 'False'. Equivalent to setting:
--
-- * setFileNameEvents
-- * setDirNameEvents
-- * setAttrsModified
-- * setSizeModified
-- * setLastWriteTimeModified
-- * setSecurityModified
--
-- /Pre-release/
--
setAllEvents :: Bool -> Config -> Config
setAllEvents s =
      setFileNameEvents s
    . setDirNameEvents s
    . setAttrsModified s
    . setSizeModified s
    . setLastWriteTimeModified s
    . setSecurityModified s

-- | The tunable events that are enabled by default are:
--
-- * setFileNameEvents True
-- * setDirNameEvents True
-- * setSizeModified True
-- * setLastWriteTimeModified True
--
-- /Pre-release/
--
defaultConfig :: Config
defaultConfig =
      setFileNameEvents True
    $ setDirNameEvents True
    $ setSizeModified True
    $ setLastWriteTimeModified True
    $ Config {watchRec = False, createFlags = 0}

getConfigFlag :: Config -> DWORD
getConfigFlag Config{..} = createFlags

getConfigRecMode :: Config -> BOOL
getConfigRecMode Config{..} = watchRec

data Event = Event
    { eventFlags :: DWORD
    , eventRelPath :: String
    , eventRootPath :: String
    , totalBytes :: DWORD
    } deriving (Show, Ord, Eq)

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-file_notify_information
data FILE_NOTIFY_INFORMATION = FILE_NOTIFY_INFORMATION
    { fniNextEntryOffset :: DWORD
    , fniAction :: DWORD
    , fniFileName :: String
    } deriving Show

type LPOVERLAPPED_COMPLETION_ROUTINE =
    FunPtr ((DWORD, DWORD, LPOVERLAPPED) -> IO ())

-- | A handle for a watch.
getWatchHandle :: FilePath -> IO (HANDLE, FilePath)
getWatchHandle dir = do
    h <- createFile dir
        -- Access mode
        fILE_LIST_DIRECTORY
        -- Share mode
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
        -- Security attributes
        Nothing
        -- Create mode, we want to look at an existing directory
        oPEN_EXISTING
        -- File attribute, NOT using OVERLAPPED since we work synchronously
        fILE_FLAG_BACKUP_SEMANTICS
        -- No template file
        Nothing
    return (h, dir)

-- For reference documentation see:
--
-- See https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-readdirectorychangesw
-- Note that this API uses UTF-16 for file system paths:
-- 1. https://docs.microsoft.com/en-us/windows/win32/intl/unicode-in-the-windows-api
-- 2. https://docs.microsoft.com/en-us/windows/win32/intl/unicode
foreign import ccall safe
    "windows.h ReadDirectoryChangesW" c_ReadDirectoryChangesW ::
           HANDLE
        -> LPVOID
        -> DWORD
        -> BOOL
        -> DWORD
        -> LPDWORD
        -> LPOVERLAPPED
        -> LPOVERLAPPED_COMPLETION_ROUTINE
        -> IO BOOL

readDirectoryChangesW ::
       HANDLE
    -> Ptr FILE_NOTIFY_INFORMATION
    -> DWORD
    -> BOOL
    -> FileNotificationFlag
    -> LPDWORD
    -> IO ()
readDirectoryChangesW h buf bufSize wst f br =
    let res = c_ReadDirectoryChangesW
                    h (castPtr buf) bufSize wst f br nullPtr nullFunPtr
     in failIfFalse_ "ReadDirectoryChangesW" res

peekFNI :: Ptr FILE_NOTIFY_INFORMATION -> IO FILE_NOTIFY_INFORMATION
peekFNI buf = do
    neof <- peekByteOff buf 0
    acti <- peekByteOff buf 4
    fnle <- peekByteOff buf 8
    -- Note: The path is UTF-16 encoded C WChars, peekCWStringLen converts
    -- UTF-16 to UTF-32 Char String
    fnam <- peekCWStringLen
        -- start of array
        (buf `plusPtr` 12,
        -- fnle is the length in *bytes*, and a WCHAR is 2 bytes
        fromEnum (fnle :: DWORD) `div` 2)
    return $ FILE_NOTIFY_INFORMATION neof acti fnam

readChangeEvents ::
    Ptr FILE_NOTIFY_INFORMATION -> String -> DWORD -> IO [Event]
readChangeEvents pfni root bytesRet = do
    fni <- peekFNI pfni
    let entry = Event
            { eventFlags = fniAction fni
            , eventRelPath = fniFileName fni
            , eventRootPath = root
            , totalBytes = bytesRet
            }
        nioff = fromEnum $ fniNextEntryOffset fni
    entries <-
        if nioff == 0
        then return []
        else readChangeEvents (pfni `plusPtr` nioff) root bytesRet
    return $ entry : entries

readDirectoryChanges ::
    String -> HANDLE -> Bool -> FileNotificationFlag -> IO [Event]
readDirectoryChanges root h wst mask = do
    let maxBuf = 63 * 1024
    allocaBytes maxBuf $ \buffer -> do
        alloca $ \bret -> do
            readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
            bytesRet <- peekByteOff bret 0
            readChangeEvents buffer root bytesRet

-- XXX Try to get these from windows header files
type FileAction = DWORD

fILE_ACTION_ADDED             :: FileAction
fILE_ACTION_ADDED             =  1

fILE_ACTION_REMOVED           :: FileAction
fILE_ACTION_REMOVED           =  2

fILE_ACTION_MODIFIED          :: FileAction
fILE_ACTION_MODIFIED          =  3

fILE_ACTION_RENAMED_OLD_NAME  :: FileAction
fILE_ACTION_RENAMED_OLD_NAME  =  4

fILE_ACTION_RENAMED_NEW_NAME  :: FileAction
fILE_ACTION_RENAMED_NEW_NAME  =  5

eventStreamAggr :: (HANDLE, FilePath, Config) -> Stream IO Event
eventStreamAggr (handle, rootPath, cfg) =  do
    let recMode = getConfigRecMode cfg
        flagMasks = getConfigFlag cfg
        repeatM = S.sequence . S.repeat
    S.concatMap S.fromList $ repeatM
        $ readDirectoryChanges rootPath handle recMode flagMasks

pathsToHandles ::
    NonEmpty FilePath -> Config -> Stream IO (HANDLE, FilePath, Config)
pathsToHandles paths cfg = do
    let pathStream = S.fromList (NonEmpty.toList paths)
        st2 = S.mapM getWatchHandle pathStream
    fmap (\(h, f) -> (h, f, cfg)) st2

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

utf8ToString :: Array Word8 -> FilePath
utf8ToString = runIdentity . S.fold Fold.toList . U.decodeUtf8 . A.read

utf8ToStringList :: NonEmpty (Array Word8) -> NonEmpty FilePath
utf8ToStringList = NonEmpty.map utf8ToString

-- | Close a Directory handle.
--
closePathHandleStream :: Stream IO (HANDLE, FilePath, Config) -> IO ()
closePathHandleStream =
    let f (h, _, _) = closeHandle h
        in S.fold (Fold.drainMapM f)

-- XXX
-- Document the path treatment for Linux/Windows/macOS modules.
-- Remove the utf-8 encoding requirement of paths? It can be encoding agnostic
-- "\" separated bytes, the application can decide what encoding to use.
-- Instead of always using widechar (-W) APIs can we call the underlying APIs
-- based on the configured code page?
-- https://docs.microsoft.com/en-us/windows/uwp/design/globalizing/use-utf8-code-page
--
-- | Start monitoring a list of directory paths for file system events with the
-- supplied configuration modifier operation over the' defaultConfig'. The
-- paths could be directories or symbolic links to directories.
--
-- When recursive mode is True, the whole directory tree under the path is
-- watched recursively.  When recursive mode is False, only the files and
-- directories directly under the watched directory are monitored, contents of
-- subdirectories are not monitored.  Monitoring starts from the current time
-- onwards. The paths are specified as UTF-8 encoded 'Array' of 'Word8'.
--
-- /Symbolic Links:/ If the pathname to be watched is a symbolic link then
-- watch the target of the symbolic link instead of the symbolic link itself.
-- Note that the path location in the events is through the original symbolic
-- link path rather than the resolved path.
--
-- @
-- watchWith
--  ('setAttrsModified' True . 'setLastWriteTimeModified' False)
--  [Array.fromList "dir"]
-- @
--
-- /Pre-release/
--
watchWith :: (Config -> Config) -> NonEmpty (Array Word8) -> Stream IO Event
watchWith f paths =
    S.bracketIO before after (S.parConcatMap (eager True) eventStreamAggr)

    where

    before = return $ pathsToHandles (utf8ToStringList paths) $ f defaultConfig
    after = closePathHandleStream

-- | Same as 'watchWith' using 'defaultConfig' and recursive mode.
--
-- >>> watchRecursive = watchWith (setRecursiveMode True)
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

getFlag :: DWORD -> Event -> Bool
getFlag mask Event{..} = eventFlags == mask

-- XXX Change the type to Array Word8 to make it compatible with other APIs.
--
-- | Get the file system object path for which the event is generated, relative
-- to the watched root. The path is a UTF-8 encoded array of bytes.
--
-- /Pre-release/
--
getRelPath :: Event -> Array Word8
getRelPath Event{..} = (UTF8.toArray . UTF8.pack) eventRelPath

-- XXX Change the type to Array Word8 to make it compatible with other APIs.
--
-- | Get the watch root directory to which this event belongs.
--
-- /Pre-release/
--
getRoot :: Event -> Array Word8
getRoot Event{..} = (UTF8.toArray . UTF8.pack) eventRootPath

-- | Get the absolute file system object path for which the event is generated.
--
-- When the watch root is a symlink, the absolute path returned is via the
-- original symlink and not through the resolved path.
--
-- /Pre-release/
--
getAbsPath :: Event -> Array Word8
getAbsPath ev = getRoot ev <> backSlash <> getRelPath ev
    where backSlash = A.fromList [ fromIntegral (ord '\\') ]

-- XXX need to document the exact semantics of these.
--
-- | This event is generated when a file or directory is created in a watched
-- directory or directory tree when in recursive watch mode.  Creating a hard
-- link also generates this event.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
--
-- /Pre-release/
--
isCreated :: Event -> Bool
isCreated = getFlag fILE_ACTION_ADDED

-- | This event is generated when a file or directory is deleted from the
-- watched directory or directory tree when in recursive mode. This event is
-- generated even when a hard link is deleted.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
--
-- /Pre-release/
--
isDeleted :: Event -> Bool
isDeleted = getFlag fILE_ACTION_REMOVED

-- | Generated for the original path when an object is moved from under a
-- monitored directory.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
--
-- /Pre-release/
--
isMovedFrom :: Event -> Bool
isMovedFrom = getFlag fILE_ACTION_RENAMED_OLD_NAME

-- | Generated for the new path when an object is moved under a monitored
-- directory.
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
--
-- /Pre-release/
--
isMovedTo :: Event -> Bool
isMovedTo = getFlag fILE_ACTION_RENAMED_NEW_NAME

-- | Generated for a path that is moved from or moved to the monitored
-- directory.
--
-- >>> isMoved ev = isMovedFrom ev || isMovedTo ev
--
-- /Occurs when either 'setFileNameEvents' or 'setDirNameEvents' is enabled/
-- /Occurs only for an object inside the watched directory/
--
-- /Pre-release/
--
isMoved :: Event -> Bool
isMoved ev = isMovedFrom ev || isMovedTo ev

-- XXX This event is generated only for files and not directories?
--
-- | This event occurs when a file or directory contents, timestamps or
-- attributes are modified.  Since it can occur on multiple changes, you may
-- have to check the attributes to know what exactly changed when multiple type
-- of modified events are enabled.
--
-- In non-recursive mode this event does not occur for directories.  In
-- recursive mode this event occurs for the parent directory if a file or
-- directory inside it is created or renamed.
--
-- /Occurs when one of the @set*Modified@ events is enabled/
--
-- /Pre-release/
--
isModified :: Event -> Bool
isModified = getFlag fILE_ACTION_MODIFIED

-- | If the kernel event buffer overflows, entire contents of the buffer are
-- discarded, therefore, events are lost.  The user application must scan
-- everything under the watched paths to know the current state of the file
-- system tree.
--
-- /Pre-release/
--
isEventsLost :: Event -> Bool
isEventsLost Event{..} = totalBytes == 0

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- | Convert an 'Event' record to a String representation.
showEvent :: Event -> String
showEvent ev@Event{..} =
        "--------------------------"
    ++ "\nRoot = " ++ utf8ToString (getRoot ev)
    ++ "\nPath = " ++ utf8ToString (getRelPath ev)
    ++ "\ngetAbsPath = " ++ utf8ToString (getAbsPath ev)
    ++ "\nFlags " ++ show eventFlags
    ++ showev isEventsLost "Overflow"
    ++ showev isCreated "Created"
    ++ showev isDeleted "Deleted"
    ++ showev isModified "Modified"
    ++ showev isMovedFrom "MovedFrom"
    ++ showev isMovedTo "MovedTo"
    ++ "\n"

    where showev f str = if f ev then "\n" ++ str else ""
