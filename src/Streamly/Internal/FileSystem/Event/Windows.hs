{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Streamly.Internal.FileSystem.Event.Windows(watchTrees, Event, showEvent) 
    where

import System.Win32.File
import System.Win32.Types
import Foreign
import Foreign.C
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.|.), (.&.), complement)
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Lazy (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Streamly (SerialT, parallel)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Memory.Array.Types (Array(..))
import Data.IORef

import qualified Data.IntMap.Lazy as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Unicode.Stream as U
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.Memory.Array as A
import qualified System.IO as SY
import qualified Streamly.Internal.Data.Stream.IsStream as S

type Handle = HANDLE

watchTrees :: [FilePath] -> SerialT IO Event
watchTrees paths = 
    S.concatMapWith parallel (\(h, r) -> S.concatMap S.fromList $ 
      S.mapM (S.parse (readOneEvent r). A.toStream) $ S.repeatM $ 
      readDirectoryChanges h True 383) (strPathToStrHandle paths)
   

strPathToStrHandle :: [FilePath] -> SerialT IO (Handle, FilePath)
strPathToStrHandle paths = do
  let pathStream = S.fromList paths
  S.mapM getWatchHandle pathStream

getWatchHandle :: FilePath -> IO (Handle, FilePath)
getWatchHandle dir = do
        h <- createFile dir
            System.Win32.File.fILE_LIST_DIRECTORY -- Access mode
            (fILE_SHARE_READ .|. fILE_SHARE_WRITE) -- Share mode
            Nothing -- security attributes
            oPEN_EXISTING -- Create mode, we want to look at an existing directory
            fILE_FLAG_BACKUP_SEMANTICS -- File attribute, nb NOT using OVERLAPPED since we work synchronously
            Nothing -- No template file
        return (h, dir)


readDirectoryChanges :: Handle -> Bool -> FileNotificationFlag -> IO (Array Word8)
readDirectoryChanges h wst mask = do
  let maxBuf = 8*1024
  allocaBytes maxBuf $ \buffer -> do
    alloca $ \bret -> do
       readDirectoryChangesW h buffer (toEnum maxBuf) wst mask bret
       A.fromStream $ A.toStream $ A.fromPtr maxBuf buffer    


-------------------------------------------------------------------
-- Low-level stuff that binds to notifications in the Win32 API

-- Defined in System.Win32.File, but with too few cases:
-- type AccessMode = UINT


fILE_LIST_DIRECTORY  :: AccessMode
fILE_LIST_DIRECTORY  =  1


-- there are many more cases but I only need this one.


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

type WCHAR = Word16
-- This is a bit overkill for now, I'll only use nullFunPtr anyway,
-- but who knows, maybe someday I'll want asynchronous callbacks on the OS level.
type LPOVERLAPPED_COMPLETION_ROUTINE = FunPtr ((DWORD, DWORD, LPOVERLAPPED) -> IO ())

data FILE_NOTIFY_INFORMATION = FILE_NOTIFY_INFORMATION
    { fniNextEntryOffset, fniAction :: DWORD
    , fniFileName :: String
    } deriving Show

-- instance Storable FILE_NOTIFY_INFORMATION where
-- ... well, we can't write an instance since the struct is not of fix size,
-- so we'll have to do it the hard way, and not get anything for free. Sigh.

-- sizeOfFNI :: FILE_NOTIFY_INFORMATION -> Int
-- sizeOfFNI fni =  (#size FILE_NOTIFY_INFORMATION) + (#size WCHAR) * (length (fniFileName fni) - 1)

peekFNI :: Ptr FILE_NOTIFY_INFORMATION -> IO FILE_NOTIFY_INFORMATION
peekFNI buf = do
  neof <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) buf
  acti <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) buf
  fnle <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) buf
  fnam <- peekCWStringLen
            (buf `plusPtr` ((12)), -- start of array
            fromEnum (fnle :: DWORD) `div` 2 ) -- fnle is the length in *bytes*, and a WCHAR is 2 bytes
  return $ FILE_NOTIFY_INFORMATION neof acti fnam

readDirectoryChangesW :: Handle -> Ptr Word8 -> DWORD -> BOOL -> FileNotificationFlag -> LPDWORD -> IO ()
readDirectoryChangesW h buf bufSize wst f br =
  failIfFalse_ "ReadDirectoryChangesW" $ c_ReadDirectoryChangesW h (castPtr buf) bufSize wst f br nullPtr nullFunPtr  

-- The interruptible qualifier will keep threads listening for events from hanging blocking when killed

foreign import ccall safe "windows.h ReadDirectoryChangesW"
  c_ReadDirectoryChangesW :: Handle -> LPVOID -> DWORD -> BOOL -> DWORD -> LPDWORD -> LPOVERLAPPED -> LPOVERLAPPED_COMPLETION_ROUTINE -> IO BOOL


-- See https://msdn.microsoft.com/en-us/library/windows/desktop/aa365465(v=vs.85).aspx
fILE_NOTIFY_CHANGE_FILE_NAME  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_FILE_NAME  =  1
fILE_NOTIFY_CHANGE_DIR_NAME  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_DIR_NAME  =  2
fILE_NOTIFY_CHANGE_ATTRIBUTES  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_ATTRIBUTES  =  4
fILE_NOTIFY_CHANGE_SIZE  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_SIZE  =  8
fILE_NOTIFY_CHANGE_LAST_WRITE  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_LAST_WRITE  =  16
fILE_NOTIFY_CHANGE_LAST_ACCESS  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_LAST_ACCESS  =  32
fILE_NOTIFY_CHANGE_CREATION  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_CREATION  =  64
fILE_NOTIFY_CHANGE_SECURITY  :: FileNotificationFlag
fILE_NOTIFY_CHANGE_SECURITY  =  256

readOneEvent :: String -> Parser IO Word8 [Event]
readOneEvent root = do
  let headerLen = 8*1024
  arr <- PR.take headerLen (A.writeN headerLen)  
  fnis <- PR.yieldM $ A.asPtr arr $ \buf -> readChangeEvents buf root
  return $ map maptoEvent fnis  

maptoEvent ::  (FILE_NOTIFY_INFORMATION, String) -> Event
maptoEvent fni =   Event{eventFlags = fniAction $ fst fni  , 
                         eventRelPath = fniFileName $ fst  fni ,                     
                         eventWd = 0 ,
                         eventRootPath = snd fni
                         }                                            

readChangeEvents :: Ptr FILE_NOTIFY_INFORMATION -> String -> IO [(FILE_NOTIFY_INFORMATION, String)]
readChangeEvents pfni root = do  
  fni <- peekFNI pfni
  let entry = (fni, root)
      nioff = fromEnum $ fniNextEntryOffset fni   
  entries <- if nioff == 0 then return [] else readChangeEvents  (pfni `plusPtr` nioff)  root
  return $ entry:entries

data Event = Event
   { eventWd :: CInt
   , eventFlags :: Word32  
   , eventRelPath :: String         
   , eventRootPath :: String 
   } deriving (Show, Ord, Eq)   

getFlag :: Word32 -> Event -> Bool
getFlag mask Event{..} = eventFlags == mask

isCreated :: Event -> Bool
isCreated = getFlag fILE_ACTION_ADDED

isDeleted :: Event -> Bool
isDeleted = getFlag fILE_ACTION_REMOVED

isMovedFrom :: Event -> Bool
isMovedFrom = getFlag fILE_ACTION_RENAMED_OLD_NAME

isMovedTo :: Event -> Bool
isMovedTo = getFlag fILE_ACTION_RENAMED_NEW_NAME

isModified :: Event -> Bool
isModified = getFlag fILE_ACTION_MODIFIED


showEvent :: Event -> String
showEvent ev@Event{..} =  
     "--------------------------"
        ++ "\nRoot = " ++ show eventRootPath
        ++ "\nPath = " ++ show eventRelPath
        ++ "\nFlags " ++ show eventFlags
        ++ showev isCreated "Created"
        ++ showev isDeleted "Deleted"
        ++ showev isModified "Modified"
        ++ showev isMovedFrom "MovedFrom"
        ++ showev isMovedTo "MovedTo"
        ++ "\n"

        where showev f str = if f ev then "\n" ++ str else ""