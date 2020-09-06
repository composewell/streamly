{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Streamly.Internal.FileSystem.Event.Windows(watchTrees, Event, showEvent) 
    where

import System.Win32.File
import System.Win32.Types
import Foreign
import Foreign.C
import Streamly (SerialT, parallel)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array(..))

import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Stream.IsStream as S

type Handle = HANDLE

watchTrees :: [FilePath] -> SerialT IO Event
watchTrees paths =  do
    let sth = strPathToStrHandle paths
    S.after (closePathHandleStream sth) $ S.concatMapWith parallel eventStreamAggr (sth)
   
eventStreamAggr :: (Handle, FilePath) -> SerialT IO Event
eventStreamAggr (handle, rootPath) =  S.concatMap S.fromList  
      $ S.mapM (S.parse (readOneEvent rootPath). A.toStream) $ S.repeatM 
      $ readDirectoryChanges handle True defaultFlags

strPathToStrHandle :: [FilePath] -> SerialT IO (Handle, FilePath)
strPathToStrHandle paths = do
  let pathStream = S.fromList paths
  S.mapM getWatchHandle pathStream

defaultFlags :: DWORD
defaultFlags = fILE_NOTIFY_CHANGE_FILE_NAME .|. fILE_NOTIFY_CHANGE_DIR_NAME .|. fILE_NOTIFY_CHANGE_ATTRIBUTES
               .|. fILE_NOTIFY_CHANGE_SIZE  .|. fILE_NOTIFY_CHANGE_LAST_WRITE .|. fILE_NOTIFY_CHANGE_SECURITY 

getWatchHandle :: FilePath -> IO (Handle, FilePath)
getWatchHandle dir = do
        h <- createFile dir
            fILE_LIST_DIRECTORY -- Access mode
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

closePathHandleStream :: SerialT IO (Handle, FilePath) -> IO ()
closePathHandleStream sth = S.mapM_ ( \(h, _) -> closeHandle h) sth
                      

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
  neof <- peekByteOff buf 0
  acti <- peekByteOff buf 4
  fnle <- peekByteOff buf 8
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

readOneEvent :: String -> Parser IO Word8 [Event]
readOneEvent root = do
  let headerLen = 8*1024
  arr <- PR.take headerLen (A.writeN headerLen)  
  fnis <- PR.yieldM $ A.asPtr arr $ \buf -> readChangeEvents buf root
  return $ map maptoEvent fnis  

maptoEvent ::  (FILE_NOTIFY_INFORMATION, String) -> Event
maptoEvent fni =   Event{eventFlags = fniAction $ fst fni, 
                         eventRelPath = fniFileName $ fst  fni,  
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
   { eventFlags :: Word32  
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