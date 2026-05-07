-- |
-- Module      : Streamly.Internal.Syscall.Windows.Common
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.Syscall.Windows.Common
    ( asCWString
    )
where

import Foreign.C (CWString)
import Foreign.Ptr (castPtr)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath)
import qualified Streamly.Internal.FileSystem.WindowsPath as Path

asCWString :: WindowsPath -> (CWString -> IO a) -> IO a
asCWString p act = Path.asW16CString p $ \ptr -> act (castPtr ptr)
