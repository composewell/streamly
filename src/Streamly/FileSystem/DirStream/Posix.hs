{-# LANGUAGE CPP        #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module      :  Streamly.FileSystem.DirStream.Posix
-- Copyright   :  © 2020 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  streamly@composewell.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides working with directory streams (POSIX).
module Streamly.FileSystem.DirStream.Posix
  (
  -- * Directory listing
    unfoldDirContents
  )
where

#include "inline.hs"

import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Data.Word8
import           Prelude                 hiding ( readFile )
import           Streamly.Internal.Data.Unfold.Types
#if !defined(mingw32_HOST_OS)
import           System.Posix.ByteString
import           System.Posix.Foreign           ( DirType )
import           System.Posix.RawFilePath.Directory.Traversals
                                         hiding ( getDirectoryContents )
#endif
import qualified Data.ByteString               as BS
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D


#if !defined(mingw32_HOST_OS)
-- | Create an 'Unfold' of directory contents.
unfoldDirContents :: MonadIO m => Unfold m DirStream (DirType, RawFilePath)
unfoldDirContents = Unfold step return
 where
  {-# INLINE_LATE step #-}
  step dirstream = do
    (typ, e) <- liftIO $ readDirEnt dirstream
    return if
      | BS.null e                       -> D.Stop
      | BS.pack [_period] == e          -> D.Skip dirstream
      | BS.pack [_period, _period] == e -> D.Skip dirstream
      | otherwise                       -> D.Yield (typ, e) dirstream
#endif
