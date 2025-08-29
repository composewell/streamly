-- |
-- Module      : Streamly.FileSystem.DirIO
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- High performance and streaming APIs for reading directories.
--
-- File system paths are specified using the 'Streamly.FileSystem.Path.Path'
-- type. If you want to convert between 'String' or 'FilePath' and 'Path' use
-- 'Streamly.FileSystem.Path.fromString_', 'Streamly.FileSystem.Path.toString'
-- from the "Streamly.FileSystem.Path" module..
--
-- >>> import qualified Streamly.FileSystem.DirIO as Dir
--

module Streamly.FileSystem.DirIO
    (
    -- * Configuration
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    -- | Only the default ReadOptions are supported for Windows. Please use "id"
    -- as the configuration modifier.
      ReadOptions
#else
      ReadOptions
    , followSymlinks
    , ignoreMissing
    , ignoreSymlinkLoops
    , ignoreInaccessible
#endif
    -- * Streams
    , read
    , readEither
    )
where

import Streamly.Internal.FileSystem.DirIO
import Prelude hiding (read)
