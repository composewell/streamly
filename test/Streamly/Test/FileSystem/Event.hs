-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Just report all events under the paths provided as arguments

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Streamly.Internal.Memory.Array (Array)
import System.Environment (getArgs)
import Streamly (SerialT)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#else
#error "FS Events not supported on this platform
#endif
import qualified Streamly.Internal.Memory.Array as Array

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8 . Stream.fromList

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event.Event

#if defined(CABAL_OS_DARWIN)
watchPaths = Event.watchTrees
#elif defined(CABAL_OS_LINUX)
watchPaths = Event.watchPaths
#else
#error "Unsupported platform
#endif

main :: IO ()
main = do
    args <- getArgs
    paths <- mapM toUtf8 args

    watchPaths (NonEmpty.fromList paths)
        & Stream.mapM_ (putStrLn . Event.showEvent)
