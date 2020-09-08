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
{-# LANGUAGE CPP #-}
module Main (main) where


import Data.Function ((&))

import System.Environment (getArgs)
import Streamly.Prelude (SerialT)

#if !defined(CABAL_OS_WINDOWS)   
import Control.Monad.IO.Class (MonadIO)    
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Storable.Foreign (Array)
#endif


import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
#if !defined(CABAL_OS_WINDOWS)
toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8 . Stream.fromList
#endif
-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------
#if defined(CABAL_OS_WINDOWS)
watchPaths :: [FilePath] -> SerialT IO Event.Event
watchPaths = Event.watchTrees
#elif defined(CABAL_OS_DARWIN)
watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event.Event    
watchPaths = Event.watchTrees
#elif defined(CABAL_OS_LINUX)
watchPaths :: NonEmpty (Array Word8) -> SerialT IO Event.Event    
watchPaths = Event.watchPaths
#endif



#if defined(CABAL_OS_WINDOWS)
main :: IO ()
main = do
    args <- getArgs   
    watchPaths args 
        & Stream.mapM_ (putStrLn . Event.showEvent)

#else    
main :: IO ()
main = do
    args <- getArgs
    paths <- mapM toUtf8 args
    watchPaths (NonEmpty.fromList paths)
        & Stream.mapM_ (putStrLn . Event.showEvent)
#endif