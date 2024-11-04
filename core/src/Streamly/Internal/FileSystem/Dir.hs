#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.Dir
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.Dir
{-# DEPRECATED "Please use \"Streamly.Internal.FileSystem.DirIO\" instead." #-}
    (
    -- * Streams
      read

    -- read not just the names but also the inode attrs of the children. This
    -- abstraction makes sense because when we read the dir contents we also
    -- get the inodes, and it is cheaper to get the attrs from the inodes
    -- instead of resolving the paths and get those. This abstraction may be
    -- less portable as different platforms may have different attrs. To
    -- optimize, we can also add a filter/pattern/parser on the names of the
    -- children that we want to read. We can call that readAttrsWith? Or just
    -- have the default readAttrs do that? Usually we won't need that, so it
    -- may be better to keep that a separate API.
    -- , readAttrs

    -- recursive read requires us to read the attributes of the children to
    -- determine if something is a dirctory or not. Therefore, it may be a good
    -- idea to have a low level routine that also spits out the attributes of
    -- the files, we get that for free. We can also add a filter/pattern/parser
    -- on the names of the children that we want to read.
    --, readAttrsRecursive -- Options: acyclic, follow symlinks
    , readFiles
    , readDirs
    , readEither
    , readEitherPaths

    -- We can implement this in terms of readAttrsRecursive without losing
    -- perf.
    -- , readEitherRecursive -- Options: acyclic, follow symlinks
    -- , readAncestors -- read the parent chain using the .. entry.
    -- , readAncestorsAttrs

    -- * Unfolds
    -- | Use the more convenient stream APIs instead of unfolds where possible.
    , reader
    , fileReader
    , dirReader
    , eitherReader
    , eitherReaderPaths

      {-
    , toStreamWithBufferOf

    , readChunks
    , readChunksWithBufferOf

    , toChunksWithBufferOf
    , toChunks

    , write
    , writeWithBufferOf

    -- Byte stream write (Streams)
    , fromStream
    , fromStreamWithBufferOf

    -- -- * Array Write
    , writeArray
    , writeChunks
    , writeChunksWithBufferOf

    -- -- * Array stream Write
    , fromChunks
    , fromChunksWithBufferOf
    -}
    -- * Deprecated
    , toStream
    , toEither
    , toFiles
    , toDirs
    )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (bimap)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import System.FilePath ((</>))
import qualified Streamly.Data.Stream as S

import Streamly.Internal.FileSystem.Path (Path)

import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.DirIO as DirIO
import qualified Streamly.Internal.Data.Unfold as Unfold

import Prelude hiding (read)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINE ePathMap #-}
ePathMap :: Either Path Path -> Either FilePath FilePath
ePathMap (Left a) = Left (Path.toString a)
ePathMap (Right a) = Right (Path.toString a)

{-# INLINE pMapUnfold #-}
pMapUnfold :: MonadCatch m => Unfold m Path Path -> Unfold m FilePath FilePath
pMapUnfold = fmap Path.toString . Unfold.lmapM Path.fromString

{-# INLINE pMapUnfoldE #-}
pMapUnfoldE
    :: MonadCatch m
    => Unfold m Path (Either Path Path)
    -> Unfold m FilePath (Either FilePath FilePath)
pMapUnfoldE = fmap ePathMap . Unfold.lmapM Path.fromString

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

--  | Read a directory emitting a stream with names of the children. Filter out
--  "." and ".." entries.
--
--  /Internal/
--
{-# INLINE reader #-}
reader :: (MonadIO m, MonadCatch m) => Unfold m FilePath FilePath
reader = fmap Path.toString $ Unfold.lmapM Path.fromString DirIO.reader

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries.
--
--  /Internal/
--
{-# INLINE eitherReader #-}
eitherReader :: (MonadIO m, MonadCatch m) => Unfold m FilePath (Either FilePath FilePath)
eitherReader = pMapUnfoldE DirIO.eitherReader


{-# INLINE eitherReaderPaths #-}
eitherReaderPaths ::(MonadIO m, MonadCatch m) => Unfold m FilePath (Either FilePath FilePath)
eitherReaderPaths = pMapUnfoldE DirIO.eitherReaderPaths

--
-- | Read files only.
--
--  /Internal/
--
{-# INLINE fileReader #-}
fileReader :: (MonadIO m, MonadCatch m) => Unfold m FilePath FilePath
fileReader = pMapUnfold DirIO.fileReader

-- | Read directories only. Filter out "." and ".." entries.
--
--  /Internal/
--
{-# INLINE dirReader #-}
dirReader :: (MonadIO m, MonadCatch m) => Unfold m FilePath FilePath
dirReader = pMapUnfold DirIO.dirReader

-- | Raw read of a directory.
--
-- /Pre-release/
{-# INLINE read #-}
read :: (MonadIO m, MonadCatch m) => FilePath -> Stream m FilePath
read = S.unfold reader

{-# DEPRECATED toStream "Please use 'read' instead" #-}
{-# INLINE toStream #-}
toStream :: (MonadIO m, MonadCatch m) => String -> Stream m String
toStream = read

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries. The output contains the names of the directories and files.
--
-- /Pre-release/
{-# INLINE readEither #-}
readEither :: (MonadIO m, MonadCatch m) => FilePath -> Stream m (Either FilePath FilePath)
readEither = S.unfold eitherReader

-- | Like 'readEither' but prefix the names of the files and directories with
-- the supplied directory path.
{-# INLINE readEitherPaths #-}
readEitherPaths :: (MonadIO m, MonadCatch m) => FilePath -> Stream m (Either FilePath FilePath)
readEitherPaths dir = fmap (bimap (dir </>) (dir </>)) $ readEither dir

{-# DEPRECATED toEither "Please use 'readEither' instead" #-}
{-# INLINE toEither #-}
toEither :: (MonadIO m, MonadCatch m) => FilePath -> Stream m (Either FilePath FilePath)
toEither = readEither

-- | Read files only.
--
--  /Internal/
--
{-# INLINE readFiles #-}
readFiles :: (MonadIO m, MonadCatch m) => FilePath -> Stream m FilePath
readFiles = S.unfold fileReader

{-# DEPRECATED toFiles "Please use 'readFiles' instead" #-}
{-# INLINE toFiles #-}
toFiles :: (MonadIO m, MonadCatch m) => FilePath -> Stream m FilePath
toFiles = readFiles

-- | Read directories only.
--
--  /Internal/
--
{-# INLINE readDirs #-}
readDirs :: (MonadIO m, MonadCatch m) => FilePath -> Stream m FilePath
readDirs = S.unfold dirReader

{-# DEPRECATED toDirs "Please use 'readDirs' instead" #-}
{-# INLINE toDirs #-}
toDirs :: (MonadIO m, MonadCatch m) => String -> Stream m String
toDirs = readDirs
