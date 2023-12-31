-- |
-- Module      : Streamly.Internal.Data.MutArray.Stream
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of mutable arrays.
--
-- We can either push these in the MutArray module with a "chunks" prefix or
-- keep this as a separate module and release it.
--
module Streamly.Internal.Data.MutArray.Stream
{-# DEPRECATED "Please use \"Streamly.Internal.Data.MutArray\" instead." #-}
    (
    -- * Generation
      MArray.chunksOf
    , MArray.pinnedChunksOf
    , MArray.writeChunks -- chunksWrite?
    , MArray.splitOn -- chunksSplitOn

    -- * Compaction
    , packArraysChunksOf
    , MArray.SpliceState (..)
    , lpackArraysChunksOf
    , compact -- chunksCompact
    , compactLE
    , compactEQ
    , compactGE

    -- * Elimination
    , MArray.flattenArrays -- chunksConcat
    , MArray.flattenArraysRev -- chunksConcatRev
    , MArray.fromArrayStreamK -- chunksCoalesce
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser (ParseError)
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser as ParserD
import qualified Streamly.Internal.Data.Stream as D

-------------------------------------------------------------------------------
-- Compact
-------------------------------------------------------------------------------

-- XXX This can be removed once compactLEFold/compactLE are implemented.
--
-- | This mutates the first array (if it has space) to append values from the
-- second one. This would work for immutable arrays as well because an
-- immutable array never has space so a new array is allocated instead of
-- mutating it.
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- @since 0.7.0
{-# INLINE packArraysChunksOf #-}
packArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> D.Stream m (MutArray a) -> D.Stream m (MutArray a)
packArraysChunksOf = MArray.compactChunksLE

-- XXX Remove this once compactLEFold is implemented
-- lpackArraysChunksOf = Fold.many compactLEFold
--
{-# INLINE lpackArraysChunksOf #-}
lpackArraysChunksOf :: (MonadIO m, Unbox a)
    => Int -> Fold m (MutArray a) () -> Fold m (MutArray a) ()
lpackArraysChunksOf = MArray.lCompactChunksGE

-- XXX Same as compactLE, to be removed once that is implemented.
--
-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- /Internal/
{-# INLINE compact #-}
compact :: (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compact = packArraysChunksOf

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size. Note that if a single array is bigger than the
-- specified size we do not split it to fit. When we coalesce multiple arrays
-- if the size would exceed the specified size we do not coalesce therefore the
-- actual array size may be less than the specified chunk size.
--
-- /Internal/
{-# INLINE compactLEParserD #-}
compactLEParserD ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> ParserD.Parser (MutArray a) m (MutArray a)
compactLEParserD = MArray.pCompactChunksLE

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- minimum specified size. Note that if all the arrays in the stream together
-- are smaller than the specified size the resulting array will be smaller than
-- the specified size. When we coalesce multiple arrays if the size would exceed
-- the specified size we stop coalescing further.
--
-- /Internal/
{-# INLINE compactGEFold #-}
compactGEFold ::
       forall m a. (MonadIO m, Unbox a)
    => Int -> FL.Fold m (MutArray a) (MutArray a)
compactGEFold = MArray.fCompactChunksGE

-- | Coalesce adjacent arrays in incoming stream to form bigger arrays of a
-- maximum specified size in bytes.
--
-- /Internal/
compactLE :: (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (Either ParseError (MutArray a))
compactLE n = D.parseManyD (compactLEParserD n)

-- | Like 'compactLE' but generates arrays of exactly equal to the size
-- specified except for the last array in the stream which could be shorter.
--
-- /Unimplemented/
{-# INLINE compactEQ #-}
compactEQ :: -- (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactEQ _n _xs = undefined
    -- IsStream.fromStreamD $ D.foldMany (compactEQFold n) (IsStream.toStreamD xs)

-- | Like 'compactLE' but generates arrays of size greater than or equal to the
-- specified except for the last array in the stream which could be shorter.
--
-- /Internal/
{-# INLINE compactGE #-}
compactGE ::
       (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactGE n = D.foldMany (compactGEFold n)
