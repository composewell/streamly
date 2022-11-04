-- |
-- Module      : Streamly.Internal.Data.Pipe
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- There are three fundamental types in streamly. They are streams
-- ("Streamly.Data.Stream"), pipes ("Streamly.Internal.Data.Pipe") and folds ("Streamly.Data.Fold").
-- Streams are sources or producers of values, multiple sources can be merged
-- into a single source but a source cannot be split into multiple stream
-- sources.  Folds are sinks or consumers, a stream can be split and
-- distributed to multiple folds but the results cannot be merged back into a
-- stream source again. Pipes are transformations, a stream source can be split
-- and distributed to multiple pipes each pipe can apply its own transform on
-- the stream and the results can be merged back into a single pipe. Pipes can
-- be attached to a source to produce a source or they can be attached to a
-- fold to produce a fold, or multiple pipes can be merged or zipped into a
-- single pipe.
--
-- > import qualified Streamly.Internal.Data.Pipe as Pipe

module Streamly.Internal.Data.Pipe
    (
    -- * Pipe Type
      Pipe

    -- * Pipes
    -- ** Mapping
    , map
    , mapM

    {-
    -- ** Filtering
    , lfilter
    , lfilterM
    -- , ldeleteBy
    -- , luniq

    {-
    -- ** Mapping Filters
    , lmapMaybe
    , lmapMaybeM

    -- ** Scanning Filters
    , lfindIndices
    , lelemIndices

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , linsertBy
    , lintersperseM

    -- ** Reordering
    , lreverse
    -}

    -- * Parsing
    -- ** Trimming
    , ltake
    -- , lrunFor -- time
    , ltakeWhile
    {-
    , ltakeWhileM
    , ldrop
    , ldropWhile
    , ldropWhileM
    -}

    -- ** Splitting
    -- | Streams can be split into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- *** By Chunks
    , chunksOf
    , sessionsOf

    -- *** By Elements
    , splitBy
    , splitSuffixBy
    , splitSuffixBy'
    -- , splitPrefixBy
    , wordsBy

    -- *** By Sequences
    , splitOn
    , splitSuffixOn
    -- , splitPrefixOn
    -- , wordsOn

    -- Keeping the delimiters
    , splitOn'
    , splitSuffixOn'
    -- , splitPrefixOn'

    -- Splitting by multiple sequences
    -- , splitOnAny
    -- , splitSuffixOnAny
    -- , splitPrefixOnAny

    -- ** Grouping
    , groups
    , groupsBy
    , groupsRollingBy
    -}

    -- * Composing Pipes
    , tee
    , zipWith
    , compose

    {-
    -- * Distributing
    -- |
    -- The 'Applicative' instance of a distributing 'Fold' distributes one copy
    -- of the stream to each fold and combines the results using a function.
    --
    -- @
    --
    --                 |-------Fold m a b--------|
    -- ---stream m a---|                         |---m (b,c,...)
    --                 |-------Fold m a c--------|
    --                 |                         |
    --                            ...
    -- @
    --
    -- To compute the average of numbers in a stream without going through the
    -- stream twice:
    --
    -- >>> let avg = (/) <$> FL.sum <*> fmap fromIntegral FL.length
    -- >>> FL.foldl' avg (S.enumerateFromTo 1.0 100.0)
    -- 50.5
    --
    -- The 'Semigroup' and 'Monoid' instances of a distributing fold distribute
    -- the input to both the folds and combines the outputs using Monoid or
    -- Semigroup instances of the output types:
    --
    -- >>> import Data.Monoid (Sum)
    -- >>> FL.foldl' (FL.head <> FL.last) (fmap Sum $ S.enumerateFromTo 1.0 100.0)
    -- Just (Sum {getSum = 101.0})
    --
    -- The 'Num', 'Floating', and 'Fractional' instances work in the same way.

    , tee
    , distribute

    -- * Partitioning
    -- |
    -- Direct items in the input stream to different folds using a function to
    -- select the fold. This is useful to demultiplex the input stream.
    -- , partitionByM
    -- , partitionBy
    , partition

    -- * Demultiplexing
    , demux
    -- , demuxWith
    , demux_
    -- , demuxWith_

    -- * Classifying
    , classify
    -- , classifyWith

    -- * Unzipping
    , unzip
    -- These can be expressed using lmap/lmapM and unzip
    -- , unzipWith
    -- , unzipWithM

    -- * Nested Folds
    -- , concatMap
    -- , chunksOf
    , duplicate  -- experimental

    -- * Windowed Classification
    -- | Split the stream into windows or chunks in space or time. Each window
    -- can be associated with a key, all events associated with a particular
    -- key in the window can be folded to a single result. The stream is split
    -- into windows of specified size, the window can be terminated early if
    -- the closing flag is specified in the input stream.
    --
    -- The term "chunk" is used for a space window and the term "session" is
    -- used for a time window.

    -- ** Tumbling Windows
    -- | A new window starts after the previous window is finished.
    -- , classifyChunksOf
    , classifySessionsOf

    -- ** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.
    -- , classifyKeepAliveChunks
    , classifyKeepAliveSessions

    {-
    -- ** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -}
    -- ** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer
-}
    )
where

-- import Control.Concurrent (threadDelay, forkIO, killThread)
-- import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
-- import Control.Exception (SomeException(..), catch, mask)
-- import Control.Monad (void)
-- import Control.Monad.Catch (throwM)
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Trans (lift)
-- import Control.Monad.Trans.Control (control)
-- import Data.Functor.Identity (Identity)
-- import Data.Heap (Entry(..))
-- import Data.Map.Strict (Map)
-- import Data.Maybe (fromJust, isJust, isNothing)

-- import Foreign.Storable (Storable(..))
import Prelude
       hiding (id, filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip,
               span, splitAt, break, mapM)

-- import qualified Data.Heap as H
-- import qualified Data.Map.Strict as Map
-- import qualified Prelude

-- import Streamly.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Pipe.Type
       (Pipe(..), PipeState(..), Step(..), zipWith, tee, map, compose)
-- import Streamly.Internal.Data.Array.Unboxed.Type (Array)
-- import Streamly.Internal.Data.Ring.Unboxed (Ring)
-- import Streamly.Internal.Data.Stream (Stream)
-- import Streamly.Internal.Data.Time.Units
-- (AbsTime, MilliSecond64(..), addToAbsTime, diffAbsTime, toRelTime,
-- toAbsTime)

-- import Streamly.Internal.Data.Strict

-- import qualified Streamly.Internal.Data.Array.Unboxed.Type as A
-- import qualified Streamly.Data.Stream as S
-- import qualified Streamly.Internal.Data.Stream.StreamD as D
-- import qualified Streamly.Internal.Data.Stream.StreamK as K
-- import qualified Streamly.Internal.Data.Stream.Common as P

------------------------------------------------------------------------------
-- Pipes
------------------------------------------------------------------------------

-- | Lift a monadic function to a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Pipe m a b
mapM f = Pipe consume undefined ()
    where
    consume _ a = do
        r <- f a
        return $ Yield r (Consume ())
