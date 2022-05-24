-- |
-- Module      : Streamly.Internal.Data.Stream.Reduce
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reduce streams by streams, folds or parsers.

module Streamly.Internal.Data.Stream.Reduce
    (

    -- * Reduce By Folds
    -- |
    -- Reduce a stream by folding . Functions
    -- generally ending in these shapes:
    --
    -- @
    -- f (Fold m a b) -> Stream m a -> Stream m b
    -- f (Parser m a b) -> Stream m a -> Stream m b
    -- @

    -- ** Generic Folding
    -- | Apply folds on a stream.
      foldMany
    , foldManyPost
    , refoldMany
    , foldSequence
    , foldIterateM
    , refoldIterateM
    )
where

import Streamly.Internal.Data.Fold.Type (Fold (..))
import Streamly.Internal.Data.Refold.Type (Refold (..))
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (concatMap, map)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Array.Foreign as Array

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- Splitting operations that take a predicate and a Fold can be
-- expressed using parseMany. Operations like chunksOf, intervalsOf, split*,
-- can be expressed using parseMany when used with an appropriate Parser.
--
-- XXX We need takeGE/takeBetween to implement "some" using "many".

-- | Like 'foldMany' but appends empty fold output if the fold and stream
-- termination aligns:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.fold Fold.toList $ Stream.foldManyPost f $ Stream.unfold Unfold.fromList []
-- [0]
-- >>> Stream.fold Fold.toList $ Stream.foldManyPost f $ Stream.unfold Unfold.fromList [1..9]
-- [3,7,11,15,9]
-- >>> Stream.fold Fold.toList $ Stream.foldManyPost f $ Stream.unfold Unfold.fromList [1..10]
-- [3,7,11,15,19,0]
--
-- /Pre-release/
--
{-# INLINE foldManyPost #-}
foldManyPost
    :: (Monad m)
    => Fold m a b
    -> Stream m a
    -> Stream m b
foldManyPost f m = fromStreamD $ D.foldManyPost f (toStreamD m)

-- | Apply a 'Fold' repeatedly on a stream and emit the fold outputs in the
-- output stream.
--
-- To sum every two contiguous elements in a stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.fold Fold.toList $ Stream.foldMany f $ Stream.unfold Unfold.fromList [1..10]
-- [3,7,11,15,19]
--
-- On an empty stream the output is empty:
--
-- >>> Stream.fold Fold.toList $ Stream.foldMany f $ Stream.unfold Unfold.fromList []
-- []
--
-- Note @Stream.foldMany (Fold.take 0)@ would result in an infinite loop in a
-- non-empty stream.
--
-- @since 0.8.0
--
{-# INLINE foldMany #-}
foldMany
    :: (Monad m)
    => Fold m a b
    -> Stream m a
    -> Stream m b
foldMany f m = fromStreamD $ D.foldMany f (toStreamD m)

-- | Like 'foldMany' but using the 'Refold' type instead of 'Fold'.
--
-- /Pre-release/
{-# INLINE refoldMany #-}
refoldMany :: (Monad m) =>
    Refold m c a b -> m c -> Stream m a -> Stream m b
refoldMany f action = fromStreamD . D.refoldMany f action . toStreamD

-- | Apply a stream of folds to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE foldSequence #-}
foldSequence
       :: -- (Monad m) =>
       Stream m (Fold m a b)
    -> Stream m a
    -> Stream m b
foldSequence _f _m = undefined

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- @
-- >>> import Data.Monoid (Sum(..))
-- >>> f x = return (Fold.take 2 (Fold.sconcat x))
-- >>> s = Stream.map Sum $ Stream.unfold Unfold.fromList [1..10]
-- >>> Stream.fold Fold.toList $ Stream.map getSum $ Stream.foldIterateM f (pure 0) s
-- [3,10,21,36,55,55]
--
-- @
--
-- This is the streaming equivalent of monad like sequenced application of
-- folds where next fold is dependent on the previous fold.
--
-- /Pre-release/
--
{-# INLINE foldIterateM #-}
foldIterateM ::
       (Monad m) => (b -> m (Fold m a b)) -> m b -> Stream m a -> Stream m b
foldIterateM f i m = fromStreamD $ D.foldIterateM f i (toStreamD m)

-- | Like 'foldIterateM' but using the 'Refold' type instead. This could be
-- much more efficient due to stream fusion.
--
-- /Internal/
{-# INLINE refoldIterateM #-}
refoldIterateM :: (Monad m) =>
    Refold m b a b -> m b -> Stream m a -> Stream m b
refoldIterateM c i m = fromStreamD $ D.refoldIterateM c i (toStreamD m)
