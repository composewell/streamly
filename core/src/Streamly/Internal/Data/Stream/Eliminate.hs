-- |
-- Module      : Streamly.Internal.Data.Stream.Eliminate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Eliminate
    (
     fold
    )
where

import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Type as StreamD

-- $setup
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.fold Fold.sum (Stream.unfold Unfold.enumerateFromTo (1,100))
-- 5050
--
-- Folds never fail, therefore, they produce a default value even when no input
-- is provided. It means we can always fold an empty stream and get a valid
-- result.  For example:
--
-- >> Stream.fold Fold.sum Stream.nil
-- >0
--
-- However, 'foldMany' on an empty stream results in an empty stream.
-- Therefore, @Stream.fold f@ is not the same as @Stream.head . Stream.foldMany
-- f@.
--
-- @fold f = Stream.parse (Parser.fromFold f)@
--
-- /Pre-release/
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Stream m a -> m b
fold fld = StreamD.fold fld . Stream.toStreamD
