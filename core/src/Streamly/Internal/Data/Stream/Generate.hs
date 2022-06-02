-- |
-- Module      : Streamly.Internal.Data.Stream.Generate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Generate
    (
      cons
    , consM
    , unfold
    , append
    )
where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Type as StreamD
import qualified Streamly.Internal.Data.Stream.StreamK.Type as StreamK

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

infixr 5 `cons`

-- | Construct a stream by adding a pure value at the head of an existing
-- stream. Same as the following but more efficient:
--
-- For example:
--
-- >> s = 1 `Stream.cons` 2 `Stream.cons` 3 `Stream.cons` Stream.nil
-- >> Stream.fold Fold.toList s
-- >[1,2,3]
--
-- >>> cons x xs = return x `Stream.consM` xs
--
-- /Pre-release/
--
{-# INLINE cons #-}
cons :: a -> Stream m a -> Stream m a
cons x = Stream.fromStreamK . StreamK.cons x . Stream.toStreamK

infixr 5 `consM`

-- | Constructs a stream by adding a monadic action at the head of an
-- existing stream. For example:
--
-- >> s = putChar 'h' `Stream.consM` putChar 'i' `Stream.consM` Stream.nil
-- >> Stream.fold Fold.toList s
-- >hi
--
-- >> consM x xs = Stream.fromEffect x `Stream.append` xs
--
-- /Pre-release/
--
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m = Stream.fromStreamK . StreamK.consM m . Stream.toStreamK

------------------------------------------------------------------------------
-- From Unfold
------------------------------------------------------------------------------

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> s = Stream.unfold (Unfold.replicateM 3) (putStrLn "hello")
-- >>> Stream.fold Fold.drain s
-- hello
-- hello
-- hello
--
-- /Pre-release/
{-# INLINE unfold #-}
unfold :: Monad m => Unfold m a b -> a -> Stream m b
unfold unf = Stream.fromStreamD . StreamD.unfold unf

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

infixr 6 `append`

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> s1 = Stream.unfold Unfold.fromList [1,2]
-- >>> s2 = Stream.unfold Unfold.fromList [3,4]
-- >>> Stream.fold Fold.toList $ s1 `Stream.append` s2
-- [1,2,3,4]
--
-- This operation can be used to fold an infinite lazy container of streams.
--
-- /Pre-release/
--
{-# INLINE append #-}
append :: Stream m a -> Stream m a -> Stream m a
append = (<>)
