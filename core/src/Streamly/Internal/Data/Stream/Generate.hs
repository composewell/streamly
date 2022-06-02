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

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamK.Type as StreamK

-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.IORef
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
--

-- | Construct a stream by adding a pure value at the head of an existing
-- stream. This is the same as @(return a) \`consM` r@ but
-- more efficient. For concurrent streams this is not concurrent whereas
-- 'consM' is concurrent. For example:
--
-- @
-- > toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- /Pre-release/
--
{-# INLINE cons #-}
cons :: a -> Stream m a -> Stream m a
cons x = Stream.fromStreamK . StreamK.cons x . Stream.toStreamK

-- | Constructs a stream by adding a monadic action at the head of an
-- existing stream. For example:
--
-- @
-- > toList $ getLine \`consM` getLine \`consM` nil
-- hello
-- world
-- ["hello","world"]
-- @
--
-- > consM x xs = fromEffect x `append` xs
--
-- /Pre-release/
--
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m = Stream.fromStreamK . StreamK.consM m . Stream.toStreamK

------------------------------------------------------------------------------
-- Combining
------------------------------------------------------------------------------

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> import Streamly.Internal.Data.Stream (append)
-- >>> stream1 = Stream.fromList [1,2]
-- >>> stream2 = Stream.fromList [3,4]
-- >>> Stream.toList $ stream1 `append` stream2
-- [1,2,3,4]
--
-- This operation can be used to fold an infinite lazy container of streams.
--
-- /Pre-release/
--
{-# INLINE append #-}
append :: Stream m a -> Stream m a -> Stream m a
append = (<>)
