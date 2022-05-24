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
      unfold
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

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.Type as Stream

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream


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
unfold unf = Stream.fromStreamD . D.unfold unf
