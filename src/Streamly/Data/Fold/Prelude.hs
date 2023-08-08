-- |
-- Module      : Streamly.Data.Fold.Prelude
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
module Streamly.Data.Fold.Prelude
    ( module Streamly.Data.Fold
    , toHashMapIO
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Streamly.Data.Fold
import Streamly.Internal.Data.Fold (toContainerIO)
import Streamly.Internal.Data.IsMap.HashMap ()

-- | Split the input stream based on a hashable component of the key field and
-- fold each split using the given fold. Useful for map/reduce, bucketizing
-- the input in different bins or for generating histograms.
--
-- Example:
--
-- >>> import Data.HashMap.Strict (HashMap, fromList)
-- >>> import qualified Streamly.Data.Fold.Prelude as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import Streamly.Data.Fold.Prelude (toHashMapIO)
-- >>> :{
--  do
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--      classify = toHashMapIO fst (Fold.lmap snd Fold.toList)
--  x <- Stream.fold classify input :: IO (HashMap String [Double])
--  let y = fromList [("ONE",[1.0,1.1]),("TWO",[2.0,2.2])]
--  return (x == y)
-- :}
-- True
--
-- /Pre-release/
--
{-# INLINE toHashMapIO #-}
toHashMapIO :: (MonadIO m, Hashable k, Ord k) =>
    (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
toHashMapIO = toContainerIO
