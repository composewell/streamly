-- |
-- Module      : Stream.Common
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Stream.Common
    ( sourceUnfoldr
    , sourceUnfoldrM
    , sourceUnfoldrAction
    , benchIOSink
    , benchIOSrc
    )
where

import Streamly.Internal.Data.Stream (Stream, fromStreamD)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified  Streamly.Internal.Data.Stream.StreamD as D
import Control.DeepSeq (NFData)
import Gauge
import Prelude hiding (mapM)
import System.Random (randomRIO)

{-# INLINE toNull #-}
toNull :: Monad m => Stream m a -> m ()
toNull = Stream.fold Fold.drain

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM count start = fromStreamD $ D.unfoldrM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldr count start = fromStreamD $ D.unfoldr step start
    where
    step cnt =
        if cnt > start + count
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (Monad m1, Monad m) => Int -> Int -> Stream m (m1 Int)
sourceUnfoldrAction value n = fromStreamD $ D.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (return cnt, cnt + 1)

{-# INLINE benchIOSink #-}
benchIOSink
    :: (NFData b)
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-- | Takes a source, and uses it with a default drain/fold method.
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: String
    -> (Int -> Stream IO a)
    -> Benchmark
benchIOSrc name f =
    bench name $ nfIO $ randomRIO (1,1) >>= toNull . f