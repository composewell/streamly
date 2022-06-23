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
    )
where

import Streamly.Internal.Data.Stream.Type (Stream, fromStreamK)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import Control.DeepSeq (NFData)
import Gauge
import Prelude hiding (mapM)
import System.Random (randomRIO)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (Monad m) => Int -> Int -> Stream m Int
sourceUnfoldrM count start = fromStreamK $ K.unfoldrMWith K.consM step start
    where
    step cnt =
        if cnt > start + count
        then return Nothing
        else return (Just (cnt, cnt + 1))


{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Int -> Stream m Int
sourceUnfoldr count start = fromStreamK $ K.unfoldr step start
    where
    step cnt =
        if cnt > start + count
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrAction #-}
sourceUnfoldrAction :: (Monad m, Monad m1)
    => Int -> Int -> Stream m (m1 Int)
sourceUnfoldrAction value n = fromStreamK $ K.unfoldr step n
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
