-- |
-- Module      : Streamly.Internal.Data.Ring
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Ring
    ( Ring(..)
    , createRing
    , unsafeInsertRing
    ) where

import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Streamly.Internal.Data.Array.Generic.Mut.Type
    ( MutArray(..)
    , new
    , uninit
    , putIndexUnsafe
    )

data Ring a = Ring
    { ringArr :: MutArray a
    -- XXX This could be an unboxed IO ref
    -- If we want to rely on an external Int count using mod ringMax as index
    -- into the ring then we do not need to maintain this, assuming the counter
    -- would not overflow.
    , ringHead :: IORef Int -- current index to be over-written
    , ringMax :: !Int       -- first index beyond allocated memory
    }

-- XXX If we align the ringMax to nearest power of two then computation of the
-- index to write could be cheaper.
{-# INLINE createRing #-}
createRing :: Int -> IO (Ring a)
createRing count = do
    arr <- new count
    arr1 <- uninit arr count
    ref <- newIORef 0
    return (Ring
        { ringArr = arr1
        , ringHead = ref
        , ringMax = count
        })

-- XXX This is safe
{-# INLINE unsafeInsertRing #-}
unsafeInsertRing :: Ring a -> a -> IO ()
unsafeInsertRing Ring{..} x = do
    when (ringMax > 0) $ do
        idx <- readIORef ringHead
        putIndexUnsafe idx ringArr x
        let idx1 = idx + 1
            next = if idx1 == ringMax then 0 else idx1
        writeIORef ringHead next
