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

import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef, IORef)
import Streamly.Internal.Data.Array.Mut.Type
    ( Array(..)
    , newArray
    , putIndexUnsafe
    )

data Ring a = Ring
    { arr :: Array a
    , ringHead :: IORef Int -- current index to be over-written
    , ringMax :: !Int       -- first index beyond allocated memory
    }

{-# INLINE createRing #-}
createRing :: Int -> IO (Ring a)
createRing count = do
    arr' <- newArray count
    head' <- newIORef 0
    return (Ring
        { arr = arr'
        , ringHead = head'
        , ringMax = count
        })

{-# INLINE unsafeInsertRing #-}
unsafeInsertRing :: Ring a -> Int -> a -> IO ()
unsafeInsertRing Ring{..} idx x = do
    putIndexUnsafe arr (mod idx ringMax) x
    ref <- readIORef ringHead
    if (ref+1) < ringMax
    then modifyIORef' ringHead ( + 1)
    else writeIORef ringHead 0
