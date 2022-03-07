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

import Control.Monad.Primitive (PrimMonad(PrimState))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef, IORef)
import Data.Primitive.Array (newArray, writeArray, MutableArray)

data Ring a = Ring
    { arr :: MutableArray (PrimState IO) a
    , ringHead :: IORef Int -- current index to be over-written
    , ringMax :: !Int       -- first index beyond allocated memory
    }

{-# INLINE createRing #-}
createRing :: Int -> IO (Ring a)
createRing count = do
    arr' <- newArray count (undefined :: a)
    head' <- newIORef 0
    return (Ring
        { arr = arr'
        , ringHead = head'
        , ringMax = count
        })

{-# INLINE unsafeInsertRing #-}
unsafeInsertRing :: Ring a -> Int -> a -> IO ()
unsafeInsertRing Ring{..} idx x = do
    writeArray arr (mod idx ringMax) x
    ref <- readIORef ringHead
    if (ref+1) < ringMax
    then modifyIORef' ringHead ( + 1)
    else writeIORef ringHead 0
