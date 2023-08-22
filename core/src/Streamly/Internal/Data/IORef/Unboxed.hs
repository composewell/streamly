-- |
-- Module      : Streamly.Internal.Data.IORef.Unboxed
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A mutable variable in a mutation capable monad (IO) holding a 'Unboxed'
-- value. This allows fast modification because of unboxed storage.
--
-- = Multithread Consistency Notes
--
-- In general, any value that straddles a machine word cannot be guaranteed to
-- be consistently read from another thread without a lock.  GHC heap objects
-- are always machine word aligned, therefore, a 'IORef' is also word aligned.
-- On a 64-bit platform, writing a 64-bit aligned type from one thread and
-- reading it from another thread should give consistent old or new value. The
-- same holds true for 32-bit values on a 32-bit platform.

module Streamly.Internal.Data.IORef.Unboxed
    (
      IORef

    -- * Construction
    , newIORef
    , newIORefWithIndex

    -- * Write
    , writeIORef
    , writeIORefWithIndex
    , modifyIORef'
    , modifyIORefWithIndex'

    -- * Read
    , readIORef
    , readIORefWithIndex
    , toStreamD
    , toStreamDWithIndex
    )
where

#include "inline.hs"

import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Unbox
    ( MutableByteArray(..)
    , Unbox(..)
    , newBytes
    , sizeOf
    )

import qualified Streamly.Internal.Data.Stream.Type as D

-- | An 'IORef' holds a single 'Unbox'-able value with offset.
data IORef a = IORef !MutableByteArray !Int

-- | Create a new 'IORef' at index 'i'.
--
-- /Pre-release/
{-# INLINE newIORefWithIndex #-}
newIORefWithIndex :: forall a. Unbox a => Int -> a -> IO (IORef a)
newIORefWithIndex i x = do
    var <- newBytes (sizeOf (Proxy :: Proxy a))
    pokeByteIndex i var x
    return $ IORef var i

-- | Create a new 'IORef' starting at index '0'.
--
-- /Pre-release/
{-# INLINE newIORef #-}
newIORef :: forall a. Unbox a => a -> IO (IORef a)
newIORef = newIORefWithIndex 0

-- | Write a value to an 'IORef' at index 'i'.
--
-- /Pre-release/
{-# INLINE writeIORefWithIndex #-}
writeIORefWithIndex :: Unbox a => Int -> IORef a -> a -> IO ()
writeIORefWithIndex i (IORef var _) = pokeByteIndex i var

-- | Write a value to an 'IORef'.
--
-- /Pre-release/
{-# INLINE writeIORef #-}
writeIORef :: Unbox a => IORef a -> a -> IO ()
writeIORef arr@(IORef _ o) = writeIORefWithIndex o arr

-- | Read a value from an 'IORef' at index 'i'.
--
-- /Pre-release/
{-# INLINE readIORefWithIndex #-}
readIORefWithIndex :: Unbox a => Int -> IORef a -> IO a
readIORefWithIndex i (IORef var _) = peekByteIndex i var

-- | Read a value from an 'IORef'.
--
-- /Pre-release/
{-# INLINE readIORef #-}
readIORef :: Unbox a => IORef a -> IO a
readIORef arr@(IORef _ o) = readIORefWithIndex o arr

-- | Modify the value of an 'IORef' at index 'i' using a function with strict application.
--
-- /Pre-release/
{-# INLINE modifyIORefWithIndex' #-}
modifyIORefWithIndex' :: Unbox a => Int -> IORef a -> (a -> a) -> IO ()
modifyIORefWithIndex' i var g = do
  x <- readIORefWithIndex i var
  writeIORefWithIndex i var (g x)

-- | Modify the value of an 'IORef' using a function with strict application.
--
-- /Pre-release/
{-# INLINE modifyIORef' #-}
modifyIORef' :: Unbox a => IORef a -> (a -> a) -> IO ()
modifyIORef' arr@(IORef _ o) = modifyIORefWithIndex' o arr

-- | Generate a stream by continuously reading the IORef at index 'i'.
--
-- /Pre-release/
{-# INLINE_NORMAL toStreamDWithIndex #-}
toStreamDWithIndex :: (MonadIO m, Unbox a) => Int -> IORef a -> D.Stream m a
toStreamDWithIndex i var = D.Stream step ()

    where

    {-# INLINE_LATE step #-}
    step _ () = liftIO (readIORefWithIndex i var) >>= \x -> return $ D.Yield x ()

-- | Generate a stream by continuously reading the IORef.
--
-- /Pre-release/
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: (MonadIO m, Unbox a) => IORef a -> D.Stream m a
toStreamD var = D.Stream step ()

    where

    {-# INLINE_LATE step #-}
    step _ () = liftIO (readIORef var) >>= \x -> return $ D.Yield x ()
