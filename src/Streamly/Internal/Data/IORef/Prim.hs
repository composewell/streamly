{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.IORef.Prim
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A mutable variable in a mutation capable monad (IO) holding a 'Prim'
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

module Streamly.Internal.Data.IORef.Prim
    (
      IORef
    , Prim

    -- * Construction
    , newIORef

    -- * Write
    , writeIORef
    , modifyIORef'

    -- * Read
    , readIORef
    , toStreamD
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Primitive (primitive_)
import Data.Primitive.Types (Prim, sizeOf#, readByteArray#, writeByteArray#)
import GHC.Exts (MutableByteArray#, newByteArray#, RealWorld)
import GHC.IO (IO(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

-- | An 'IORef' holds a single 'Prim' value.
data IORef a = IORef (MutableByteArray# RealWorld)

-- | Create a new 'IORef'.
--
-- /Pre-release/
{-# INLINE newIORef #-}
newIORef :: forall a. Prim a => a -> IO (IORef a)
newIORef x = IO (\s# ->
      case newByteArray# (sizeOf# (undefined :: a)) s# of
        (# s1#, arr# #) ->
            case writeByteArray# arr# 0# x s1# of
                s2# -> (# s2#, IORef arr# #)
    )

-- | Write a value to an 'IORef'.
--
-- /Pre-release/
{-# INLINE writeIORef #-}
writeIORef :: Prim a => IORef a -> a -> IO ()
writeIORef (IORef arr#) x = primitive_ (writeByteArray# arr# 0# x)

-- | Read a value from an 'IORef'.
--
-- /Pre-release/
{-# INLINE readIORef #-}
readIORef :: Prim a => IORef a -> IO a
readIORef (IORef arr#) = IO (readByteArray# arr# 0#)

-- | Modify the value of an 'IORef' using a function with strict application.
--
-- /Pre-release/
{-# INLINE modifyIORef' #-}
modifyIORef' :: Prim a => IORef a -> (a -> a) -> IO ()
modifyIORef' (IORef arr#) g = primitive_ $ \s# ->
  case readByteArray# arr# 0# s# of
    (# s'#, a #) -> let a' = g a in a' `seq` writeByteArray# arr# 0# a' s'#

-- | Generate a stream by continuously reading the IORef.
--
-- /Pre-release/
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: (MonadIO m, Prim a) => IORef a -> D.Stream m a
toStreamD var = D.Stream step ()

    where

    {-# INLINE_LATE step #-}
    step _ () = liftIO (readIORef var) >>= \x -> return $ D.Yield x ()
