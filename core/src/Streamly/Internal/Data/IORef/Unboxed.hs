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
import Streamly.Internal.Data.Unboxed
    ( MutableByteArray(..)
    , Unbox
    , sizeOf
    , peekWith
    , pokeWith
    , newUnpinnedBytes
    )

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

-- | An 'IORef' holds a single 'Unbox'-able value.
newtype IORef a = IORef (MutableByteArray a)

-- | Create a new 'IORef'.
--
-- /Pre-release/
{-# INLINE newIORef #-}
newIORef :: forall a. Unbox a => a -> IO (IORef a)
newIORef x = do
    var <- newUnpinnedBytes (sizeOf (undefined :: a))
    pokeWith var 0 x
    return $ IORef var

-- | Write a value to an 'IORef'.
--
-- /Pre-release/
{-# INLINE writeIORef #-}
writeIORef :: Unbox a => IORef a -> a -> IO ()
writeIORef (IORef var) = pokeWith var 0

-- | Read a value from an 'IORef'.
--
-- /Pre-release/
{-# INLINE readIORef #-}
readIORef :: Unbox a => IORef a -> IO a
readIORef (IORef var) = peekWith var 0

-- | Modify the value of an 'IORef' using a function with strict application.
--
-- /Pre-release/
{-# INLINE modifyIORef' #-}
modifyIORef' :: Unbox a => IORef a -> (a -> a) -> IO ()
modifyIORef' var g = do
  x <- readIORef var
  writeIORef var (g x)

-- | Generate a stream by continuously reading the IORef.
--
-- /Pre-release/
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: (MonadIO m, Unbox a) => IORef a -> D.Stream m a
toStreamD var = D.Stream step ()

    where

    {-# INLINE_LATE step #-}
    step _ () = liftIO (readIORef var) >>= \x -> return $ D.Yield x ()
