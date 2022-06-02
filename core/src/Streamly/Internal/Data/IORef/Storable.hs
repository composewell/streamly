{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.IORef.Storable
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A mutable variable in a mutation capable monad (IO) holding a 'Storable'
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

module Streamly.Internal.Data.IORef.Storable
    (
      IORef
    , Storable

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

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable, peekByteOff, pokeByteOff, sizeOf)
import GHC.Exts ( MutableByteArray#, Ptr(..), RealWorld
                , byteArrayContents#, newPinnedByteArray#, touch#
                , unsafeCoerce#
                )
import GHC.Int (Int(..))
import GHC.IO (IO(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

-- XXX data IORef a = IORef (Ptr a) ?
-- | An 'IORef' holds a single 'Storable' value.
data IORef a = IORef (MutableByteArray# RealWorld)

instance NFData (IORef a) where
    rnf (IORef _) = ()

{-# INLINE toPtr #-}
toPtr :: IORef a -> Ptr a
toPtr (IORef arr#) = Ptr (byteArrayContents# (unsafeCoerce# arr#))

{-# INLINE touch #-}
touch :: IORef a -> IO ()
touch (IORef arr#) =
    IO $ \s -> case touch# arr# s of s1 -> (# s1, () #)

{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: IORef a -> (Ptr a -> IO b) -> IO b
asPtrUnsafe ref f = do
    res <- f (toPtr ref)
    touch ref
    return res

-- | Create a new 'IORef'.
--
-- /Pre-release/
{-# INLINE newIORef #-}
newIORef :: forall a. Storable a => a -> IO (IORef a)
newIORef x = do
    ref <-
        IO
            (\s# ->
                 let !(I# i#) = sizeOf (undefined :: a)
                  in case newPinnedByteArray# i# s# of
                         (# s1#, arr# #) -> (# s1#, IORef arr# #))
    asPtrUnsafe ref $ \ptr -> pokeByteOff ptr 0 x >> return ref

-- | Write a value to an 'IORef'.
--
-- /Pre-release/
{-# INLINE writeIORef #-}
writeIORef :: Storable a => IORef a -> a -> IO ()
writeIORef ref x = asPtrUnsafe ref $ \ptr -> pokeByteOff ptr 0 x

-- | Read a value from an 'IORef'.
--
-- /Pre-release/
{-# INLINE readIORef #-}
readIORef :: Storable a => IORef a -> IO a
readIORef ref = asPtrUnsafe ref $ \ptr -> peekByteOff ptr 0

-- XXX What if GC + reallocation happens in the middle?
-- XXX Will unpinned work?
-- | Modify the value of an 'IORef' using a function with strict application.
--
-- /Pre-release/
{-# INLINE modifyIORef' #-}
modifyIORef' :: Storable a => IORef a -> (a -> a) -> IO ()
modifyIORef' ref g =
    asPtrUnsafe ref $ \ptr -> do
        a <- peekByteOff ptr 0
        let a' = g a
        a' `seq` pokeByteOff ptr 0 a'

-- | Generate a stream by continuously reading the IORef.
--
-- /Pre-release/
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: (MonadIO m, Storable a) => IORef a -> D.Stream m a
toStreamD var = D.Stream step ()

    where

    {-# INLINE_LATE step #-}
    step _ () = liftIO (readIORef var) >>= \x -> return $ D.Yield x ()
