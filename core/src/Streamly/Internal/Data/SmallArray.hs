{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.SmallArray
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.SmallArray
    (
      Array
    , unsafeFreeze
    , unsafeThaw
    , byteLength
    , length
    , read
    , createOf
    , castUnsafe
    , cast
    , asBytes
    , fromPureStreamN
    , asCStringUnsafe
    , fromByteStr
    , fromByteStr#
    , splice
    , toArray
    , fromArray
    , reader
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import Data.Proxy (Proxy(..))
import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import GHC.Ptr (Ptr(..))
import GHC.Exts (Addr#)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.MutSmallArray.Type (MutArray(..))
import Streamly.Internal.Data.MutByteArray.Type (MutByteArray)
import Streamly.Internal.Data.Producer.Type (Producer(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (length, read)

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.MutByteArray.Type as MutByteArray
import qualified Streamly.Internal.Data.MutSmallArray.Type as MutArray
import qualified Streamly.Internal.Data.Producer as Producer

newtype Array a = Array MutByteArray

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MutArray a -> Array a
unsafeFreeze (MutArray c) = Array c

{-# INLINE unsafeThaw #-}
unsafeThaw :: Array a -> MutArray a
unsafeThaw (Array c) = MutArray c

{-# INLINE toArray #-}
toArray :: Array a -> Array.Array a
toArray arr@(Array c) = Array.Array c 0 (byteLength arr)

-- Creates unpinned
{-# INLINE fromArray #-}
fromArray :: Array.Array a -> Array a
fromArray (Array.Array c start end) = unsafePerformIO $ do
    let len = end - start
    barr <- liftIO $ MutByteArray.new len
    MutByteArray.putSliceUnsafe c 0 barr 0 len
    return (Array barr)

{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength = MutArray.byteLength . unsafeThaw

{-# INLINE length #-}
length :: Unbox a => Array a -> Int
length arr = MutArray.length (unsafeThaw arr)

{-# INLINE_EARLY read #-}
read :: (Monad m, Unbox a) => Array a -> Stream m a
read arr = MutArray.toStreamWith (return . unsafeInlineIO) (unsafeThaw arr)

{-# INLINE_NORMAL createOf #-}
createOf :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (Array a)
createOf = fmap unsafeFreeze . MutArray.createOf

{-
{-# INLINE_NORMAL unsafeCreateOf #-}
unsafeCreateOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (Array a)
unsafeCreateOf n = unsafeFreeze <$> MutArray.unsafeCreateOf n
-}

castUnsafe :: Array a -> Array b
castUnsafe (Array contents) = Array contents

-- | Cast an @MutArray a@ into an @MutArray Word8@.
--
asBytes :: Array a -> Array Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
cast :: forall a b. Unbox b => Array a -> Maybe (Array b)
cast arr =
    let len = byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

{-# INLINE fromPureStreamN #-}
fromPureStreamN :: Unbox a => Int -> Stream Identity a -> Array a
fromPureStreamN n x =
    unsafePerformIO $ fmap unsafeFreeze (MutArray.fromPureStreamN n x)

{-# INLINE splice #-}
splice :: MonadIO m => Array a -> Array a -> m (Array a)
splice arr1 arr2 =
    unsafeFreeze <$> MutArray.splice (unsafeThaw arr1) (unsafeThaw arr2)

{-# INLINE toPinnedCString #-}
toPinnedCString :: Array Word8 -> Array Word8
toPinnedCString arr1 =
    unsafePerformIO (unsafeFreeze <$> MutArray.toPinnedCString (unsafeThaw arr1))

{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => Array a -> (Ptr a -> m b) -> m b
asPtrUnsafe arr = MutArray.asPtrUnsafe (unsafeThaw arr)

asCStringUnsafe :: Array a -> (CString -> IO b) -> IO b
asCStringUnsafe arr act = do
    let arr1 = toPinnedCString (asBytes arr)
    -- asPtrUnsafe makes sure the array is pinned
    asPtrUnsafe arr1 $ \ptr -> act (castPtr ptr)

fromByteStr# :: MonadIO m => Addr# -> m (Array Word8)
fromByteStr# addr = fmap unsafeFreeze (MutArray.fromByteStr# addr)

fromByteStr :: MonadIO m => Ptr Word8 -> m (Array Word8)
fromByteStr (Ptr addr#) = fromByteStr# addr#

{-# INLINE_NORMAL producer #-}
producer :: forall m a. (Monad m, Unbox a) => Producer m (Array a) a
producer =
    Producer.translate unsafeThaw unsafeFreeze
        $ MutArray.producerWith (return . unsafeInlineIO)

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
reader = Producer.simplify producer
