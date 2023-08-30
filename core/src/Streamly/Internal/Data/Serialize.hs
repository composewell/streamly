-- |
-- Module      : Streamly.Internal.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.Serialize
    ( Serialize(..)
    , VLWord64(..)
    , encode
    , pinnedEncode
    , decode
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

#include "assert.hs"

#ifdef DEBUG
import Control.Exception (assert)
#endif

import Control.Monad (void)
import Data.List (foldl')
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Streamly.Internal.Data.Unbox
    ( MutableByteArray(..)
    , PinnedState(..)
    , Unbox
    )
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.System.IO (unsafeInlineIO)
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import GHC.Stable (StablePtr(..))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)

import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Array as Array

import GHC.Exts

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A type implementing the 'Serialize' interface supplies operations for
-- reading and writing the type from and to a mutable byte array (an unboxed
-- representation of the type) in memory. The read operation 'deserialize'
-- deserializes the boxed type from the mutable byte array. The write operation
-- 'serialize' serializes the boxed type to the mutable byte array.
--
-- IMPORTANT: The serialized data's byte ordering is not normalized, which means
-- it remains the same as the machine's byte order where the function is
-- executed. Consequently, it may not be compatible across machines with
-- different byte ordering.
--
-- 'Serialize' contains enough information to serialize and deserialize variable
-- length types.
--
-- >>> import Streamly.Internal.Data.Serialize (Serialize(..))
--
-- >>> :{
-- data Object = Object
--     { _obj1 :: [Int]
--     , _obj2 :: Int
--     }
-- :}
--
-- >>> :{
-- instance Serialize Object where
--     size acc obj = size (size acc (_obj1 obj)) (_obj2 obj)
--     deserialize i arr len = do
--         (i1, x0) <- deserialize i arr len
--         (i2, x1) <- deserialize i1 arr len
--         pure (i2, Object x0 x1)
--     serialize i arr (Object x0 x1) = do
--         i1 <- serialize i arr x0
--         i2 <- serialize i1 arr x1
--         pure i2
-- :}
--
class Serialize a where
    -- XXX Use (a -> Sum Int) instead, remove the Size type

    -- A left fold step to fold a generic structure to its serializable size.
    -- It is of the form @Int -> a -> Int@ because you can have tail-recursive
    -- traversal of the structures.

    -- | Get the size, in bytes, reqired to store the serialized
    -- representation of the type. Size cannot be zero.
    size :: Int -> a -> Int

    -- We can implement the following functions without returning the `Int`
    -- offset but that may require traversing the Haskell structure again to get
    -- the size. Therefore, this is a performance optimization.

    -- | @deserialize offset array arrayLen@ deserializes a value from the
    -- given byte-index in the array. Returns a tuple of the next byte-index
    -- and the deserialized value.
    deserialize :: Int -> MutableByteArray -> Int -> IO (Int, a)

    -- | Write the serialized representation of the value in the array at the
    -- given byte-index. Returns the next byte-index. This is an unsafe
    -- operation, the programmer must ensure that the array has enough space
    -- available in the array to serialize the value as determined by the
    -- @size@ operation.
    serialize :: Int -> MutableByteArray -> a -> IO Int

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- XXX We do not need to pass the end of data offset, we can just check the
-- mutable array end here to avoid a crash, and when we return from deserialize
-- we can check if the offset returned is beyond the bound or not.
--
#ifdef DEBUG
{-# INLINE checkBounds #-}
checkBounds :: String -> Int -> MutableByteArray -> IO ()
checkBounds _label _off _arr = do
    sz <- sizeOfMutableByteArray _arr
    if (_off > sz)
    then error
        $ _label
            ++ ": accessing array at offset = "
            ++ show (_off - 1)
            ++ " max valid offset = " ++ show (sz - 1)
    else return ()
#endif

-- Note: Instead of passing around the size parameter, we can use
-- (sizeOfMutableByteArray arr) for checking the array bound, but that turns
-- out to be more expensive.
--
-- Another way to optimize this is to avoid the check for fixed size
-- structures. For fixed size structures we can do a check at the top level and
-- then use checkless deserialization using the Unbox type class. That will
-- require ConstSize and VarSize constructors in size. The programmer can
-- bundle all const size fields in a newtype to make serialization faster. This
-- can speed up the computation of size when serializing and checking size when
-- deserialing.
--
-- For variable size non-recursive structures a separate size validation method
-- could be used to validate the size before deserializing. "validate" can also
-- be used to collpase multiple chunks of arrays coming from network into a
-- single array for deserializing. But that can also be done by framing the
-- serialized value with a size header.
--
{-# INLINE deserializeUnsafe #-}
deserializeUnsafe :: forall a. Unbox a => Int -> MutableByteArray -> Int -> IO (Int, a)
deserializeUnsafe off arr sz =
    let next = off + Unbox.sizeOf (Proxy :: Proxy a)
     in do
        -- Keep likely path in the straight branch.
        if (next <= sz)
        then Unbox.peekByteIndex off arr >>= \val -> pure (next, val)
        else error
            $ "deserialize: accessing array at offset = "
                ++ show (next - 1)
                ++ " max valid offset = " ++ show (sz - 1)

{-# INLINE serializeUnsafe #-}
serializeUnsafe :: forall a. Unbox a => Int -> MutableByteArray -> a -> IO Int
serializeUnsafe off arr val =
    let next = off + Unbox.sizeOf (Proxy :: Proxy a)
     in do
#ifdef DEBUG
        checkBounds "serialize" next arr
#endif
        Unbox.pokeByteIndex off arr val
        pure next

#define DERIVE_SERIALIZE_FROM_UNBOX(_type) \
instance Serialize _type where \
; {-# INLINE size #-} \
;    size acc _ = acc +  Unbox.sizeOf (Proxy :: Proxy _type) \
; {-# INLINE deserialize #-} \
;    deserialize off arr end = deserializeUnsafe off arr end :: IO (Int, _type) \
; {-# INLINE serialize #-} \
;    serialize =  \
        serializeUnsafe :: Int -> MutableByteArray -> _type -> IO Int

DERIVE_SERIALIZE_FROM_UNBOX(Bool)
DERIVE_SERIALIZE_FROM_UNBOX(Char)
DERIVE_SERIALIZE_FROM_UNBOX(Int8)
DERIVE_SERIALIZE_FROM_UNBOX(Int16)
DERIVE_SERIALIZE_FROM_UNBOX(Int32)
DERIVE_SERIALIZE_FROM_UNBOX(Int)
DERIVE_SERIALIZE_FROM_UNBOX(Int64)
DERIVE_SERIALIZE_FROM_UNBOX(Word)
DERIVE_SERIALIZE_FROM_UNBOX(Word8)
DERIVE_SERIALIZE_FROM_UNBOX(Word16)
DERIVE_SERIALIZE_FROM_UNBOX(Word32)
DERIVE_SERIALIZE_FROM_UNBOX(Word64)
DERIVE_SERIALIZE_FROM_UNBOX(Double)
DERIVE_SERIALIZE_FROM_UNBOX(Float)
DERIVE_SERIALIZE_FROM_UNBOX((StablePtr a))
DERIVE_SERIALIZE_FROM_UNBOX((Ptr a))
DERIVE_SERIALIZE_FROM_UNBOX((FunPtr a))

instance forall a. Serialize a => Serialize [a] where

    -- {-# INLINE size #-}
    size acc xs = foldl' size (acc + (Unbox.sizeOf (Proxy :: Proxy Int))) xs

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE deserialize #-}
    deserialize off arr sz = do
        (off1, len) <- deserialize off arr sz :: IO (Int, Int)
        let
            peekList f o i | i >= 3 = do
              -- Unfold the loop three times
              (o1, x1) <- deserialize o arr sz
              (o2, x2) <- deserialize o1 arr sz
              (o3, x3) <- deserialize o2 arr sz
              peekList (f . (\xs -> x1:x2:x3:xs)) o3 (i - 3)
            peekList f o 0 = pure (o, f [])
            peekList f o i = do
              (o1, x) <- deserialize o arr sz
              peekList (f . (x:)) o1 (i - 1)
        peekList id off1 len

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE serialize #-}
    serialize off arr val = do
        void $ serialize off arr (length val)
        let off1 = off + Unbox.sizeOf (Proxy :: Proxy Int)
        let pokeList o [] = pure o
            pokeList o (x:xs) = do
              o1 <- serialize o arr x
              pokeList o1 xs
        pokeList off1 val

--------------------------------------------------------------------------------
-- Variable length encoding for unsigned 64 bit integer
--------------------------------------------------------------------------------

-- See https://sqlite.org/src4/doc/trunk/www/varint.wiki
newtype VLWord64 =
    VLWord64 Word64
    deriving (Num, Enum, Real, Integral, Show, Eq, Ord, Bounded, Generic)

-- | div256 x = x `div` 256
div256 :: Word64 -> Word64
div256 x = x `shiftR` 8

-- | mult256 x = x * 256
mult256 :: Word64 -> Word64
mult256 x = x `shiftL` 8

-- | mod256 x = x % 256
mod256 :: Word64 -> Word64
mod256 x = x .&. 0xFF

w64_w8 :: Word64 -> Word8
w64_w8 = fromIntegral

w64_w16 :: Word64 -> Word16
w64_w16 = fromIntegral

w64_w32 :: Word64 -> Word32
w64_w32 = fromIntegral

w8_64 :: Word8 -> Word64
w8_64 = fromIntegral

w16_64 :: Word16 -> Word64
w16_64 = fromIntegral

w32_64 :: Word32 -> Word64
w32_64 = fromIntegral

instance Serialize VLWord64 where

    {-# INLINE size #-}
    size acc (VLWord64 x)
         | x <= 240 = 1 + acc
         | x <= 2287 = 2 + acc
         | x <= 67823 = 3 + acc
         | x <= 16777215 = 4 + acc
         | x <= 4294967295 = 5 + acc
         | x <= 1099511627775 = 6 + acc
         | x <= 281474976710655 = 7 + acc
         | x <= 72057594037927935 = 8 + acc
         | otherwise = 9 + acc

    -- Inlining this causes large compilation times for tests
    {-# INLINE deserialize #-}
    deserialize off arr sz = do
        (off1, b0 :: Word8) <- deserialize off arr sz
        if b0 <= 240
        then pure (off1, VLWord64 (w8_64 b0))
        else if b0 >= 241 && b0 <= 248
        then do
            (off2, b1 :: Word8) <- deserialize off1 arr sz
            pure (off2, VLWord64 (240 + mult256 (w8_64 (b0 - 241)) + w8_64 b1))
        else if b0 == 249
        then do
            (off2, b1 :: Word8) <- deserialize off1 arr sz
            (off3, b2 :: Word8) <- deserialize off2 arr sz
            pure (off3, VLWord64 (2288 + mult256 (w8_64 b1) + w8_64 b2))
        else if b0 == 250
        then do
            (off2, b1 :: Word8) <- deserialize off1 arr sz
            (off3, b2_3 :: Word16) <- deserialize off2 arr sz
            pure (off3, VLWord64 ((w8_64 b1 `shiftL` 16) .|. w16_64 b2_3))
        else if b0 == 251
        then do
            (off2, b1_4 :: Word32) <- deserialize off1 arr sz
            pure (off2, VLWord64 (w32_64 b1_4))
        else if b0 == 252
        then do
            (off2, b1 :: Word8) <- deserialize off1 arr sz
            (off3, b2_5 :: Word32) <- deserialize off2 arr sz
            pure (off3, VLWord64 ((w8_64 b1 `shiftL` 32) .|. w32_64 b2_5))
        else if b0 == 253
        then do
            (off2, b1_2 :: Word16) <- deserialize off1 arr sz
            (off3, b3_6 :: Word32) <- deserialize off2 arr sz
            pure (off3, VLWord64 ((w16_64 b1_2 `shiftL` 32) .|. w32_64 b3_6))
        else if b0 == 254
        then do
            (off2, b1 :: Word8) <- deserialize off1 arr sz
            (off3, b2_3 :: Word16) <- deserialize off2 arr sz
            (off4, b4_7 :: Word32) <- deserialize off3 arr sz
            pure
                ( off4
                , VLWord64
                      ((w8_64 b1 `shiftL` 48)
                           .|. (w16_64 b2_3 `shiftL` 32)
                           .|. w32_64 b4_7))
        else do
            (off2, b1_8 :: Word64) <- deserialize off1 arr sz
            pure (off2, VLWord64 b1_8)

    -- Inlining this causes large compilation times for tests
    {-# INLINE serialize #-}
    serialize off arr (VLWord64 v)
         | v <= 240 = serialize off arr (fromIntegral v :: Word8)
         | v <= 2287 = do
               let b0 = w64_w8 $ div256 (v - 240) + 241
                   b1 = w64_w8 $ mod256 (v - 240)
               off1 <- serialize off arr b0
               serialize off1 arr b1
         | v <= 67823 = do
               let b0 = 249 :: Word8
                   b1 = w64_w8 $ div256 (v - 2288)
                   b2 = w64_w8 $ mod256 (v - 2288)
               off1 <- serialize off arr b0
               off2 <- serialize off1 arr b1
               serialize off2 arr b2
         | v <= 16777215 = do
               let b0 = 250 :: Word8
                   b1 = w64_w8 $ v `shiftR` 16
                   b2_3 = w64_w16 v
               off1 <- serialize off arr b0
               off2 <- serialize off1 arr b1
               serialize off2 arr b2_3
         | v <= 4294967295 = do
               let b0 = 251 :: Word8
                   b1_4 = w64_w32 v
               off1 <- serialize off arr b0
               serialize off1 arr b1_4
         | v <= 1099511627775 = do
               let b0 = 252 :: Word8
                   b1 = w64_w8 $ v `shiftR` 32
                   b2_5 = w64_w32 v
               off1 <- serialize off arr b0
               off2 <- serialize off1 arr b1
               serialize off2 arr b2_5
         | v <= 281474976710655 = do
               let b0 = 253 :: Word8
                   b1_2 = w64_w16 $ v `shiftR` 32
                   b3_6 = w64_w32 v
               off1 <- serialize off arr b0
               off2 <- serialize off1 arr b1_2
               serialize off2 arr b3_6
         | v <= 72057594037927935 = do
               let b0 = 254 :: Word8
                   b1 = w64_w8 $ v `shiftR` 48
                   b2_3 = w64_w16 $ v `shiftR` 32
                   b4_7 = w64_w32 v
               off1 <- serialize off arr b0
               off2 <- serialize off1 arr b1
               off3 <- serialize off2 arr b2_3
               serialize off3 arr b4_7
         | otherwise = do
               let b0 = 255 :: Word8
                   b1_8 = v
               off1 <- serialize off arr b0
               serialize off1 arr b1_8

--------------------------------------------------------------------------------
-- High level functions
--------------------------------------------------------------------------------

{-# INLINE encodeAs #-}
encodeAs :: forall a. Serialize a => PinnedState -> a -> Array Word8
encodeAs ps a =
    unsafeInlineIO $ do
        let len = size 0 a
        -- We encode the length of the encoding as a header hence the 8 extra
        -- bytes to encode Int64
        mbarr <- Unbox.newBytesAs ps (8  +  len)
        off1 <- serialize 0 mbarr (fromIntegral len :: Int64)
        off2 <- serialize off1 mbarr a
        assertM(off2 == len + off1)
        pure $ Array mbarr 0 len

{-# INLINE encode #-}
encode :: Serialize a => a -> Array Word8
encode = encodeAs Unpinned

{-# INLINE pinnedEncode #-}
pinnedEncode :: Serialize a => a -> Array Word8
pinnedEncode = encodeAs Pinned

{-# INLINE decode #-}
decode :: Serialize a => Array Word8 -> a
decode arr@(Array {..}) = unsafeInlineIO $ do
    let lenArr = Array.length arr
    (off1, lenEncoding :: Int64) <- deserialize 0 arrContents lenArr
    (off2, val) <- deserialize off1 arrContents lenArr
    assertM(fromIntegral lenEncoding + off1 == off2)
    assertM(lenArr == off2)
    pure val
