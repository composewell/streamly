-- |
-- Module      : Streamly.Internal.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.Serialize
    ( Size(..)
    , Serialize(..)
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

import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Array as Array

import GHC.Exts

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- XXX Use (a -> Sum Int) instead, remove the Size type

-- | A left fold step to fold a generic structure to its serializable size.
newtype Size a = Size (Int -> a -> Int) -- a left fold or Sum monoid

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
-- >>> import Streamly.Internal.Data.Serialize (Serialize(..), Size(..))
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
--     size =
--         case (size :: Size [Int], size :: Size Int) of
--             (Size f, Size g) ->
--                 Size $ \acc obj ->
--                     acc + f 0 (_obj1 obj) + g 0 (_obj2 obj)
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
    -- | Get the 'Size', in bytes, reqired to store the serialized
    -- representation of the type. Size cannot be zero.
    size :: Size a

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
;    size = Size (\acc _ -> acc +  Unbox.sizeOf (Proxy :: Proxy _type)) \
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
    size = Size $ \acc xs ->
        case size :: Size a of
            Size f -> foldl' f (acc + (Unbox.sizeOf (Proxy :: Proxy Int))) xs

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE deserialize #-}
    deserialize off arr sz = do
        len <- Unbox.peekByteIndex off arr :: IO Int
        let off1 = off + Unbox.sizeOf (Proxy :: Proxy Int)
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
-- High level functions
--------------------------------------------------------------------------------

{-# INLINE encodeAs #-}
encodeAs :: forall a. Serialize a => PinnedState -> a -> Array Word8
encodeAs ps a =
    unsafeInlineIO $ do
        let len =
              case size :: Size a of
                  Size f -> f 0 a
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
