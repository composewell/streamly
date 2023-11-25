-- |
-- Module      : Streamly.Internal.Data.Serialize.Type
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.Serialize.Type
    (
      Serialize(..)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.List (foldl')
import Data.Proxy (Proxy (..))
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.MutByteArray.Type (MutByteArray(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import GHC.Stable (StablePtr(..))

import qualified Streamly.Internal.Data.MutByteArray.Type as MBA
import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.MutArray as MutArray

import GHC.Exts

--------------------------------------------------------------------------------
-- Developer Note
--------------------------------------------------------------------------------

-- IMPORTANT
-- =========
--
-- Don't ever serialize the absolute offsets in the encoding. Serialize length
-- instead. Absolute offsets are NOT stable.
--
-- They will only work if the start offset of the Array when encoding and
-- decoding is the same. This is almost never the case.

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The 'Serialize' type class provides operations for serialization and
-- deserialization of general Haskell data types to and from their byte stream
-- representation.
--
-- Unlike 'Unbox', 'Serialize' uses variable length encoding, therefore, it can
-- serialize recursive and variable length data types like lists, or variable
-- length sum types where the length of the value may vary depending on a
-- particular constructor. For variable length data types the length is encoded
-- along with the data.
--
-- The 'deserializeAt' operation reads bytes from the mutable byte array and
-- builds a Haskell data type from these bytes, the number of bytes it reads
-- depends on the type and the encoded value it is reading. 'serializeAt'
-- operation converts a Haskell data type to its binary representation which
-- must consist of as many bytes as added by the @addSizeTo@ operation for that
-- value and then stores these bytes into the mutable byte array. The
-- programmer is expected to use the @addSizeTo@ operation and allocate an
-- array of sufficient length before calling 'serializeAt'.
--
-- IMPORTANT: The serialized data's byte ordering remains the same as the host
-- machine's byte order. Therefore, it can not be deserialized from host
-- machines with a different byte ordering.
--
-- Instances can be derived via Template Haskell, or written manually.
--
-- Here is an example, for deriving an instance of this type class using
-- template Haskell:
--
-- >>> :{
-- data Object = Object
--     { _obj1 :: [Int]
--     , _obj2 :: Int
--     }
-- :}
--
-- @
-- import Streamly.Data.MutByteArray (deriveSerialize)
-- \$(deriveSerialize [d|instance Serialize Object|])
-- @
--
-- See 'Streamly.Data.MutByteArray.deriveSerialize' and
-- 'Streamly.Data.MutByteArray.deriveSerializeWith' for more information on
-- deriving using Template Haskell.
--
-- Here is an example of a manual instance.
--
-- >>> import Streamly.Data.MutByteArray (Serialize(..))
--
-- >>> :{
-- instance Serialize Object where
--     addSizeTo acc obj = addSizeTo (addSizeTo acc (_obj1 obj)) (_obj2 obj)
--     deserializeAt i arr len = do
--          -- Check the array bounds before reading
--         (i1, x0) <- deserializeAt i arr len
--         (i2, x1) <- deserializeAt i1 arr len
--         pure (i2, Object x0 x1)
--     serializeAt i arr (Object x0 x1) = do
--         i1 <- serializeAt i arr x0
--         i2 <- serializeAt i1 arr x1
--         pure i2
-- :}
--
class Serialize a where
    -- XXX Use (a -> Sum Int) instead, remove the Size type

    -- A left fold step to fold a generic structure to its serializable size.
    -- It is of the form @Int -> a -> Int@ because you can have tail-recursive
    -- traversal of the structures.

    -- | @addSizeTo accum value@ returns @accum@ incremented by the size of the
    -- serialized representation of @value@ in bytes. Size cannot be zero. It
    -- should be at least 1 byte.
    addSizeTo :: Int -> a -> Int

    -- We can implement the following functions without returning the `Int`
    -- offset but that may require traversing the Haskell structure again to get
    -- the size. Therefore, this is a performance optimization.

    -- | @deserializeAt byte-offset array arrayLen@ deserializes a value from the
    -- given byte-offset in the array. Returns a tuple consisting of the next
    -- byte-offset and the deserialized value.
    --
    -- Throws an exception if the operation would exceed the supplied arrayLen.
    deserializeAt :: Int -> MutByteArray -> Int -> IO (Int, a)

    -- | @serializeAt byte-offset array value@ writes the serialized
    -- representation of the @value@ in the array at the given byte-offset.
    -- Returns the next byte-offset.
    --
    -- This is an unsafe operation, the programmer must ensure that the array
    -- has enough space available to serialize the value as determined by the
    -- @addSizeTo@ operation.
    serializeAt :: Int -> MutByteArray -> a -> IO Int

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- XXX We do not need to pass the end of data offset, we can just check the
-- mutable array end here to avoid a crash, and when we return from deserialize
-- we can check if the offset returned is beyond the bound or not.
--
#ifdef DEBUG
{-# INLINE checkBounds #-}
checkBounds :: String -> Int -> MutByteArray -> IO ()
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
{-# INLINE deserializeChecked #-}
deserializeChecked :: forall a. Unbox a => Int -> MutByteArray -> Int -> IO (Int, a)
deserializeChecked off arr sz =
    let next = off + Unbox.sizeOf (Proxy :: Proxy a)
     in do
        -- Keep likely path in the straight branch.
        if (next <= sz)
        then Unbox.peekAt off arr >>= \val -> pure (next, val)
        else error
            $ "deserializeAt: accessing array at offset = "
                ++ show (next - 1)
                ++ " max valid offset = " ++ show (sz - 1)

{-# INLINE serializeUnsafe #-}
serializeUnsafe :: forall a. Unbox a => Int -> MutByteArray -> a -> IO Int
serializeUnsafe off arr val =
    let next = off + Unbox.sizeOf (Proxy :: Proxy a)
     in do
#ifdef DEBUG
        checkBounds "serializeAt" next arr
#endif
        Unbox.pokeAt off arr val
        pure next

#define DERIVE_SERIALIZE_FROM_UNBOX(_type) \
instance Serialize _type where \
; {-# INLINE addSizeTo #-} \
;    addSizeTo acc _ = acc +  Unbox.sizeOf (Proxy :: Proxy _type) \
; {-# INLINE deserializeAt #-} \
;    deserializeAt off arr end = deserializeChecked off arr end :: IO (Int, _type) \
; {-# INLINE serializeAt #-} \
;    serializeAt =  \
        serializeUnsafe :: Int -> MutByteArray -> _type -> IO Int

DERIVE_SERIALIZE_FROM_UNBOX(())
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

    -- {-# INLINE addSizeTo #-}
    addSizeTo acc xs =
        foldl' addSizeTo (acc + (Unbox.sizeOf (Proxy :: Proxy Int))) xs

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE deserializeAt #-}
    deserializeAt off arr sz = do
        (off1, len64) <- deserializeAt off arr sz :: IO (Int, Int64)
        let len = (fromIntegral :: Int64 -> Int) len64
            peekList f o i | i >= 3 = do
              -- Unfold the loop three times
              (o1, x1) <- deserializeAt o arr sz
              (o2, x2) <- deserializeAt o1 arr sz
              (o3, x3) <- deserializeAt o2 arr sz
              peekList (f . (\xs -> x1:x2:x3:xs)) o3 (i - 3)
            peekList f o 0 = pure (o, f [])
            peekList f o i = do
              (o1, x) <- deserializeAt o arr sz
              peekList (f . (x:)) o1 (i - 1)
        peekList id off1 len

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE serializeAt #-}
    serializeAt off arr val = do
        let off1 = off + Unbox.sizeOf (Proxy :: Proxy Int64)
        let pokeList acc o [] =
              Unbox.pokeAt off arr (acc :: Int64) >> pure o
            pokeList acc o (x:xs) = do
              o1 <- serializeAt o arr x
              pokeList (acc + 1) o1 xs
        pokeList 0 off1 val

instance Serialize (Array a) where
    {-# INLINE addSizeTo #-}
    addSizeTo i (Array {..}) = i + (arrEnd - arrStart) + 8

    {-# INLINE deserializeAt #-}
    deserializeAt off arr len = do
        (off1, byteLen) <- deserializeAt off arr len :: IO (Int, Int)
        let off2 = off1 + byteLen
        when (off2 > len) $
            error
                $ "deserializeAt: accessing array at offset = "
                    ++ show (off2 - 1)
                    ++ " max valid offset = " ++ show (len - 1)
        -- XXX Use MutByteArray.cloneSliceUnsafe
        let slice = MutArray.MutArray arr off1 off2 off2
        newArr <- MutArray.clone slice
        pure (off2, Array.unsafeFreeze newArr)

    {-# INLINE serializeAt #-}
    serializeAt off arr (Array {..}) = do
        let arrLen = arrEnd - arrStart
        off1 <- serializeAt off arr arrLen
        MBA.putSliceUnsafe arrContents arrStart arr off1 arrLen
        pure (off1 + arrLen)

instance (Serialize a, Serialize b) => Serialize (a, b) where

    {-# INLINE addSizeTo #-}
    addSizeTo acc (a, b) = addSizeTo (addSizeTo acc a) b

    {-# INLINE serializeAt #-}
    serializeAt off arr (a, b) = do
        off1 <- serializeAt off arr a
        serializeAt off1 arr b

    {-# INLINE deserializeAt #-}
    deserializeAt off arr end = do
        (off1, a) <- deserializeAt off arr end
        (off2, b) <- deserializeAt off1 arr end
        pure (off2, (a, b))
