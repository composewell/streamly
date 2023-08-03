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
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.List (foldl')
import Data.Proxy (Proxy (..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))

import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import GHC.Stable (StablePtr(..))

import qualified Streamly.Internal.Data.Unbox as Unbox

import GHC.Exts

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Info about the length of a serializable type. Length can depend on the
-- value or can be independent.
data Size a
    = VarSize (a -> Int)
    | ConstSize !Int

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
--     { _varLen :: [Int]
--     , _constLen :: Int
--     }
-- :}
--
-- >>> :{
-- instance Serialize Object where
--     size =
--         case (size :: Size [Int], size :: Size Int) of
--             (VarSize f, ConstSize g) ->
--                 VarSize $ \obj ->
--                     f (_varLen obj) + g
--             _ -> error "size is not defined properly"
--     deserialize i arr = do
--         (i1, x0) <- deserialize i arr
--         (i2, x1) <- deserialize i1 arr
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

    -- | Deserialize a value from the given byte-index in the array. Returns a
    -- tuple of the next byte-index and the deserialized value.
    deserialize :: Int -> MutableByteArray -> IO (Int, a)

    -- | Write the serialized representation of the value in the array at the
    -- given byte-index. Returns the next byte-index.
    serialize :: Int -> MutableByteArray -> a -> IO Int

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

#define DERIVE_SERIALIZE_FROM_UNBOX(_type) \
instance Serialize _type where \
;    size = ConstSize $ Unbox.sizeOf (Proxy :: Proxy _type) \
;    deserialize off arr = \
        Unbox.peekByteIndex off arr >>= \
            \val -> let sz = Unbox.sizeOf (Proxy :: Proxy _type) \
                     in pure (off + sz, val) \
;    serialize off arr val = \
        Unbox.pokeByteIndex off arr val \
            >> let sz = Unbox.sizeOf (Proxy :: Proxy _type) \
                in pure (off + sz)

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

    {-# INLINE size #-}
    size = VarSize $ \lst ->
        case size :: Size a of
            VarSize f ->
                foldl'
                    (\acc x -> acc + f x)
                    (Unbox.sizeOf (Proxy :: Proxy Int))
                    lst
            ConstSize sz ->
                length lst
                    * sz
                    + Unbox.sizeOf (Proxy :: Proxy Int)

    {-# INLINE deserialize #-}
    deserialize off arr = do
        len <- Unbox.peekByteIndex off arr :: IO Int
        let off1 = off + Unbox.sizeOf (Proxy :: Proxy Int)
        let peekList buf o 0 = pure (o, reverse buf)
            peekList buf o i = do
              (o1, x) <- deserialize o arr
              peekList (x:buf) o1 (i - 1)
        peekList [] off1 len

    {-# INLINE serialize #-}
    serialize off arr val = do
        void $ serialize off arr (length val)
        let off1 = off + Unbox.sizeOf (Proxy :: Proxy Int)
        let pokeList o [] = pure o
            pokeList o (x:xs) = do
              o1 <- serialize o arr x
              pokeList o1 xs
        pokeList off1 val
