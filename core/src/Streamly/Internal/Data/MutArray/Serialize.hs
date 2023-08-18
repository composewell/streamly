-- |
-- Module      : Streamly.Internal.Data.MutArray.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.MutArray.Serialize
    ( Size(..)
    , Serialize(..)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

#include "assert.hs"

#ifdef DEBUG
import Control.Exception (assert)
import Streamly.Internal.Data.Unbox (Unbox(..), sizeOfMutableByteArray)
#endif

import Data.Proxy (Proxy (..))
import Streamly.Internal.Data.MutArray.Type (MutArray)
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import GHC.Stable (StablePtr(..))

import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.MutArray.Type as MutArray

import GHC.Exts

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A left fold step to fold a generic structure to its serializable size.
newtype Size a = Size (Int -> a -> Int) -- a left fold or Sum monoid

class Serialize a where
    -- | Get the 'Size', in bytes, reqired to store the serialized
    -- representation of the type. Size cannot be zero.
    size :: Size a

    -- | Deserialize a value from the given byte-index in the array. Returns a
    -- tuple of deserialized value and potentially a reallocated array.
    deserialize :: MutArray Word8 -> IO (a, MutArray Word8)

    -- | Write the serialized representation of the value in the array.
    serialize :: MutArray Word8 -> a -> IO (MutArray Word8)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- XXX check bounds in deserialize, use box instead of boxUnsafe
#define DERIVE_SERIALIZE_FROM_UNBOX(_type) \
instance Serialize _type where \
; {-# INLINE size #-} \
;    size = Size (\acc _ -> acc +  Unbox.sizeOf (Proxy :: Proxy _type)) \
; {-# INLINE deserialize #-} \
;    deserialize = MutArray.boxUnsafe \
; {-# INLINE serialize #-} \
;    serialize = MutArray.unbox

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
