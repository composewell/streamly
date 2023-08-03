-- |
-- Module      : Streamly.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Data.Serialize
    (

    -- * Types
      MutableByteArray

    -- * Unbox
    , Unbox(..)
    , deriveUnbox
    , deriveUnboxWith

    -- * Serialize
    , Size(..)
    , Serialize(..)
    , deriveSerialize
    , deriveSerializeWith

    -- * Encoding
    , encode
    , pinnedEncode

    -- * Decoding
    , decode
    ) where

#include "assert.hs"

import Data.Int (Int64)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Serialize (Serialize(..), Size(..))
import Streamly.Internal.Data.Serialize.TH
    ( deriveSerialize
    , deriveSerializeWith
    )
import Streamly.Internal.Data.Unbox
    ( MutableByteArray
    , PinnedState(..)
    , Unbox(..)
    )
import Streamly.Internal.Data.Unbox.TH (deriveUnbox, deriveUnboxWith)
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Array as Array

--------------------------------------------------------------------------------
-- Serialize
--------------------------------------------------------------------------------

{-# INLINE encodeAs #-}
encodeAs :: forall a. Serialize a => PinnedState -> a -> Array Word8
encodeAs ps a =
    unsafeInlineIO $ do
        let len =
              case size :: Size a of
                  ConstSize sz -> sz
                  VarSize f -> f a
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

--------------------------------------------------------------------------------
-- Deserialize
--------------------------------------------------------------------------------

{-# INLINE decode #-}
decode :: Serialize a => Array Word8 -> a
decode arr@(Array {..}) = unsafeInlineIO $ do
    let lenArr = Array.length arr
    (off1, lenEncoding :: Int64) <- deserialize 0 arrContents
    (off2, val) <- deserialize off1 arrContents
    assertM(fromIntegral lenEncoding + off1 == off2)
    assertM(lenArr == off2)
    pure val
