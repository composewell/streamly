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
        mbarr <- Unbox.newBytesAs ps len
        nextOff <- serialize 0 mbarr a
        assertM(nextOff == len)
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
    let len = Array.length arr
    (nextOff, val) <- deserialize 0 arrContents
    assertM(nextOff == len)
    pure val
