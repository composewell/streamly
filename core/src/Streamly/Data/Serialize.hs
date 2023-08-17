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

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Serialize
    ( Serialize(..)
    , Size(..)
    , decode
    , encode
    , pinnedEncode
    )
import Streamly.Internal.Data.Serialize.TH
    ( deriveSerialize
    , deriveSerializeWith
    )
import Streamly.Internal.Data.Unbox
    ( MutableByteArray
    , Unbox(..)
    )
import Streamly.Internal.Data.Unbox.TH (deriveUnbox, deriveUnboxWith)
