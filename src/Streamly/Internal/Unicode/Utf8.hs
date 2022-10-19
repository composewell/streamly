-- |
-- Module      : Streamly.Internal.Unicode.Utf8
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Utf8
    (
    -- * Type
      Utf8

    -- * Creation and elimination
    , pack
    , unpack
    , toArray
    )
where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Data.Word (Word8)
import Streamly.Data.Array.Unboxed (Array)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array.Unboxed as Array
    ( fromStreamN
    , read
    )
import qualified Streamly.Internal.Unicode.Stream as Unicode

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A space efficient, packed, unboxed Unicode container.
newtype Utf8 =
    Utf8 (Array Word8)
    deriving (NFData)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{-# INLINE toArray #-}
toArray :: Utf8 -> Array Word8
toArray (Utf8 arr) = arr


{-# INLINEABLE pack #-}
pack :: String -> Utf8
pack s =
    Utf8
        $ unsafePerformIO
        $ Array.fromStreamN len $ Unicode.encodeUtf8' $ Stream.fromList s

    where

    len = length s

{-# INLINEABLE unpack #-}
unpack :: Utf8 -> String
unpack u =
    unsafePerformIO
        $ Stream.fold Fold.toList $ Unicode.decodeUtf8' $ Array.read $ toArray u
