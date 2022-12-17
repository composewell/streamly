-- |
-- Module      : Streamly.Internal.Serialize
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Convert Haskell data types to byte streams and vice-versa.

module Streamly.Internal.Serialize
    ( ToBytes (..)
    , FromBytes (..)
    )
where

import Data.Word (Word8)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Parser (Parser)

class ToBytes a where
    -- | Convert a Haskell type to a byte stream.
    toBytes :: a -> Stream m Word8

class FromBytes a where
    -- | Decode a byte stream to a Haskell type.
    fromBytes :: Parser Word8 m a
