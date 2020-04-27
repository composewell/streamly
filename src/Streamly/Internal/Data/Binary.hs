{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans  #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Binary
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generic serialization (encoding to Word8 stream) and deserialization
-- (decoding from Word8 stream) for any Haskell type based on a 'Binary' type
-- class.

module Streamly.Internal.Data.Binary
    (
      Binary (..)
    )
where

import Control.Monad.Catch (MonadCatch)
import Data.Bits ((.|.))
import Data.Word (Word8)
import GHC.Base
import GHC.Word hiding (eqWord8)

import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Binary.Encode as EN
import qualified Streamly.Internal.Data.Binary.Decode as DE
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Prelude as S

-------------------------------------------------------------------------------
-- Type class for serializing and deserializing
-------------------------------------------------------------------------------

class Binary a where
    decode :: MonadCatch m => Parser m Word8 a
    -- XXX an unfold would be more efficient. We will have to concatMap to
    -- concatenate the streams, and Stream type is not efficient for that.
    encode :: Monad m => a -> SerialT m Word8

-------------------------------------------------------------------------------
-- Binary instances
-------------------------------------------------------------------------------

-- | () <=> 0
instance Binary () where
    decode     = DE.eqWord8 0 *> PR.yield ()
    encode ()  = S.yield 0

-- | False <=> 0, True <=> 1
instance Binary Bool where
    decode = DE.bool
    encode = EN.bool

-- | LT <=> 0, EQ <=> 1, GT <=> 2
instance Binary Ordering where
    decode = DE.ordering
    encode = EN.ordering

{-
-- | Char <=> UTF8
instance Binary Char where
    decode = DE.charUtf8
    encode = EN.charUtf8
    -}

-------------------------------------------------------------------------------
-- Words
-------------------------------------------------------------------------------

-- | Word8 <=> Word8
instance Binary Word8 where
    decode = DE.word8
    encode = EN.word8

-- XXX Should we use little endian instead so that we do not need to convert?
-- | Word16 <=> first MSB then LSB (big endian)
instance Binary Word16 where
    decode = DE.word16be
    encode = EN.word16be

{-
-- | Word32 <=> first MSB then LSBs (big endian)
instance Binary Word32 where
    decode = DE.word32be
    encode = EN.word32be

-- | Word64 <=> first MSB then LSBs (big endian)
instance Binary Word64 where
    decode = DE.word64be
    encode = EN.word64be

-- | Word <=> first MSB then LSBs (big endian)
instance Binary Word where
    decode = DE.wordbe
    encode = EN.wordbe

-------------------------------------------------------------------------------
-- Ints
-------------------------------------------------------------------------------

-- | Int8 <=> Int8
instance Binary Int8 where
    decode = DE.int8
    encode = EN.int8

-- XXX Should we use little endian instead so that we do not need to convert?
-- | Int16 <=> first MSB then LSB (big endian)
instance Binary Int16 where
    decode = DE.int16be
    encode = EN.int16be

-- | Int32 <=> first MSB then LSBs (big endian)
instance Binary Int32 where
    decode = DE.int32be
    encode = EN.int32be

-- | Int64 <=> first MSB then LSBs (big endian)
instance Binary Int64 where
    decode = DE.int64be
    encode = EN.int64be

-- | Int <=> first MSB then LSBs (big endian)
instance Binary Int where
    decode = DE.intbe
    encode = EN.intbe

-- | Integer <=> first MSB then LSBs (big endian)
instance Binary Integer where
    decode = DE.integerbe
    encode = EN.integerbe

-- | Ratio <=> numerator first then denominator
instance Binary (Ratio a) where
    decode = DE.ratio
    encode = EN.ratio

-- | Natural <=> first MSB then LSBs (big endian)
instance Binary Natural where
    decode = DE.natural
    encode = EN.integerbe

instance Binary Float where
    decode = DE.float32be
    encode = EN.float32be

instance Binary Double where
    decode = DE.float64be
    encode = EN.float64be

-------------------------------------------------------------------------------
-- Tuples
-------------------------------------------------------------------------------

-- | @(a,b)@ <=> first @a@ then @b@
instance Binary (a,b) where
    decode = DE.twoOf
    encode = EN.twoOf

-- | @[a]@ <=> first element first
instance Binary [a] where
    decode = DE.listOf
    encode = EN.listOf

-- | @Array a@ <=> first element first
instance Binary (Array a) where
    decode = DE.arrayOf
    encode = EN.arrayOf

-- | @Stream a@ <=> @Stream Word8@
instance Binary (Stream a) where
    decode = DE.streamOf
    encode = EN.streamOf
-}
