-- |
-- Module      : Streamly.Internal.Data.Binary.Decode
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parsers for binary encoded basic Haskell data types.

module Streamly.Internal.Data.Binary.Decode
    ( unit
    , bool
    , ordering
    , eqWord8
    , word8
    , word16be
    )
where

import Control.Monad.Catch (MonadCatch, throwM)
import Data.Bits ((.|.), unsafeShiftL)
import Data.Word (Word8, Word16)
import Streamly.Internal.Data.Parser (Parser)

import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PRK

-- | A value of type '()' is encoded as @0@ in binary encoding.
--
-- @
-- 0 ==> ()
-- @
--
-- /Internal/
--
{-# INLINE unit #-}
unit :: MonadCatch m => Parser m Word8 ()
unit = eqWord8 0 *> PR.yield ()

{-# INLINE word8ToBool #-}
word8ToBool :: Word8 -> Either String Bool
word8ToBool 0 = Right False
word8ToBool 1 = Right True
word8ToBool w = Left ("Invalid Bool encoding " ++ Prelude.show w)

-- | A value of type 'Bool' is encoded as follows in binary encoding.
--
-- @
-- 0 ==> False
-- 1 ==> True
-- @
--
-- /Internal/
--
{-# INLINE bool #-}
bool :: MonadCatch m => Parser m Word8 Bool
bool = PR.either word8ToBool

{-# INLINE word8ToOrdering #-}
word8ToOrdering :: Word8 -> Either String Ordering
word8ToOrdering 0 = Right LT
word8ToOrdering 1 = Right EQ
word8ToOrdering 2 = Right GT
word8ToOrdering w = Left ("Invalid Ordering encoding " ++ Prelude.show w)

-- | A value of type 'Ordering' is encoded as follows in binary encoding.
--
-- @
-- 0 ==> LT
-- 1 ==> EQ
-- 2 ==> GT
-- @
--
-- /Internal/
--
{-# INLINE ordering #-}
ordering :: MonadCatch m => Parser m Word8 Ordering
ordering = PR.either word8ToOrdering

-- XXX should go in a Word8 parser module?
-- | Accept the input byte only if it is equal to the specified value.
--
-- /Internal/
--
{-# INLINE eqWord8 #-}
eqWord8 :: MonadCatch m => Word8 -> Parser m Word8 Word8
eqWord8 b = PR.satisfy (== b)

-- | Accept any byte.
--
-- /Internal/
--
{-# INLINE word8 #-}
word8 :: MonadCatch m => Parser m Word8 Word8
word8 = PR.satisfy (const True)

-- | Big endian (MSB first) Word16
{-# INLINE word16beD #-}
word16beD :: MonadCatch m => PRD.Parser m Word8 Word16
word16beD = PRD.Parser step initial extract

    where

    initial = return Nothing

    step Nothing a =
        -- XXX We can use a non-failing parser or a fold so that we do not
        -- have to buffer for backtracking which is inefficient.
        return $ PRD.Continue 0 (Just (fromIntegral a `unsafeShiftL` 8))
    step (Just w) a =
        return $ PRD.Done 0 (w .|. fromIntegral a)

    extract Nothing = throwM $ PRD.ParseError "word16beD: end of input"
    extract (Just _) = error "word16beD: unreachable state"

-- | Parse two bytes as a 'Word16', the first byte is the MSB of the Word16 and
-- second byte is the LSB (big endian representation).
--
-- /Internal/
--
{-# INLINE word16be #-}
word16be :: MonadCatch m => Parser m Word8 Word16
word16be = PRK.toParserK word16beD
