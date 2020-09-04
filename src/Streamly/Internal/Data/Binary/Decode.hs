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
    , word16le
    , word32be
    , word32le
    , word64be
    , word64le
    , word64host
    )
where

import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits ((.|.), unsafeShiftL)
import Data.Word (Word8, Word16, Word32, Word64)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple' (..))

import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
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

    initial = return Nothing'

    step Nothing' a =
        -- XXX We can use a non-failing parser or a fold so that we do not
        -- have to buffer for backtracking which is inefficient.
        return $ PRD.Continue 0 (Just' (fromIntegral a `unsafeShiftL` 8))
    step (Just' w) a =
        return $ PRD.Done 0 (w .|. fromIntegral a)

    extract _ = throwM $ PRD.ParseError "word16be: end of input"

-- | Parse two bytes as a 'Word16', the first byte is the MSB of the Word16 and
-- second byte is the LSB (big endian representation).
--
-- /Internal/
--
{-# INLINE word16be #-}
word16be :: MonadCatch m => Parser m Word8 Word16
word16be = PRK.toParserK word16beD

-- | Little endian (LSB first) Word16
{-# INLINE word16leD #-}
word16leD :: MonadCatch m => PRD.Parser m Word8 Word16
word16leD = PRD.Parser step initial extract

    where

    initial = return Nothing'

    step Nothing' a =
        return $ PRD.Continue 0 (Just' (fromIntegral a))
    step (Just' w) a =
        return $ PRD.Done 0 (w .|. fromIntegral a `unsafeShiftL` 8)

    extract _ = throwM $ PRD.ParseError "word16le: end of input"

-- | Parse two bytes as a 'Word16', the first byte is the LSB of the Word16 and
-- second byte is the MSB (little endian representation).
--
-- /Internal/
--
{-# INLINE word16le #-}
word16le :: MonadCatch m => Parser m Word8 Word16
word16le = PRK.toParserK word16leD

-- | Big endian (MSB first) Word32
{-# INLINE word32beD #-}
word32beD :: MonadCatch m => PRD.Parser m Word8 Word32
word32beD = PRD.Parser step initial extract

    where

    initial = return $ Tuple' 0 24

    step (Tuple' w sh) a = return $
        if sh /= 0
        then
            let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
             in PRD.Continue 0 (Tuple' w1 (sh - 8))
        else PRD.Done 0 (w .|. fromIntegral a)

    extract _ = throwM $ PRD.ParseError "word32beD: end of input"

-- | Parse four bytes as a 'Word32', the first byte is the MSB of the Word32
-- and last byte is the LSB (big endian representation).
--
-- /Internal/
--
{-# INLINE word32be #-}
word32be :: MonadCatch m => Parser m Word8 Word32
word32be = PRK.toParserK word32beD

-- | Little endian (LSB first) Word32
{-# INLINE word32leD #-}
word32leD :: MonadCatch m => PRD.Parser m Word8 Word32
word32leD = PRD.Parser step initial extract

    where

    initial = return $ Tuple' 0 0

    step (Tuple' w sh) a = return $
        let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
         in if sh /= 24
            then PRD.Continue 0 (Tuple' w1 (sh + 8))
            else PRD.Done 0 w1

    extract _ = throwM $ PRD.ParseError "word32leD: end of input"

-- | Parse four bytes as a 'Word32', the first byte is the MSB of the Word32
-- and last byte is the LSB (big endian representation).
--
-- /Internal/
--
{-# INLINE word32le #-}
word32le :: MonadCatch m => Parser m Word8 Word32
word32le = PRK.toParserK word32leD

-- | Big endian (MSB first) Word64
{-# INLINE word64beD #-}
word64beD :: MonadCatch m => PRD.Parser m Word8 Word64
word64beD = PRD.Parser step initial extract

    where

    initial = return $ Tuple' 0 56

    step (Tuple' w sh) a = return $
        if sh /= 0
        then
            let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
             in PRD.Continue 0 (Tuple' w1 (sh - 8))
        else PRD.Done 0 (w .|. fromIntegral a)

    extract _ = throwM $ PRD.ParseError "word64beD: end of input"

-- | Parse eight bytes as a 'Word64', the first byte is the MSB of the Word64
-- and last byte is the LSB (big endian representation).
--
-- /Internal/
--
{-# INLINE word64be #-}
word64be :: MonadCatch m => Parser m Word8 Word64
word64be = PRK.toParserK word64beD

-- | Little endian (LSB first) Word64
{-# INLINE word64leD #-}
word64leD :: MonadCatch m => PRD.Parser m Word8 Word64
word64leD = PRD.Parser step initial extract

    where

    initial = return $ Tuple' 0 0

    step (Tuple' w sh) a = return $
        let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
         in if sh /= 56
            then PRD.Continue 0 (Tuple' w1 (sh + 8))
            else PRD.Done 0 w1

    extract _ = throwM $ PRD.ParseError "word64leD: end of input"

-- | Parse eight bytes as a 'Word64', the first byte is the MSB of the Word64
-- and last byte is the LSB (big endian representation).
--
-- /Internal/
--
{-# INLINE word64le #-}
word64le :: MonadCatch m => Parser m Word8 Word64
word64le = PRK.toParserK word64leD

-------------------------------------------------------------------------------
-- Host byte order
-------------------------------------------------------------------------------

-- | Parse eight bytes as a 'Word64' in the host byte order.
--
-- /Internal/
--
{-# INLINE word64host #-}
word64host :: (MonadIO m, MonadCatch m) => Parser m Word8 Word64
word64host =
    fmap (flip A.unsafeIndex 0 . A.unsafeCast) $ PR.takeEQ 8 (A.writeN 8)
