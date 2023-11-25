-- |
-- Module      : Streamly.Internal.Data.Binary.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Decode Haskell data types from byte streams.
--
-- It would be inefficient to use this to compose parsers for general algebraic
-- data types. For general deserialization of ADTs please use the Serialize
-- type class instances. The fastest way to deserialize byte streams
-- representing Haskell data types is to write them to arrays and deserialize
-- the array using the Serialize type class.

module Streamly.Internal.Data.Binary.Parser
    (
    -- * Type class
      FromBytes (..)

    -- * Decoders
    , unit
    , bool
    , ordering
    , eqWord8 -- XXX rename to word8Eq
    , word8
    , word16be
    , word16le
    , word32be
    , word32le
    , word64be
    , word64le
    , word64host
    , int8
    , int16be
    , int16le
    , int32be
    , int32le
    , int64be
    , int64le
    , float32be
    , float32le
    , double64be
    , double64le
    , charLatin1
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Bits ((.|.), unsafeShiftL)
import Data.Char (chr)
import Data.Int (Int8, Int16, Int32, Int64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Data.Word (Word8, Word16, Word32, Word64)
import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple' (..))
import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Data.Array as A
    (getIndexUnsafe, castUnsafe)
import qualified Streamly.Internal.Data.Parser as PR
    (fromPure, either, satisfy, takeEQ)
import qualified Streamly.Internal.Data.Parser as PRD
    (Parser(..), Initial(..), Step(..))

-- Note: The () type does not need to have an on-disk representation in theory.
-- But we use a concrete representation for it so that we count how many ()
-- types we have. Or when we have an array of units the array a concrete
-- length.

-- | A value of type '()' is encoded as @0@ in binary encoding.
--
-- @
-- 0 ==> ()
-- @
--
-- /Pre-release/
--
{-# INLINE unit #-}
unit :: Monad m => Parser Word8 m ()
unit = eqWord8 0 *> PR.fromPure ()

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
-- /Pre-release/
--
{-# INLINE bool #-}
bool :: Monad m => Parser Word8 m Bool
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
-- /Pre-release/
--
{-# INLINE ordering #-}
ordering :: Monad m => Parser Word8 m Ordering
ordering = PR.either word8ToOrdering

-- XXX should go in a Word8 parser module?
-- | Accept the input byte only if it is equal to the specified value.
--
-- /Pre-release/
--
{-# INLINE eqWord8 #-}
eqWord8 :: Monad m => Word8 -> Parser Word8 m Word8
eqWord8 b = PR.satisfy (== b)

-- | Accept any byte.
--
-- /Pre-release/
--
{-# INLINE word8 #-}
word8 :: Monad m => Parser Word8 m Word8
word8 = PR.satisfy (const True)

-- | Big endian (MSB first) Word16
{-# INLINE word16beD #-}
word16beD :: Monad m => PRD.Parser Word8 m Word16
word16beD = PRD.Parser step initial extract

    where

    initial = return $ PRD.IPartial Nothing'

    step Nothing' a =
        -- XXX We can use a non-failing parser or a fold so that we do not
        -- have to buffer for backtracking which is inefficient.
        return $ PRD.Continue 0 (Just' (fromIntegral a `unsafeShiftL` 8))
    step (Just' w) a =
        return $ PRD.Done 0 (w .|. fromIntegral a)

    extract _ = return $ PRD.Error "word16be: end of input"

-- | Parse two bytes as a 'Word16', the first byte is the MSB of the Word16 and
-- second byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word16be #-}
word16be :: Monad m => Parser Word8 m Word16
word16be = word16beD

-- | Little endian (LSB first) Word16
{-# INLINE word16leD #-}
word16leD :: Monad m => PRD.Parser Word8 m Word16
word16leD = PRD.Parser step initial extract

    where

    initial = return $ PRD.IPartial Nothing'

    step Nothing' a =
        return $ PRD.Continue 0 (Just' (fromIntegral a))
    step (Just' w) a =
        return $ PRD.Done 0 (w .|. fromIntegral a `unsafeShiftL` 8)

    extract _ = return $ PRD.Error "word16le: end of input"

-- | Parse two bytes as a 'Word16', the first byte is the LSB of the Word16 and
-- second byte is the MSB (little endian representation).
--
-- /Pre-release/
--
{-# INLINE word16le #-}
word16le :: Monad m => Parser Word8 m Word16
word16le = word16leD

-- | Big endian (MSB first) Word32
{-# INLINE word32beD #-}
word32beD :: Monad m => PRD.Parser Word8 m Word32
word32beD = PRD.Parser step initial extract

    where

    initial = return $ PRD.IPartial $ Tuple' 0 24

    step (Tuple' w sh) a = return $
        if sh /= 0
        then
            let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
             in PRD.Continue 0 (Tuple' w1 (sh - 8))
        else PRD.Done 0 (w .|. fromIntegral a)

    extract _ = return $ PRD.Error "word32beD: end of input"

-- | Parse four bytes as a 'Word32', the first byte is the MSB of the Word32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word32be #-}
word32be :: Monad m => Parser Word8 m Word32
word32be = word32beD

-- | Little endian (LSB first) Word32
{-# INLINE word32leD #-}
word32leD :: Monad m => PRD.Parser Word8 m Word32
word32leD = PRD.Parser step initial extract

    where

    initial = return $ PRD.IPartial $ Tuple' 0 0

    step (Tuple' w sh) a = return $
        let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
         in if sh /= 24
            then PRD.Continue 0 (Tuple' w1 (sh + 8))
            else PRD.Done 0 w1

    extract _ = return $ PRD.Error "word32leD: end of input"

-- | Parse four bytes as a 'Word32', the first byte is the MSB of the Word32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word32le #-}
word32le :: Monad m => Parser Word8 m Word32
word32le = word32leD

-- | Big endian (MSB first) Word64
{-# INLINE word64beD #-}
word64beD :: Monad m => PRD.Parser Word8 m Word64
word64beD = PRD.Parser step initial extract

    where

    initial = return $ PRD.IPartial $ Tuple' 0 56

    step (Tuple' w sh) a = return $
        if sh /= 0
        then
            let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
             in PRD.Continue 0 (Tuple' w1 (sh - 8))
        else PRD.Done 0 (w .|. fromIntegral a)

    extract _ = return $ PRD.Error "word64beD: end of input"

-- | Parse eight bytes as a 'Word64', the first byte is the MSB of the Word64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word64be #-}
word64be :: Monad m => Parser Word8 m Word64
word64be = word64beD

-- | Little endian (LSB first) Word64
{-# INLINE word64leD #-}
word64leD :: Monad m => PRD.Parser Word8 m Word64
word64leD = PRD.Parser step initial extract

    where

    initial = return $ PRD.IPartial $ Tuple' 0 0

    step (Tuple' w sh) a = return $
        let w1 = w .|. (fromIntegral a `unsafeShiftL` sh)
         in if sh /= 56
            then PRD.Continue 0 (Tuple' w1 (sh + 8))
            else PRD.Done 0 w1

    extract _ = return $ PRD.Error "word64leD: end of input"

-- | Parse eight bytes as a 'Word64', the first byte is the MSB of the Word64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word64le #-}
word64le :: Monad m => Parser Word8 m Word64
word64le = word64leD

{-# INLINE int8 #-}
int8 :: Monad m => Parser Word8 m Int8
int8 = fromIntegral <$> word8

-- | Parse two bytes as a 'Int16', the first byte is the MSB of the Int16 and
-- second byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int16be #-}
int16be :: Monad m => Parser Word8 m Int16
int16be = fromIntegral <$> word16be

-- | Parse two bytes as a 'Int16', the first byte is the LSB of the Int16 and
-- second byte is the MSB (little endian representation).
--
-- /Pre-release/
--
{-# INLINE int16le #-}
int16le :: Monad m => Parser Word8 m Int16
int16le = fromIntegral <$> word16le

-- | Parse four bytes as a 'Int32', the first byte is the MSB of the Int32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int32be #-}
int32be :: Monad m => Parser Word8 m Int32
int32be = fromIntegral <$> word32be

-- | Parse four bytes as a 'Int32', the first byte is the MSB of the Int32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int32le #-}
int32le :: Monad m => Parser Word8 m Int32
int32le = fromIntegral <$> word32le

-- | Parse eight bytes as a 'Int64', the first byte is the MSB of the Int64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int64be #-}
int64be :: Monad m => Parser Word8 m Int64
int64be = fromIntegral <$> word64be

-- | Parse eight bytes as a 'Int64', the first byte is the MSB of the Int64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int64le #-}
int64le :: Monad m => Parser Word8 m Int64
int64le = fromIntegral <$> word64le

{-# INLINE float32be #-}
float32be :: MonadIO m => Parser Word8 m Float
float32be = castWord32ToFloat <$> word32be

{-# INLINE float32le #-}
float32le :: MonadIO m => Parser Word8 m Float
float32le = castWord32ToFloat <$> word32le

{-# INLINE double64be #-}
double64be :: MonadIO m => Parser Word8 m Double
double64be =  castWord64ToDouble <$> word64be

{-# INLINE double64le #-}
double64le :: MonadIO m => Parser Word8 m Double
double64le = castWord64ToDouble <$> word64le

-- | Accept any byte.
--
-- /Pre-release/
--
{-# INLINE charLatin1 #-}
charLatin1 :: Monad m => Parser Word8 m Char
charLatin1 = fmap (chr . fromIntegral) word8

-------------------------------------------------------------------------------
-- Host byte order
-------------------------------------------------------------------------------

-- | Parse eight bytes as a 'Word64' in the host byte order.
--
-- /Pre-release/
--
{-# INLINE word64host #-}
word64host :: MonadIO m => Parser Word8 m Word64
word64host =
    fmap (A.getIndexUnsafe 0 . A.castUnsafe) $ PR.takeEQ 8 (A.writeN 8)

-------------------------------------------------------------------------------
-- Type class
-------------------------------------------------------------------------------

class FromBytes a where
    -- | Decode a byte stream to a Haskell type.
    fromBytes :: Parser Word8 m a
