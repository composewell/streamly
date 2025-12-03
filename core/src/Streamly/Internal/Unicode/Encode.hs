{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Unicode.Encode
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Unicode.Encode
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- XXX Use to/from instead of encode/decode for more compact naming.

    -- * Elimination (Encoding)
    -- ** Latin1 Encoding to Byte Stream
      encodeLatin1
    , encodeLatin1'
    , encodeLatin1_

    -- ** UTF-8 Encoding to Byte Stream
    , readCharUtf8'
    , readCharUtf8
    , readCharUtf8_
    , encodeUtf8
    , encodeUtf8'
    , encodeUtf8_
    , encodeStrings

    -- ** UTF-8 Encoding to Chunk Stream
    -- , toUtf8Chunks
    -- , toUtf8Chunks'
    -- , toUtf8Chunks_
    -- , toUtf8ChunksEndByLn

    -- , toPinnedUtf8Chunks
    -- , toPinnedUtf8Chunks'
    -- , toPinnedUtf8Chunks_
    -- , toPinnedUtf8ChunksEndByLn

    -- ** UTF-16 Encoding to Byte Stream
    , encodeUtf16le'
    , encodeUtf16le

    -- * StreamD UTF8 Encoding / Decoding transformations.
    , encodeUtf8D
    , encodeUtf8D'
    , encodeUtf8D_

    -- * Word16 Utilities
    -- , swapByteOrder

    -- * Deprecations
    , encodeLatin1Lax
    , encodeUtf8Lax
    )
where

#include "inline.hs"

-- MachDeps.h includes ghcautoconf.h that defines WORDS_BIGENDIAN for big endian
-- systems.
#include "MachDeps.h"

import Control.Monad.IO.Class (MonadIO)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Char (chr, ord)
import Data.Word (Word8, Word16)
import GHC.Base (assert)
import GHC.IO.Encoding.Failure (isSurrogate)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Stream (Step (..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as D

#include "DocTestUnicodeStream.hs"

-------------------------------------------------------------------------------
-- Latin1 encoding
-------------------------------------------------------------------------------

-- | Encode a stream of Unicode characters to bytes by mapping each character
-- to a byte in 0-255 range. Throws an error if the input stream contains
-- characters beyond 255.
--
{-# INLINE encodeLatin1' #-}
encodeLatin1' :: Monad m => Stream m Char -> Stream m Word8
encodeLatin1' = fmap convert
    where
    convert c =
        let codepoint = ord c
        in if codepoint > 255
           then error $ "Streamly.Unicode.encodeLatin1 invalid " ++
                      "input char codepoint " ++ show codepoint
           else fromIntegral codepoint

-- XXX Should we instead replace the invalid chars by NUL or whitespace or some
-- other control char? That may affect the perf a bit but may be a better
-- behavior.
--
-- | Like 'encodeLatin1'' but silently maps input codepoints beyond 255 to
-- arbitrary Latin1 chars in 0-255 range. No error or exception is thrown when
-- such mapping occurs.
--
{-# INLINE encodeLatin1 #-}
encodeLatin1 :: Monad m => Stream m Char -> Stream m Word8
encodeLatin1 = fmap (fromIntegral . ord)

-- | Like 'encodeLatin1' but drops the input characters beyond 255.
--
{-# INLINE encodeLatin1_ #-}
encodeLatin1_ :: Monad m => Stream m Char -> Stream m Word8
encodeLatin1_ = fmap (fromIntegral . ord) . Stream.filter (<= chr 255)

-- | Same as 'encodeLatin1'
--
{-# DEPRECATED encodeLatin1Lax "Please use 'encodeLatin1' instead" #-}
{-# INLINE encodeLatin1Lax #-}
encodeLatin1Lax :: Monad m => Stream m Char -> Stream m Word8
encodeLatin1Lax = encodeLatin1

-------------------------------------------------------------------------------
-- UTF-8 decoding
-------------------------------------------------------------------------------

-- Int helps in cheaper conversion from Int to Char
type CodePoint = Int
type DecodeState = Word8

-- We can divide the errors in three general categories:
-- * A non-starter was encountered in a begin state
-- * A starter was encountered without completing a codepoint
-- * The last codepoint was not complete (input underflow)
--
-- Need to separate resumable and non-resumable error. In case of non-resumable
-- error we can also provide the failing byte. In case of resumable error the
-- state can be opaque.
--
data DecodeError = DecodeError !DecodeState !CodePoint deriving Show

-------------------------------------------------------------------------------
-- One shot decoding
-------------------------------------------------------------------------------

data CodingFailureMode
    = TransliterateCodingFailure
    | ErrorOnCodingFailure
    | DropOnCodingFailure
    deriving (Show)

-- XXX this is defined in both Encode/Decode modules.

-- | Swap the byte order of Word16
--
-- > swapByteOrder 0xABCD == 0xCDAB
-- > swapByteOrder . swapByteOrder == id
{-# INLINE _swapByteOrder #-}
_swapByteOrder :: Word16 -> Word16
_swapByteOrder w = (w `shiftL` 8) .|. (w `shiftR` 8)

-------------------------------------------------------------------------------
-- Encoding Unicode (UTF-8) Characters
-------------------------------------------------------------------------------

data WList a = WCons !a !(WList a) | WNil

-- UTF-8 primitives, Lifted from GHC.IO.Encoding.UTF8.

{-# INLINE ord2 #-}
ord2 :: Char -> (WList Word8)
ord2 c = assert (n >= 0x80 && n <= 0x07ff) (WCons x1 (WCons x2 WNil))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
    x2 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord3 #-}
ord3 :: Char -> (WList Word8)
ord3 c = assert (n >= 0x0800 && n <= 0xffff) (WCons x1 (WCons x2 (WCons x3 WNil)))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
    x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x3 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord4 #-}
ord4 :: Char -> (WList Word8)
ord4 c = assert (n >= 0x10000)  (WCons x1 (WCons x2 (WCons x3 (WCons x4 WNil))))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
    x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
    x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x4 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE_NORMAL readCharUtf8With #-}
readCharUtf8With :: Monad m => (WList Word8) -> Unfold m Char Word8
readCharUtf8With surr = Unfold step inject

    where

    inject c =
        return $ case ord c of
            x | x <= 0x7F -> fromIntegral x `WCons` WNil
              | x <= 0x7FF -> ord2 c
              | x <= 0xFFFF -> if isSurrogate c then surr else ord3 c
              | otherwise -> ord4 c

    {-# INLINE_LATE step #-}
    step WNil = return Stop
    step (WCons x xs) = return $ Yield x xs

{-# INLINE_NORMAL readCharUtf8' #-}
readCharUtf8' :: Monad m => Unfold m Char Word8
readCharUtf8' =
    readCharUtf8With $
        error "Streamly.Internal.Unicode.readCharUtf8': Encountered a surrogate"

-- More yield points improve performance, but I am not sure if they can cause
-- too much code bloat or some trouble with fusion. So keeping only two yield
-- points for now, one for the ascii chars (fast path) and one for all other
-- paths (slow path).
{-# INLINE_NORMAL encodeUtf8D' #-}
encodeUtf8D' :: Monad m => D.Stream m Char -> D.Stream m Word8
encodeUtf8D' = D.unfoldEach readCharUtf8'

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream. When
-- any invalid character (U+D800-U+D8FF) is encountered in the input stream the
-- function errors out.
--
{-# INLINE encodeUtf8' #-}
encodeUtf8' :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8' = encodeUtf8D'

{-# INLINE_NORMAL readCharUtf8 #-}
readCharUtf8 :: Monad m => Unfold m Char Word8
readCharUtf8 = readCharUtf8With $ WCons 239 (WCons 191 (WCons 189 WNil))

-- | See section "3.9 Unicode Encoding Forms" in
-- https://www.unicode.org/versions/Unicode13.0.0/UnicodeStandard-13.0.pdf
--
{-# INLINE_NORMAL encodeUtf8D #-}
encodeUtf8D :: Monad m => D.Stream m Char -> D.Stream m Word8
encodeUtf8D = D.unfoldEach readCharUtf8

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream. Any
-- Invalid characters (U+D800-U+D8FF) in the input stream are replaced by the
-- Unicode replacement character U+FFFD.
--
{-# INLINE encodeUtf8 #-}
encodeUtf8 :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8 = encodeUtf8D

{-# INLINE_NORMAL readCharUtf8_ #-}
readCharUtf8_ :: Monad m => Unfold m Char Word8
readCharUtf8_ = readCharUtf8With WNil

{-# INLINE_NORMAL encodeUtf8D_ #-}
encodeUtf8D_ :: Monad m => D.Stream m Char -> D.Stream m Word8
encodeUtf8D_ = D.unfoldEach readCharUtf8_

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream. Any
-- Invalid characters (U+D800-U+D8FF) in the input stream are dropped.
--
{-# INLINE encodeUtf8_ #-}
encodeUtf8_ :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8_ = encodeUtf8D_

-- | Same as 'encodeUtf8'
--
{-# DEPRECATED encodeUtf8Lax "Please use 'encodeUtf8' instead" #-}
{-# INLINE encodeUtf8Lax #-}
encodeUtf8Lax :: Monad m => Stream m Char -> Stream m Word8
encodeUtf8Lax = encodeUtf8

-------------------------------------------------------------------------------
-- Encoding to Utf16
-------------------------------------------------------------------------------

{-# INLINE utf16LowSurrogate #-}
utf16LowSurrogate :: Word16
utf16LowSurrogate = 0xDC00

{-# INLINE utf16HighSurrogate #-}
utf16HighSurrogate :: Word16
utf16HighSurrogate = 0xD800

{-# INLINE_NORMAL readCharUtf16With #-}
readCharUtf16With :: Monad m => WList Word16 -> Unfold m Char Word16
readCharUtf16With invalidReplacement = Unfold step inject

    where

    inject c =
        return $ case ord c of
            x | x < 0xD800 -> fromIntegral x `WCons` WNil
              | x > 0xDFFF && x <= 0xFFFF -> fromIntegral x `WCons` WNil
              | x >= 0x10000 && x <= 0x10FFFF ->
                    let u = x - 0x10000                         -- 20 bits
                        h = utf16HighSurrogate
                                + fromIntegral (u `shiftR` 10)  -- 10 bits
                        l = utf16LowSurrogate
                                + fromIntegral (u .&. 0x3FF)    -- 10 bits
                    in WCons h $ WCons l WNil
              | otherwise -> invalidReplacement

    {-# INLINE_LATE step #-}
    step WNil = return Stop
    step (WCons x xs) = return $ Yield x xs

{-# INLINE encodeUtf16' #-}
encodeUtf16' :: Monad m => Stream m Char -> Stream m Word16
encodeUtf16' = D.unfoldEach (readCharUtf16With errString)
    where
    errString =
        error
            $ "Streamly.Internal.Unicode.encodeUtf16': Encountered an \
               invalid character"

{-# INLINE encodeUtf16 #-}
encodeUtf16 :: Monad m => Stream m Char -> Stream m Word16
encodeUtf16 = D.unfoldEach (readCharUtf16With WNil)

-- | Similar to 'encodeUtf16le' but throws an error if any invalid character is
-- encountered.
--
{-# INLINE encodeUtf16le' #-}
encodeUtf16le' :: Monad m => Stream m Char -> Stream m Word16
encodeUtf16le' =
#ifdef WORDS_BIGENDIAN
    fmap _swapByteOrder .
#endif
        encodeUtf16'

-- | Encode a stream of Unicode characters to a UTF-16 encoded stream. Any
-- invalid characters in the input stream are replaced by the Unicode
-- replacement character U+FFFD.
--
-- The resulting Word16s are encoded in little-endian byte order.
--
{-# INLINE encodeUtf16le #-}
encodeUtf16le :: Monad m => Stream m Char -> Stream m Word16
encodeUtf16le =
#ifdef WORDS_BIGENDIAN
    fmap _swapByteOrder .
#endif
        encodeUtf16

-------------------------------------------------------------------------------
-- Encode streams of containers
-------------------------------------------------------------------------------

-- | Encode a container to @Array Word8@ provided an unfold to covert it to a
-- Char stream and an encoding function.
--
-- /Internal/
{-# INLINE encodeObject #-}
encodeObject :: MonadIO m =>
       (Stream m Char -> Stream m Word8)
    -> Unfold m a Char
    -> a
    -> m (Array Word8)
encodeObject encode u = Stream.fold Array.create . encode . Stream.unfold u

-- | Encode a stream of container objects using the supplied encoding scheme.
-- Each object is encoded as an @Array Word8@.
--
-- /Internal/
{-# INLINE encodeObjects #-}
encodeObjects :: MonadIO m =>
       (Stream m Char -> Stream m Word8)
    -> Unfold m a Char
    -> Stream m a
    -> Stream m (Array Word8)
encodeObjects encode u = Stream.mapM (encodeObject encode u)

-- | Encode a stream of 'String' using the supplied encoding scheme. Each
-- string is encoded as an @Array Word8@.
--
{-# INLINE encodeStrings #-}
encodeStrings :: MonadIO m =>
       (Stream m Char -> Stream m Word8)
    -> Stream m String
    -> Stream m (Array Word8)
encodeStrings encode = encodeObjects encode Unfold.fromList
