-- |
-- Module      : Streamly.Internal.Unicode.Stream
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Bjoern Hoehrmann 2008-2009
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Unicode.Stream
    (
    -- * Construction (Decoding)
      decodeLatin1

    -- ** UTF-8 Decoding
    , CodingFailureMode(..)
    , writeCharUtf8'
    , parseCharUtf8With
    , decodeUtf8
    , decodeUtf8'
    , decodeUtf8_

    -- ** Resumable UTF-8 Decoding
    , DecodeError(..)
    , DecodeState
    , CodePoint
    , decodeUtf8Either
    , resumeDecodeUtf8Either

    -- ** UTF-8 Array Stream Decoding
    , decodeUtf8Chunks
    , decodeUtf8Chunks'
    , decodeUtf8Chunks_

    -- * Elimination (Encoding)
    -- ** Latin1 Encoding
    , encodeLatin1
    , encodeLatin1'
    , encodeLatin1_

    -- ** UTF-8 Encoding
    , readCharUtf8'
    , readCharUtf8
    , readCharUtf8_
    , encodeUtf8
    , encodeUtf8'
    , encodeUtf8_
    , encodeStrings
    {-
    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd
    -}

    -- * Transformation
    , stripHead
    , lines
    , words
    , unlines
    , unwords

    -- * StreamD UTF8 Encoding / Decoding transformations.
    , decodeUtf8D
    , decodeUtf8D'
    , decodeUtf8D_
    , encodeUtf8D
    , encodeUtf8D'
    , encodeUtf8D_
    , decodeUtf8EitherD
    , resumeDecodeUtf8EitherD

    -- * Decoding String Literals
    , fromStr#

    -- * Deprecations
    , decodeUtf8Lax
    , encodeLatin1Lax
    , encodeUtf8Lax
    )
where

#include "inline.hs"

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Char (chr, ord)
#if MIN_VERSION_base(4,17,0)
import Data.Char (generalCategory, GeneralCategory(Space))
#endif
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Storable (Storable(..))
#ifndef __GHCJS__
import Fusion.Plugin.Types (Fuse(..))
#endif
import GHC.Base (assert, unsafeChr)
import GHC.Exts (Addr#)
import GHC.IO.Encoding.Failure (isSurrogate)
import GHC.Ptr (Ptr (..), plusPtr)
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Array.Mut.Type (MutableByteArray)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Stream (Step (..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unbox (Unbox(peekByteIndex))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Parser as Parser (Parser)
import qualified Streamly.Internal.Data.Parser as ParserD
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as D

import Prelude hiding (lines, words, unlines, unwords)

-- $setup
-- >>> :m
-- >>> :set -XMagicHash
-- >>> import Prelude hiding (lines, words, unlines, unwords)
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Unicode.Stream as Unicode
-- >>> import Streamly.Internal.Unicode.Stream

-------------------------------------------------------------------------------
-- Latin1 decoding
-------------------------------------------------------------------------------

-- | Decode a stream of bytes to Unicode characters by mapping each byte to a
-- corresponding Unicode 'Char' in 0-255 range.
--
{-# INLINE decodeLatin1 #-}
decodeLatin1 :: Monad m => Stream m Word8 -> Stream m Char
decodeLatin1 = fmap (unsafeChr . fromIntegral)

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

-- See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

-- XXX Use names decodeSuccess = 0, decodeFailure = 12

decodeTable :: [Word8]
decodeTable = [
   -- The first part of the table maps bytes to character classes that
   -- to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

   -- The second part is a transition table that maps a combination
   -- of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12
  ]

{-# INLINE utf8dLength #-}
utf8dLength :: Int
utf8dLength = length decodeTable

-- | We do not want to garbage collect this and free the memory, we want to
-- keep this persistent. We don't know how to do that with GHC without having a
-- reference in some global structure. So we use a hack, use mallocBytes so
-- that the GC has no way to free it.
{-# NOINLINE utf8d #-}
utf8d :: Ptr Word8
utf8d = unsafePerformIO $ do
    let size = utf8dLength
    p <- liftIO $ mallocBytes size
    void $ D.fold
        (Fold.foldlM' (\b a -> poke b a >> return (b `plusPtr` 1)) (return p))
        (D.fromList decodeTable)
    return p

-- | Return element at the specified index without checking the bounds.
-- and without touching the foreign ptr.
{-# INLINE_NORMAL unsafePeekElemOff #-}
unsafePeekElemOff :: forall a. Storable a => Ptr a -> Int -> a
unsafePeekElemOff p i =
    let !x = unsafeInlineIO $ peekElemOff p i
     in x

-- XXX We can use a fromPtr stream to implement it.
{-# INLINE showMemory #-}
showMemory ::
    forall a. (Show a, Storable a) => Ptr a -> Ptr a -> String
showMemory cur end
    | cur < end =
        let cur1 = cur `plusPtr` sizeOf (undefined :: a)
         in show (unsafeInlineIO $ peek cur) ++ " " ++ showMemory cur1 end
showMemory _ _ = ""

-- decode is split into two separate cases to avoid branching instructions.
-- From the higher level flow we already know which case we are in so we can
-- call the appropriate decode function.
--
-- When the state is 0
{-# INLINE decode0 #-}
decode0 :: Ptr Word8 -> Word8 -> Tuple' DecodeState CodePoint
decode0 table byte =
    let !t = table `unsafePeekElemOff` fromIntegral byte
        !codep' = (0xff `shiftR` fromIntegral t) .&. fromIntegral byte
        !state' = table `unsafePeekElemOff` (256 + fromIntegral t)
     in assert ((byte > 0x7f || error showByte)
                && (state' /= 0 || error (showByte ++ showTable)))
               (Tuple' state' codep')

    where

    utf8tableEnd = table `plusPtr` 364
    showByte = "Streamly: decode0: byte: " ++ show byte
    showTable = " table: " ++ showMemory table utf8tableEnd

-- When the state is not 0
{-# INLINE decode1 #-}
decode1
    :: Ptr Word8
    -> DecodeState
    -> CodePoint
    -> Word8
    -> Tuple' DecodeState CodePoint
decode1 table state codep byte =
    -- Remember codep is Int type!
    -- Can it be unsafe to convert the resulting Int to Char?
    let !t = table `unsafePeekElemOff` fromIntegral byte
        !codep' = (fromIntegral byte .&. 0x3f) .|. (codep `shiftL` 6)
        !state' = table `unsafePeekElemOff`
                    (256 + fromIntegral state + fromIntegral t)
     in assert (codep' <= 0x10FFFF
                    || error (showByte ++ showState state codep))
               (Tuple' state' codep')
    where

    utf8tableEnd = table `plusPtr` 364
    showByte = "Streamly: decode1: byte: " ++ show byte
    showState st cp =
        " state: " ++ show st ++
        " codepoint: " ++ show cp ++
        " table: " ++ showMemory table utf8tableEnd

-------------------------------------------------------------------------------
-- Resumable UTF-8 decoding
-------------------------------------------------------------------------------

-- Strangely, GHCJS hangs linking template-haskell with this
#ifndef __GHCJS__
{-# ANN type UTF8DecodeState Fuse #-}
#endif
data UTF8DecodeState s a
    = UTF8DecodeInit s
    | UTF8DecodeInit1 s Word8
    | UTF8DecodeFirst s Word8
    | UTF8Decoding s !DecodeState !CodePoint
    | YieldAndContinue a (UTF8DecodeState s a)
    | Done

{-# INLINE_NORMAL resumeDecodeUtf8EitherD #-}
resumeDecodeUtf8EitherD
    :: Monad m
    => DecodeState
    -> CodePoint
    -> D.Stream m Word8
    -> D.Stream m (Either DecodeError Char)
resumeDecodeUtf8EitherD dst codep (D.Stream step state) =
    let stt =
            if dst == 0
            then UTF8DecodeInit state
            else UTF8Decoding state dst codep
    in D.Stream (step' utf8d) stt
  where
    {-# INLINE_LATE step' #-}
    step' _ gst (UTF8DecodeInit st) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (UTF8DecodeInit1 s x)
            Skip s -> Skip (UTF8DecodeInit s)
            Stop   -> Skip Done

    step' _ _ (UTF8DecodeInit1 st x) = do
        -- Note: It is important to use a ">" instead of a "<=" test
        -- here for GHC to generate code layout for default branch
        -- prediction for the common case. This is fragile and might
        -- change with the compiler versions, we need a more reliable
        -- "likely" primitive to control branch predication.
        case x > 0x7f of
            False ->
                return $ Skip $ YieldAndContinue
                    (Right $ unsafeChr (fromIntegral x))
                    (UTF8DecodeInit st)
            -- Using a separate state here generates a jump to a
            -- separate code block in the core which seems to perform
            -- slightly better for the non-ascii case.
            True -> return $ Skip $ UTF8DecodeFirst st x

    -- XXX should we merge it with UTF8DecodeInit1?
    step' table _ (UTF8DecodeFirst st x) = do
        let (Tuple' sv cp) = decode0 table x
        return $
            case sv of
                12 ->
                    Skip $ YieldAndContinue (Left $ DecodeError 0 (fromIntegral x))
                                            (UTF8DecodeInit st)
                0 -> error "unreachable state"
                _ -> Skip (UTF8Decoding st sv cp)

    -- We recover by trying the new byte x a starter of a new codepoint.
    -- XXX on error need to report the next byte "x" as well.
    -- XXX need to use the same recovery in array decoding routine as well
    step' table gst (UTF8Decoding st statePtr codepointPtr) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
                return $
                    case sv of
                        0 -> Skip $ YieldAndContinue (Right $ unsafeChr cp)
                                        (UTF8DecodeInit s)
                        12 ->
                            Skip $ YieldAndContinue (Left $ DecodeError statePtr codepointPtr)
                                        (UTF8DecodeInit1 s x)
                        _ -> Skip (UTF8Decoding s sv cp)
            Skip s -> return $ Skip (UTF8Decoding s statePtr codepointPtr)
            Stop -> return $ Skip $ YieldAndContinue (Left $ DecodeError statePtr codepointPtr) Done

    step' _ _ (YieldAndContinue c s) = return $ Yield c s
    step' _ _ Done = return Stop

-- XXX We can use just one API, and define InitState = 0 and InitCodePoint = 0
-- to use as starting state.
--
{-# INLINE_NORMAL decodeUtf8EitherD #-}
decodeUtf8EitherD :: Monad m
    => D.Stream m Word8 -> D.Stream m (Either DecodeError Char)
decodeUtf8EitherD = resumeDecodeUtf8EitherD 0 0

-- |
--
-- /Pre-release/
{-# INLINE decodeUtf8Either #-}
decodeUtf8Either :: Monad m
    => Stream m Word8 -> Stream m (Either DecodeError Char)
decodeUtf8Either = decodeUtf8EitherD

-- |
--
-- /Pre-release/
{-# INLINE resumeDecodeUtf8Either #-}
resumeDecodeUtf8Either
    :: Monad m
    => DecodeState
    -> CodePoint
    -> Stream m Word8
    -> Stream m (Either DecodeError Char)
resumeDecodeUtf8Either = resumeDecodeUtf8EitherD

-------------------------------------------------------------------------------
-- One shot decoding
-------------------------------------------------------------------------------

data CodingFailureMode
    = TransliterateCodingFailure
    | ErrorOnCodingFailure
    | DropOnCodingFailure
    deriving (Show)

{-# INLINE replacementChar #-}
replacementChar :: Char
replacementChar = '\xFFFD'

data UTF8CharDecodeState a
    = UTF8CharDecodeInit
    | UTF8CharDecoding !DecodeState !CodePoint

{-# INLINE parseCharUtf8WithD #-}
parseCharUtf8WithD ::
       Monad m => CodingFailureMode -> ParserD.Parser Word8 m Char
parseCharUtf8WithD cfm = ParserD.Parser (step' utf8d) initial extract

    where

    prefix = "Streamly.Internal.Data.Stream.parseCharUtf8WithD:"

    {-# INLINE initial #-}
    initial = return $ ParserD.IPartial UTF8CharDecodeInit

    handleError err souldBackTrack =
        case cfm of
            ErrorOnCodingFailure -> ParserD.Error err
            TransliterateCodingFailure ->
                case souldBackTrack of
                    True -> ParserD.Done 1 replacementChar
                    False -> ParserD.Done 0 replacementChar
            DropOnCodingFailure ->
                case souldBackTrack of
                    True -> ParserD.Continue 1 UTF8CharDecodeInit
                    False -> ParserD.Continue 0 UTF8CharDecodeInit

    {-# INLINE step' #-}
    step' table UTF8CharDecodeInit x =
        -- Note: It is important to use a ">" instead of a "<=" test
        -- here for GHC to generate code layout for default branch
        -- prediction for the common case. This is fragile and might
        -- change with the compiler versions, we need a more reliable
        -- "likely" primitive to control branch predication.
        return $ case x > 0x7f of
            False -> ParserD.Done 0 $ unsafeChr $ fromIntegral x
            True ->
                let (Tuple' sv cp) = decode0 table x
                 in case sv of
                        12 ->
                            let msg = prefix
                                    ++ "Invalid first UTF8 byte" ++ show x
                             in handleError msg False
                        0 -> error $ prefix ++ "unreachable state"
                        _ -> ParserD.Continue 0 (UTF8CharDecoding sv cp)

    step' table (UTF8CharDecoding statePtr codepointPtr) x = return $
        let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
         in case sv of
            0 -> ParserD.Done 0 $ unsafeChr cp
            12 ->
                let msg = prefix
                        ++ "Invalid subsequent UTF8 byte"
                        ++ show x
                        ++ "in state"
                        ++ show statePtr
                        ++ "accumulated value"
                        ++ show codepointPtr
                 in handleError msg True
            _ -> ParserD.Continue 0 (UTF8CharDecoding sv cp)

    {-# INLINE extract #-}
    extract UTF8CharDecodeInit =  error $ prefix ++ "Not enough input"
    extract (UTF8CharDecoding _ _) =
        case cfm of
            ErrorOnCodingFailure ->
                return $ ParserD.Error $ prefix ++ "Not enough input"
            TransliterateCodingFailure ->
                return (ParserD.Done 0 replacementChar)
            -- XXX We shouldn't error out here. There is no way to represent an
            -- empty parser result unless we return a "Maybe" type.
            DropOnCodingFailure -> error $ prefix ++ "Not enough input"

-- XXX This should ideally accept a "CodingFailureMode" and perform appropriate
-- error handling. This isn't possible now as "TransliterateCodingFailure"'s
-- workflow requires backtracking 1 element. This can be revisited once "Fold"
-- supports backtracking.
{-# INLINE writeCharUtf8' #-}
writeCharUtf8' :: Monad m => Fold m Word8 Char
writeCharUtf8' =  ParserD.toFold (parseCharUtf8WithD ErrorOnCodingFailure)

-- XXX The initial idea was to have "parseCharUtf8" and offload the error
-- handling to another parser. So, say we had "parseCharUtf8'",
--
-- >>> parseCharUtf8Smart = parseCharUtf8' <|> Parser.fromPure replacementChar
--
-- But unfortunately parseCharUtf8Smart used in conjunction with "parseMany" -
-- that is "parseMany parseCharUtf8Smart" on a stream causes the heap to
-- overflow. Even a heap size of 500 MB was not sufficient.
--
-- This needs to be investigated futher.
{-# INLINE parseCharUtf8With #-}
parseCharUtf8With ::
       Monad m => CodingFailureMode -> Parser.Parser Word8 m Char
parseCharUtf8With = parseCharUtf8WithD

-- XXX write it as a parser and use parseMany to decode a stream, need to check
-- if that preserves the same performance. Or we can use a resumable parser
-- that parses a chunk at a time.
--
-- XXX Implement this in terms of decodeUtf8Either. Need to make sure that
-- decodeUtf8Either preserves the performance characterstics.
--
{-# INLINE_NORMAL decodeUtf8WithD #-}
decodeUtf8WithD :: Monad m
    => CodingFailureMode -> D.Stream m Word8 -> D.Stream m Char
decodeUtf8WithD cfm (D.Stream step state) =
    D.Stream (step' utf8d) (UTF8DecodeInit state)

    where

    prefix = "Streamly.Internal.Data.Stream.StreamD.decodeUtf8With: "

    {-# INLINE handleError #-}
    handleError e s =
        case cfm of
            ErrorOnCodingFailure -> error e
            TransliterateCodingFailure -> YieldAndContinue replacementChar s
            DropOnCodingFailure -> s

    {-# INLINE handleUnderflow #-}
    handleUnderflow =
        case cfm of
            ErrorOnCodingFailure -> error $ prefix ++ "Not enough input"
            TransliterateCodingFailure -> YieldAndContinue replacementChar Done
            DropOnCodingFailure -> Done

    {-# INLINE_LATE step' #-}
    step' _ gst (UTF8DecodeInit st) = do
        r <- step (adaptState gst) st
        return $ case r of
            Yield x s -> Skip (UTF8DecodeInit1 s x)
            Skip s -> Skip (UTF8DecodeInit s)
            Stop   -> Skip Done

    step' _ _ (UTF8DecodeInit1 st x) = do
        -- Note: It is important to use a ">" instead of a "<=" test
        -- here for GHC to generate code layout for default branch
        -- prediction for the common case. This is fragile and might
        -- change with the compiler versions, we need a more reliable
        -- "likely" primitive to control branch predication.
        case x > 0x7f of
            False ->
                return $ Skip $ YieldAndContinue
                    (unsafeChr (fromIntegral x))
                    (UTF8DecodeInit st)
            -- Using a separate state here generates a jump to a
            -- separate code block in the core which seems to perform
            -- slightly better for the non-ascii case.
            True -> return $ Skip $ UTF8DecodeFirst st x

    -- XXX should we merge it with UTF8DecodeInit1?
    step' table _ (UTF8DecodeFirst st x) = do
        let (Tuple' sv cp) = decode0 table x
        return $
            case sv of
                12 ->
                    let msg = prefix ++ "Invalid first UTF8 byte " ++ show x
                     in Skip $ handleError msg (UTF8DecodeInit st)
                0 -> error "unreachable state"
                _ -> Skip (UTF8Decoding st sv cp)

    -- We recover by trying the new byte x as a starter of a new codepoint.
    -- XXX need to use the same recovery in array decoding routine as well
    step' table gst (UTF8Decoding st statePtr codepointPtr) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
                return $ case sv of
                    0 -> Skip $ YieldAndContinue
                            (unsafeChr cp) (UTF8DecodeInit s)
                    12 ->
                        let msg = prefix
                                ++ "Invalid subsequent UTF8 byte "
                                ++ show x
                                ++ " in state "
                                ++ show statePtr
                                ++ " accumulated value "
                                ++ show codepointPtr
                         in Skip $ handleError msg (UTF8DecodeInit1 s x)
                    _ -> Skip (UTF8Decoding s sv cp)
            Skip s -> return $
                Skip (UTF8Decoding s statePtr codepointPtr)
            Stop -> return $ Skip handleUnderflow

    step' _ _ (YieldAndContinue c s) = return $ Yield c s
    step' _ _ Done = return Stop

{-# INLINE decodeUtf8D #-}
decodeUtf8D :: Monad m => D.Stream m Word8 -> D.Stream m Char
decodeUtf8D = decodeUtf8WithD TransliterateCodingFailure

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- Any invalid codepoint encountered is replaced with the unicode replacement
-- character.
--
{-# INLINE decodeUtf8 #-}
decodeUtf8 :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8 = decodeUtf8D

{-# INLINE decodeUtf8D' #-}
decodeUtf8D' :: Monad m => D.Stream m Word8 -> D.Stream m Char
decodeUtf8D' = decodeUtf8WithD ErrorOnCodingFailure

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- The function throws an error if an invalid codepoint is encountered.
--
{-# INLINE decodeUtf8' #-}
decodeUtf8' :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8' = decodeUtf8D'

{-# INLINE decodeUtf8D_ #-}
decodeUtf8D_ :: Monad m => D.Stream m Word8 -> D.Stream m Char
decodeUtf8D_ = decodeUtf8WithD DropOnCodingFailure

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- Any invalid codepoint encountered is dropped.
--
{-# INLINE decodeUtf8_ #-}
decodeUtf8_ :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8_ = decodeUtf8D_

-- | Same as 'decodeUtf8'
--
{-# DEPRECATED decodeUtf8Lax "Please use 'decodeUtf8' instead" #-}
{-# INLINE decodeUtf8Lax #-}
decodeUtf8Lax :: Monad m => Stream m Word8 -> Stream m Char
decodeUtf8Lax = decodeUtf8

-------------------------------------------------------------------------------
-- Decoding Array Streams
-------------------------------------------------------------------------------

#ifndef __GHCJS__
{-# ANN type FlattenState Fuse #-}
#endif
data FlattenState s
    = OuterLoop s !(Maybe (DecodeState, CodePoint))
    | InnerLoopDecodeInit s MutableByteArray !Int !Int
    | InnerLoopDecodeFirst s MutableByteArray !Int !Int Word8
    | InnerLoopDecoding s MutableByteArray !Int !Int
        !DecodeState !CodePoint
    | YAndC !Char (FlattenState s)   -- These constructors can be
                                     -- encoded in the UTF8DecodeState
                                     -- type, I prefer to keep these
                                     -- flat even though that means
                                     -- coming up with new names
    | D

-- The normal decodeUtf8 above should fuse with flattenArrays
-- to create this exact code but it doesn't for some reason, as of now this
-- remains the fastest way I could figure out to decodeUtf8.
--
-- XXX Add Proper error messages
{-# INLINE_NORMAL decodeUtf8ArraysWithD #-}
decodeUtf8ArraysWithD ::
       MonadIO m
    => CodingFailureMode
    -> D.Stream m (Array Word8)
    -> D.Stream m Char
decodeUtf8ArraysWithD cfm (D.Stream step state) =
    D.Stream (step' utf8d) (OuterLoop state Nothing)
  where
    {-# INLINE transliterateOrError #-}
    transliterateOrError e s =
        case cfm of
            ErrorOnCodingFailure -> error e
            TransliterateCodingFailure -> YAndC replacementChar s
            DropOnCodingFailure -> s
    {-# INLINE inputUnderflow #-}
    inputUnderflow =
        case cfm of
            ErrorOnCodingFailure ->
                error $
                show "Streamly.Internal.Data.Stream.StreamD."
                ++ "decodeUtf8ArraysWith: Input Underflow"
            TransliterateCodingFailure -> YAndC replacementChar D
            DropOnCodingFailure -> D
    {-# INLINE_LATE step' #-}
    step' _ gst (OuterLoop st Nothing) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield Array {..} s ->
                     Skip (InnerLoopDecodeInit s arrContents arrStart arrEnd)
                Skip s -> Skip (OuterLoop s Nothing)
                Stop -> Skip D
    step' _ gst (OuterLoop st dst@(Just (ds, cp))) = do
        r <- step (adaptState gst) st
        return $
            case r of
                Yield Array {..} s ->
                     Skip (InnerLoopDecoding s arrContents arrStart arrEnd ds cp)
                Skip s -> Skip (OuterLoop s dst)
                Stop -> Skip inputUnderflow
    step' _ _ (InnerLoopDecodeInit st _ p end)
        | p == end = do
            return $ Skip $ OuterLoop st Nothing
    step' _ _ (InnerLoopDecodeInit st contents p end) = do
        x <- liftIO $ peekByteIndex p contents
        -- Note: It is important to use a ">" instead of a "<=" test here for
        -- GHC to generate code layout for default branch prediction for the
        -- common case. This is fragile and might change with the compiler
        -- versions, we need a more reliable "likely" primitive to control
        -- branch predication.
        case x > 0x7f of
            False ->
                return $ Skip $ YAndC
                    (unsafeChr (fromIntegral x))
                    (InnerLoopDecodeInit st contents (p + 1) end)
            -- Using a separate state here generates a jump to a separate code
            -- block in the core which seems to perform slightly better for the
            -- non-ascii case.
            True -> return $ Skip $ InnerLoopDecodeFirst st contents p end x

    step' table _ (InnerLoopDecodeFirst st contents p end x) = do
        let (Tuple' sv cp) = decode0 table x
        return $
            case sv of
                12 ->
                    Skip $
                    transliterateOrError
                        (
                           "Streamly.Internal.Data.Stream.StreamD."
                        ++ "decodeUtf8ArraysWith: Invalid UTF8"
                        ++ " codepoint encountered"
                        )
                        (InnerLoopDecodeInit st contents (p + 1) end)
                0 -> error "unreachable state"
                _ -> Skip (InnerLoopDecoding st contents (p + 1) end sv cp)
    step' _ _ (InnerLoopDecoding st _ p end sv cp)
        | p == end = return $ Skip $ OuterLoop st (Just (sv, cp))
    step' table _ (InnerLoopDecoding st contents p end statePtr codepointPtr) = do
        x <- liftIO $ peekByteIndex p contents
        let (Tuple' sv cp) = decode1 table statePtr codepointPtr x
        return $
            case sv of
                0 ->
                    Skip $
                    YAndC
                        (unsafeChr cp)
                        (InnerLoopDecodeInit st contents (p + 1) end)
                12 ->
                    Skip $
                    transliterateOrError
                        (
                           "Streamly.Internal.Data.Stream.StreamD."
                        ++ "decodeUtf8ArraysWith: Invalid UTF8"
                        ++ " codepoint encountered"
                        )
                        (InnerLoopDecodeInit st contents (p + 1) end)
                _ ->
                    Skip
                    (InnerLoopDecoding st contents (p + 1) end sv cp)
    step' _ _ (YAndC c s) = return $ Yield c s
    step' _ _ D = return Stop

-- | Like 'decodeUtf8' but for a chunked stream. It may be slightly faster than
-- flattening the stream and then decoding with 'decodeUtf8'.
{-# INLINE decodeUtf8Chunks #-}
decodeUtf8Chunks ::
       MonadIO m
    => D.Stream m (Array Word8)
    -> D.Stream m Char
decodeUtf8Chunks = decodeUtf8ArraysWithD TransliterateCodingFailure

-- | Like 'decodeUtf8\'' but for a chunked stream. It may be slightly faster
-- than flattening the stream and then decoding with 'decodeUtf8\''.
{-# INLINE decodeUtf8Chunks' #-}
decodeUtf8Chunks' ::
       MonadIO m
    => D.Stream m (Array Word8)
    -> D.Stream m Char
decodeUtf8Chunks' = decodeUtf8ArraysWithD ErrorOnCodingFailure

-- | Like 'decodeUtf8_' but for a chunked stream. It may be slightly faster
-- than flattening the stream and then decoding with 'decodeUtf8_'.
{-# INLINE decodeUtf8Chunks_ #-}
decodeUtf8Chunks_ ::
       MonadIO m
    => D.Stream m (Array Word8)
    -> D.Stream m Char
decodeUtf8Chunks_ = decodeUtf8ArraysWithD DropOnCodingFailure

-------------------------------------------------------------------------------
-- Encoding Unicode (UTF-8) Characters
-------------------------------------------------------------------------------

data WList = WCons !Word8 !WList | WNil

-- UTF-8 primitives, Lifted from GHC.IO.Encoding.UTF8.

{-# INLINE ord2 #-}
ord2 :: Char -> WList
ord2 c = assert (n >= 0x80 && n <= 0x07ff) (WCons x1 (WCons x2 WNil))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
    x2 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord3 #-}
ord3 :: Char -> WList
ord3 c = assert (n >= 0x0800 && n <= 0xffff) (WCons x1 (WCons x2 (WCons x3 WNil)))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
    x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x3 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE ord4 #-}
ord4 :: Char -> WList
ord4 c = assert (n >= 0x10000)  (WCons x1 (WCons x2 (WCons x3 (WCons x4 WNil))))
  where
    n = ord c
    x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
    x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
    x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    x4 = fromIntegral $ (n .&. 0x3F) + 0x80

{-# INLINE_NORMAL readCharUtf8With #-}
readCharUtf8With :: Monad m => WList -> Unfold m Char Word8
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
encodeUtf8D' = D.unfoldMany readCharUtf8'

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
encodeUtf8D = D.unfoldMany readCharUtf8

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
encodeUtf8D_ = D.unfoldMany readCharUtf8_

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
-- Decoding string literals
-------------------------------------------------------------------------------

-- | Read UTF-8 encoded bytes as chars from an 'Addr#' until a 0 byte is
-- encountered, the 0 byte is not included in the stream.
--
-- /Unsafe:/ The caller is responsible for safe addressing.
--
-- Note that this is completely safe when reading from Haskell string
-- literals because they are guaranteed to be NULL terminated:
--
-- >>> Stream.fold Fold.toList (Unicode.fromStr# "Haskell"#)
-- "Haskell"
--
{-# INLINE fromStr# #-}
fromStr# :: MonadIO m => Addr# -> Stream m Char
fromStr# addr = decodeUtf8 $ Stream.fromByteStr# addr

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
encodeObject encode u = Stream.fold Array.write . encode . Stream.unfold u

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

{-
-------------------------------------------------------------------------------
-- Utility operations on strings
-------------------------------------------------------------------------------

strip :: IsStream t => Stream m Char -> Stream m Char
strip = undefined

stripTail :: IsStream t => Stream m Char -> Stream m Char
stripTail = undefined
-}

-- | Remove leading whitespace from a string.
--
-- > stripHead = Stream.dropWhile isSpace
--
-- /Pre-release/
{-# INLINE stripHead #-}
stripHead :: Monad m => Stream m Char -> Stream m Char
stripHead = Stream.dropWhile isSpace

-- | Fold each line of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>> Stream.fold Fold.toList $ lines Fold.toList (Stream.fromList "lines\nthis\nstring\n\n\n")
-- ["lines","this","string","",""]
--
-- > lines = Stream.splitOnSuffix (== '\n')
--
-- /Pre-release/
{-# INLINE lines #-}
lines :: Monad m => Fold m Char b -> Stream m Char -> Stream m b
lines f = Stream.foldMany (Fold.takeEndBy_ (== '\n') f)

#if !MIN_VERSION_base(4,17,0)
foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int
#endif

-- | Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
#if MIN_VERSION_base(4,17,0)
  | otherwise = generalCategory c == Space
#else
  | otherwise = iswspace (ord c) /= 0
#endif
  where
    uc = fromIntegral (ord c) :: Word

-- | Fold each word of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>>  Stream.fold Fold.toList $ words Fold.toList (Stream.fromList "fold these     words")
-- ["fold","these","words"]
--
-- > words = Stream.wordsBy isSpace
--
-- /Pre-release/
{-# INLINE words #-}
words :: Monad m => Fold m Char b -> Stream m Char -> Stream m b
words f = D.wordsBy isSpace f

-- | Unfold a stream to character streams using the supplied 'Unfold'
-- and concat the results suffixing a newline character @\\n@ to each stream.
--
-- @
-- unlines = Stream.interposeSuffix '\n'
-- unlines = Stream.intercalateSuffix Unfold.fromList "\n"
-- @
--
-- /Pre-release/
{-# INLINE unlines #-}
unlines :: MonadIO m => Unfold m a Char -> Stream m a -> Stream m Char
unlines = Stream.interposeSuffix '\n'

-- | Unfold the elements of a stream to character streams using the supplied
-- 'Unfold' and concat the results with a whitespace character infixed between
-- the streams.
--
-- @
-- unwords = Stream.interpose ' '
-- unwords = Stream.intercalate Unfold.fromList " "
-- @
--
-- /Pre-release/
{-# INLINE unwords #-}
unwords :: MonadIO m => Unfold m a Char -> Stream m a -> Stream m Char
unwords = Stream.interpose ' '
