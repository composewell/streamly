-- |
-- Module      : Streamly.Internal.Data.Json.Stream
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedLists #-}

module Streamly.Internal.Data.Json.Stream 
    ( parseJson )
where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Data.Bits (testBit)
import Data.Char (chr, ord)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as Sci

import Streamly.Internal.Data.Parser.ParserD (Parser)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as P
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Unicode.Stream as Uni
import qualified Streamly.Data.Fold as FL

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define COLON 58
#define C_0 48
#define C_9 57
#define MINUS 45
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

backslash = 92 :: Word8
close_curly = 125 :: Word8
close_square = 93 :: Word8
comma = 44 :: Word8
double_quote = 34 :: Word8
open_curly = 123 :: Word8
open_square = 91 :: Word8
colon = 58 :: Word8
c_0 = 48 :: Word8
c_9 = 57 :: Word8
c_A = 65 :: Word8
c_F = 70 :: Word8
c_a = 97 :: Word8
c_f = 102 :: Word8
c_n = 110 :: Word8
c_t = 116 :: Word8
zero = 48 :: Word8
minus = 45 :: Word8
plus = 43 :: Word8


instance Enum a => Hashable (A.Array a) where
    hash arr = fromIntegral $ runIdentity $ IUF.fold A.read IFL.rollingHash arr
    hashWithSalt salt arr = fromIntegral $ runIdentity $
        IUF.fold A.read (IFL.rollingHashWithSalt $ fromIntegral salt) arr

type JsonString = Array Char

type JsonArray = Array Value

-- | A JSON \"object\" (key\/value map).
type Object = HashMap JsonString Value

-- | A JSON value represented as a Haskell value.
data Value
    = Object !Object
    | Array !JsonArray
    | String !JsonString
    | Number !Scientific
    | Bool !Bool
    | Null
    deriving (Eq, Show)

{-# INLINE pushToFold #-}
pushToFold :: Monad m => Fold m a b -> a -> Fold m a b
pushToFold (Fold step initial extract) a = Fold step initial' extract
    where
    initial' = do
        s <- initial
        step s a

{-# INLINE skipSpace #-}
skipSpace :: MonadCatch m => Parser m Word8 ()
skipSpace =
    P.takeWhile
        (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)
        FL.drain

{-# INLINE spaceAround #-}
spaceAround :: MonadCatch m => Parser m Word8 b -> Parser m Word8 b
spaceAround parseInBetween = do
    skipSpace
    between <- parseInBetween
    skipSpace
    return between

{-# INLINE skip #-}
skip :: MonadCatch m => Int -> Parser m a ()
skip n = P.take n FL.drain

{-# INLINE string #-}
string :: MonadCatch m => String -> Parser m Word8 ()
string = P.eqBy (==) . map (fromIntegral . ord)

{-# INLINE match #-}
match :: MonadCatch m => Word8 -> Parser m Word8 ()
match w = P.eqBy (==) [w]

{-# INLINE foldToInteger #-}
foldToInteger :: Monad m => Fold m Word8 Integer
foldToInteger = Fold step initial extract
  where
    initial = return 0

    step s a = return $ s * 10 + fromIntegral (a - 48)

    extract = return

{-# INLINE parseDecimal0 #-}
parseDecimal0 :: MonadCatch m => Parser m Word8 Integer
parseDecimal0 = do
    h <- P.peek
    n <- P.peek
    when (h == zero && n - 48 > 9) $ error "Leading zero in a number is not accepted in JSON."
    P.takeWhile1 (\w -> w - 48 <= 9) foldToInteger

{-# INLINE parseJsonNumber #-}
parseJsonNumber :: MonadCatch m => Parser m Word8 Scientific
parseJsonNumber = do
    sign <- P.peek
    let positive = sign == plus || sign /= minus
    when (sign == plus || sign == minus) (skip 1)
    n <- parseDecimal0
    let signedCoeff = if positive then n else (-n)
    return (Sci.scientific signedCoeff 0)

{-# INLINE parseJsonString #-}
parseJsonString :: MonadCatch m => Parser m Word8 JsonString
parseJsonString = do
    match DOUBLE_QUOTE
    s <- P.takeWhile (\w -> w /= DOUBLE_QUOTE && w /= BACKSLASH) (Uni.foldUtf8With A.unsafeWrite)
    w <- P.peek
    case w of
        DOUBLE_QUOTE -> skip 1 >> return s
        _ -> do
            chars40 <- P.take 40 FL.toList
            error (fmap (chr . fromIntegral) chars40 ++ " Not yet implemented to handle escape sequences in String.")

{-# INLINE parseJsonValue #-}
parseJsonValue :: MonadCatch m => Parser m Word8 Value
{- parseJsonValue = skipSpace >>
            (Object <$> parseJsonObject)
    `P.alt` (Array  <$> parseJsonArray)
    `P.alt` (String <$> parseJsonString)
    `P.alt` (Number <$> parseJsonNumber)
    `P.alt` (Bool True <$ string "true")
    `P.alt` (Bool False <$ string "false")
    `P.alt` (Null <$ string "null") -}
parseJsonValue = do
    skipSpace
    w <- P.peek
    case w of
        OPEN_CURLY -> Object <$> parseJsonObject
        OPEN_SQUARE -> Array <$> parseJsonArray
        DOUBLE_QUOTE -> String <$> parseJsonString
        C_t -> Bool True <$ string "true"
        C_f -> Bool False <$ string "false"
        C_n -> Null <$ string "null"
        CLOSE_CURLY -> return $ Object HM.empty
        CLOSE_SQUARE -> return $ Array mempty
        _  | w >= C_0 && w <= C_9 || w == MINUS -> Number <$> parseJsonNumber
           | otherwise ->  do
            chars40 <- P.take 40 FL.toList
            error $ "Encountered " ++ fmap (chr . fromIntegral) chars40 ++ " when expecting one of [, {, \", f(alse), t(rue), n(ull)."

{-# INLINE parseJsonMember #-}
parseJsonMember :: MonadCatch m => Parser m Word8 (JsonString, Value)
parseJsonMember = do
    skipSpace
    jsonString <- parseJsonString
    skipSpace
    match COLON
    jsonValue <- parseJsonValue
    return (jsonString, jsonValue)


{-# INLINE parseJsonObject #-}
parseJsonObject :: MonadCatch m => Parser m Word8 Object
parseJsonObject = do
    skipSpace
    match OPEN_CURLY
    skipSpace
    object <- P.sepBy (Fold (\h (k, v) -> return $ HM.insert k v h) (return HM.empty) return) parseJsonMember (skipSpace >> match COMMA)
    skipSpace
    match CLOSE_CURLY
    return object

{-# INLINE parseJsonArray #-}
parseJsonArray :: MonadCatch m => Parser m Word8 JsonArray
parseJsonArray = do
    skipSpace
    match OPEN_SQUARE
    skipSpace
    jsonValues <- P.sepBy A.unsafeWrite parseJsonValue (skipSpace >> match COMMA)
    skipSpace
    match CLOSE_SQUARE
    return jsonValues

{-# INLINE parseJson #-}
parseJson :: MonadCatch m => PR.Parser m Word8 Value
-- parseJson = P.toParserK (spaceAround $ (Object <$> parseJsonObject) `P.alt` (Array <$> parseJsonArray))
parseJson = P.toParserK $ do
    skipSpace
    w <- P.peek
    case w of
        OPEN_CURLY  -> Object <$> parseJsonObject
        OPEN_SQUARE -> Array <$> parseJsonArray
        _           -> error $ "Encountered " ++ [chr . fromIntegral $ w] ++ " when expection [ or {."