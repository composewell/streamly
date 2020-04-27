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

module Streamly.Internal.Data.Json.Stream where

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

import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Data.Fold as FL

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define SEMI_COLON 58
#define C_0 48
#define C_9 57
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
semi_colon = 58 :: Word8
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

{-# INLINE sepBy1 #-}
sepBy1 :: MonadCatch m
       => Fold m a c
       -> Parser m Word8 a
       -> Parser m Word8 sep
       -> Parser m Word8 c
sepBy1 fl p sep = do 
    a <- p
    P.many (pushToFold fl a) (sep >> p)

{-# INLINE sepBy #-}
sepBy :: MonadCatch m
      => Fold m a c
      -> Parser m Word8 a
      -> Parser m Word8 sep
      -> Parser m Word8 c
sepBy fl @ (Fold _ initial extract) p sep = 
    sepBy1 fl p sep `P.alt` P.yieldM (initial >>= extract)


skipSpace :: MonadCatch m => Parser m Word8 ()
skipSpace =
    P.takeWhile
        (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)
        FL.drain

spaceAround :: MonadCatch m => Parser m Word8 b -> Parser m Word8 b
spaceAround parseInBetween = do
    skipSpace
    between <- parseInBetween
    skipSpace
    return between

skip :: MonadCatch m => Int -> Parser m a ()
skip n = P.take n FL.drain

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

parseDecimal0 :: MonadCatch m => Parser m Word8 Integer
parseDecimal0 = do
    h <- P.peek
    when (h == zero) $ error "Leading zero in a number is not accepted in JSON."
    P.takeWhile1 (\w -> w - 48 <= 9) foldToInteger

parseJsonNumber :: MonadCatch m => Parser m Word8 Scientific
parseJsonNumber = do
    sign <- P.peek
    let positive = sign == plus || sign /= minus
    when (sign == plus || sign == minus) (skip 1)
    n <- parseDecimal0
    let signedCoeff = if positive then n else (-n)
    return (Sci.scientific signedCoeff 0)

parseJsonString :: MonadCatch m => Parser m Word8 JsonString
parseJsonString = do
    s <- P.takeWhile (\w -> w /= DOUBLE_QUOTE && w /= BACKSLASH && not (testBit w 7)) A.unsafeWrite
    w <- P.peek
    case w of
        DOUBLE_QUOTE -> skip 1 >> return (fmap (chr . fromIntegral) s)
        _ -> error "Not yet implemented to handle escape sequences in String."


parseJsonValue :: MonadCatch m => Parser m Word8 Value
parseJsonValue = skipSpace >>
            (Object <$> parseJsonObject)
    `P.alt` (Array  <$> parseJsonArray)
    `P.alt` (String <$> parseJsonString)
    `P.alt` (Number <$> parseJsonNumber)
    `P.alt` (Bool True <$ string "true")
    `P.alt` (Bool False <$ string "false")
    `P.alt` (Null <$ string "null")

parseJsonMember :: MonadCatch m => Parser m Word8 (JsonString, Value)
parseJsonMember = do
    skipSpace
    jsonString <- parseJsonString
    skipSpace
    match SEMI_COLON
    jsonValue <- parseJsonValue
    return (jsonString, jsonValue)

parseJsonObject :: MonadCatch m => Parser m Word8 Object
parseJsonObject = do
    skipSpace
    match OPEN_CURLY
    skipSpace
    object <- sepBy (Fold (\h (k, v) -> return $ HM.insert k v h) (return HM.empty) return) parseJsonMember (skipSpace >> match COMMA)
    skipSpace
    match CLOSE_CURLY
    return object

parseJsonArray :: MonadCatch m => Parser m Word8 JsonArray
parseJsonArray = do
    skipSpace
    match OPEN_SQUARE
    skipSpace
    jsonValues <- sepBy A.unsafeWrite parseJsonValue (skipSpace >> match COMMA)
    skipSpace
    return jsonValues

parseJson :: MonadCatch m => Parser m Word8 Value
parseJson = spaceAround $ (Object <$> parseJsonObject) `P.alt` (Array <$> parseJsonArray)

{-

    w <- P.peek
    case w of
        OPEN_CURLY -> do
            skip 1
            return Null
        C_f -> do
            string "false"
            return $ Bool False
        C_t -> do
            string "true"
            return $ Bool True
        C_n -> do
            string "null"
            return Null

-}