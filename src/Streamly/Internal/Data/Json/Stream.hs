-- |
-- Module      : Streamly.Internal.Data.Json.Stream
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : pranaysashank@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Internal.Data.Json.Stream
    ( parseJson
    , parseJsonEOF
    )
where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
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
import Streamly.Internal.Data.Strict (Tuple' (..))
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserK.Types as K
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

backslash, zero, minus, plus :: Word8
backslash = 92 :: Word8
zero = 48 :: Word8
minus = 45 :: Word8
plus = 43 :: Word8

instance Enum a => Hashable (A.Array a) where
    hash arr = fromIntegral $ runIdentity $ IUF.fold A.read IFL.rollingHash arr
    hashWithSalt salt arr = fromIntegral $ runIdentity $
        IUF.fold A.read (IFL.rollingHashWithSalt $ fromIntegral salt) arr

{-# INLINE jsonEscapes #-}
jsonEscapes :: Word8 -> Maybe Word8
jsonEscapes 34  = Just 34 -- \" -> "
jsonEscapes 92  = Just 92 -- \\ -> \
jsonEscapes 47  = Just 47 -- \/ -> /
jsonEscapes 98  = Just 8  -- \b -> BS
jsonEscapes 102 = Just 12 -- \f -> FF
jsonEscapes 110 = Just 10 -- \n -> LF
jsonEscapes 114 = Just 13 -- \r -> CR
jsonEscapes 116 = Just 9  -- \t -> TAB
jsonEscapes _   = Nothing

{-# INLINE escapeFoldUtf8With #-}
escapeFoldUtf8With :: Monad m => Fold m Char container -> Fold m Word8 container
escapeFoldUtf8With = Uni.escapeFoldUtf8With 92 jsonEscapes

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

{-# INLINE skipSpace #-}
skipSpace :: MonadCatch m => Parser m Word8 ()
skipSpace =
    P.takeWhile
        (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)
        FL.drain

{-# INLINE skip #-}
skip :: MonadCatch m => Int -> Parser m a ()
skip n = P.take n FL.drain

{-# INLINE string #-}

string :: MonadCatch m => String -> Parser m Word8 ()
string = P.eqBy (==) . map (fromIntegral . ord)

{-# INLINE match #-}
match :: MonadCatch m => Word8 -> Parser m Word8 ()
match w = P.eqBy (==) [w]

{-# SPECIALISE foldToInteger :: Monad m => Int -> Fold m Word8 Int #-}
{-# SPECIALISE foldToInteger :: Monad m => Integer -> Fold m Word8 Integer #-}
{-# INLINE foldToInteger #-}
foldToInteger :: (Num a, Monad m) => a -> Fold m Word8 a
foldToInteger begin = Fold step initial extract
  where
    initial = return begin

    step s a = return $ s * 10 + fromIntegral (a - 48)

    extract = return

{-# INLINE parseDecimal0 #-}
parseDecimal0 :: MonadCatch m => Parser m Word8 Integer
parseDecimal0 = do
    z <- P.peek
    n <- P.takeWhile1 (\w -> w - 48 <= 9) (foldToInteger 0)
    when (z == zero && n /= 0) $ do
        P.die $ " Leading zero in a number is not accepted in JSON."
    return n

{-# INLINE parseDecimal #-}
parseDecimal :: MonadCatch m => Parser m Word8 Int
parseDecimal = do
  sign <- P.peek
  let positive = sign == plus || sign /= minus
      pr = P.takeWhile1 (\w -> w - 48 <= 9) (foldToInteger 0)
  when (sign == plus || sign == minus) (skip 1)
  if positive then pr else negate <$> pr

{-# INLINE parseJsonNumber #-}
parseJsonNumber :: MonadCatch m => Parser m Word8 Scientific
parseJsonNumber = do
    sign <- P.peek
    let positive = sign == plus || sign /= minus
    when (sign == plus || sign == minus) (skip 1)
    n <- parseDecimal0
    dot <- P.peekMaybe
    Tuple' c e <-
        case dot of
            Just 46 -> do
                skip 1
                P.takeWhile1
                    (\w -> w - 48 <= 9)
                    (Tuple' <$> foldToInteger n <*> FL.length)
            _ -> return $ Tuple' n 0

    let signedCoeff = if positive then c else (-c)
    mex <- P.peekMaybe
    case mex of
        Just ex
            | ex == 101 || ex == 69 -> do
                skip 1
                Sci.scientific signedCoeff . (e +) <$> parseDecimal
        _ -> return (Sci.scientific signedCoeff e)

{-# INLINE parseJsonString #-}
parseJsonString :: MonadCatch m => Parser m Word8 JsonString
parseJsonString = do
    match DOUBLE_QUOTE
    s <- P.takeWhile (\w -> w /= DOUBLE_QUOTE && w /= BACKSLASH) (Uni.foldUtf8With A.unsafeWrite)
    w <- P.peek
    case w of
        DOUBLE_QUOTE -> skip 1 >> return s
        BACKSLASH -> fmap (s <>) escapeParseJsonString
        _ -> do
            P.die $ [(chr . fromIntegral) w] ++ " : String without end."

{-# INLINE escapeParseJsonString #-}
escapeParseJsonString :: MonadCatch m => Parser m Word8 JsonString
escapeParseJsonString = P.scan startState go (escapeFoldUtf8With A.unsafeWrite)
  where
    startState = False
    go s a
        | s = Just False
        | a == DOUBLE_QUOTE = Nothing
        | otherwise =
            let a' = a == backslash
             in Just a'

{-# INLINE parseJsonValue #-}
parseJsonValue :: MonadCatch m => Parser m Word8 Value
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
        _  | w >= C_0 && w <= C_9 || w == MINUS -> Number <$> parseJsonNumber
           | otherwise ->  do
            P.die $ "Encountered " ++ [(chr . fromIntegral) w] ++ " when expecting one of [, {, \", f(alse), t(rue), n(ull)."

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

{-# INLINE parseJsonEOF #-}
parseJsonEOF :: MonadCatch m => PR.Parser m Word8 Value
parseJsonEOF =
    K.toParserK $ do
        v <- parseJsonValue
        skipSpace
        P.eof
        return v

{-# INLINE parseJson #-}
parseJson :: MonadCatch m => PR.Parser m Word8 Value
parseJson = K.toParserK $ parseJsonValue

{-

{-# INLINE pushToFold #-}
pushToFold :: Monad m => Fold m a b -> a -> Fold m a b
pushToFold (Fold step initial extract) a = Fold step initial' extract
    where
    initial' = do
        s <- initial
        step s a

sepBy1 ::
       MonadCatch m
    => Fold m a c
    -> Parser m Word8 a
    -> Parser m Word8 sep
    -> Parser m Word8 c
sepBy1 fl p sep = do
    a <- p
    P.many (pushToFold fl a) (sep >> p)

sepBy ::
       MonadCatch m
    => Fold m a c
    -> Parser m Word8 a
    -> Parser m Word8 sep
    -> Parser m Word8 c
sepBy fl@(Fold _ initial extract) p sep =
    sepBy1 fl p sep `P.alt` P.yieldM (initial >>= extract)

parseJsonValue = skipSpace >>
            (Object <$> parseJsonObject)
    `P.alt` (Array  <$> parseJsonArray)
    `P.alt` (String <$> parseJsonString)
    `P.alt` (Number <$> parseJsonNumber)
    `P.alt` (Bool True <$ string "true")
    `P.alt` (Bool False <$ string "false")
    `P.alt` (Null <$ string "null")

-}
