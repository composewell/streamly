module Main (main) where

import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Int (Int8, Int16, Int32, Int64)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    )
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Internal.Data.Binary.Parser as Decoder
import qualified Streamly.Internal.Data.Binary.Stream as Encoder
import qualified Streamly.Data.Stream as Stream
import Streamly.Internal.Data.Parser (Parser)

import Prelude hiding
    (maximum, minimum, elem, notElem, null, product, sum, head, last, take)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import GHC.Base (unsafeChr)

word16Max :: Word16
word16Max = maxBound

word32Max :: Word32
word32Max = maxBound

word64Max :: Word64
word64Max = maxBound

chooseWord8 :: (Word8, Word8) -> Gen Word8
chooseWord8 = choose

chooseWord16 :: (Word16, Word16) -> Gen Word16
chooseWord16 = choose

chooseWord32 :: (Word32, Word32) -> Gen Word32
chooseWord32 = choose

chooseWord64 :: (Word64, Word64) -> Gen Word64
chooseWord64 = choose

chooseInt8 :: (Int8, Int8) -> Gen Int8
chooseInt8 = choose

chooseInt16 :: (Int16, Int16) -> Gen Int16
chooseInt16 = choose

chooseInt32 :: (Int32, Int32) -> Gen Int32
chooseInt32 = choose

chooseInt64 :: (Int64, Int64) -> Gen Int64
chooseInt64 = choose

chooseFloat :: (Float, Float) -> Gen Float
chooseFloat = choose

chooseDouble :: (Double, Double) -> Gen Double
chooseDouble = choose

chooseChar :: (Char, Char) -> Gen Char
chooseChar = choose

action :: (Monad m, Eq a) =>
       a
    -> (a -> Stream.Stream m Word8)
    -> Parser Word8 m a
    -> m Bool
action x enc dec = do
    let str = enc x
    v1 <- Stream.parse dec str
    return $
        case v1 of
            Right v -> x == v
            Left _ -> False

unit :: Property
unit = monadicIO $
    do
    let str = Encoder.unit
    v1 <- Stream.parse Decoder.unit str
    return $
        case v1 of
            Right v -> v == ()
            Left _ -> False

bool :: Property
bool =
    forAll (chooseWord8 (0, 1)) $ \case
        0 -> monadicIO $ action False Encoder.bool Decoder.bool
        _ -> monadicIO $ action True Encoder.bool Decoder.bool

ordering :: Property
ordering =
    forAll (chooseWord8 (0, 2)) $ \case
        0 -> monadicIO $ action LT Encoder.ordering Decoder.ordering
        1 -> monadicIO $ action EQ Encoder.ordering Decoder.ordering
        _ -> monadicIO $ action GT Encoder.ordering Decoder.ordering

word8 :: Property
word8 =
    forAll (chooseWord8 (0, 255)) $ \x ->
        monadicIO $ action x Encoder.word8  Decoder.word8

word16be :: Property
word16be =
    forAll (chooseWord16 (0, word16Max)) $ \x ->
        monadicIO $ action x Encoder.word16be Decoder.word16be

word32be :: Property
word32be =
    forAll (chooseWord32 (0, word32Max)) $ \x ->
        monadicIO $ action x Encoder.word32be Decoder.word32be

word64be :: Property
word64be =
    forAll (chooseWord64 (0, word64Max)) $ \x ->
        monadicIO $ action x  Encoder.word64be Decoder.word64be

word16le :: Property
word16le =
    forAll (chooseWord16 (0, word16Max)) $ \x ->
        monadicIO $ action x Encoder.word16le Decoder.word16le

word32le :: Property
word32le =
    forAll (chooseWord32 (0, word32Max)) $ \x ->
        monadicIO $ action x Encoder.word32le Decoder.word32le

word64le :: Property
word64le =
    forAll (chooseWord64 (0, word64Max)) $ \x ->
        monadicIO $ action x  Encoder.word64le Decoder.word64le

int8 :: Property
int8 =
    forAll (chooseInt8 (-128, 127)) $ \x ->
        monadicIO $ action x Encoder.int8  Decoder.int8

int16be :: Property
int16be =
    forAll (chooseInt16 (-32768, 32767)) $ \x ->
        monadicIO $ action x Encoder.int16be Decoder.int16be

int32be :: Property
int32be =
    forAll (chooseInt32 (-2147483648, 2147483647)) $ \x ->
        monadicIO $ action x Encoder.int32be Decoder.int32be

int64be :: Property
int64be =
    forAll (chooseInt64 (-9223372036854775808, 9223372036854775807)) $ \x ->
        monadicIO $ action x  Encoder.int64be Decoder.int64be

int16le :: Property
int16le =
    forAll (chooseInt16 (-32768, 32767)) $ \x ->
        monadicIO $ action x Encoder.int16le Decoder.int16le

int32le :: Property
int32le =
    forAll (chooseInt32 (-2147483648, 2147483647)) $ \x ->
        monadicIO $ action x Encoder.int32le Decoder.int32le

int64le :: Property
int64le =
    forAll (chooseInt64 (-9223372036854775808, 9223372036854775807)) $ \x ->
        monadicIO $ action x  Encoder.int64le Decoder.int64le

float32be :: Property
float32be =
    forAll (chooseFloat (-3.40282e+38, 3.40282e+38)) $ \x ->
        monadicIO $ action x Encoder.float32be  Decoder.float32be

float32le :: Property
float32le =
    forAll (chooseFloat (-3.40282e+38, 3.40282e+38)) $ \x ->
        monadicIO $ action x Encoder.float32le  Decoder.float32le

double64be :: Property
double64be =
    forAll (chooseDouble (-1.79769e+308, 1.79769e+308)) $ \x ->
        monadicIO $ action x Encoder.double64be  Decoder.double64be

double64le :: Property
double64le =
    forAll (chooseDouble (-1.79769e+308, 1.79769e+308)) $ \x ->
        monadicIO $ action x Encoder.double64le  Decoder.double64le

charLatin1 :: Property
charLatin1 = forAll (chooseChar (unsafeChr 0, unsafeChr 255)) $ \x ->
        monadicIO $ action x Encoder.charLatin1  Decoder.charLatin1

moduleName :: String
moduleName = "Serialize.Serializable"

main :: IO ()
main =
    hspec $ do
    describe moduleName $ do
        -- Unit
        prop "unit" Main.unit

        -- Bool
        prop "bool" Main.bool

        -- Ordering
        prop "ordering" Main.ordering

        -- Words
        prop "word8" Main.word8
        prop "word16be" Main.word16be
        prop "word32be" Main.word32be
        prop "word64be" Main.word64be
        prop "word16le" Main.word16le
        prop "word32le" Main.word32le
        prop "word64le" Main.word64le

        -- Integral
        prop "int8" Main.int8
        prop "int16be" Main.int16be
        prop "int32be" Main.int32be
        prop "int64be" Main.int64be
        prop "int16le" Main.int16le
        prop "int32le" Main.int32le
        prop "int64le" Main.int64le

        -- Fractional
        prop "float32be" Main.float32be
        prop "float32le" Main.float32le
        prop "double64be" Main.double64be
        prop "double64le" Main.double64le

        -- Char
        prop "charLatin1" Main.charLatin1
