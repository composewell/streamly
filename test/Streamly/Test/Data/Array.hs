-- |
-- Module      : Streamly.Test.Data.Array
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array (main) where

import Data.Word (Word8)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, chooseInt)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Streamly.Internal.Data.MutByteArray (Serialize)
import Streamly.Test.Common (listEquals)
import Streamly.Test.Data.Array.Type (typeMain)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.MutByteArray as Serialize
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as S

#if MIN_VERSION_QuickCheck(2,14,0)
import Test.QuickCheck (chooseAny)
#else
import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x, _) = random r in x)
#endif

moduleName :: String
moduleName = "Data.Array"

maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

-------------------------------------------------------------------------------
-- Array.Stream tests
-------------------------------------------------------------------------------

chunksOf :: Monad m => Int -> Fold.Fold m a b -> S.Stream m a -> S.Stream m b
chunksOf n f = S.foldMany (Fold.take n f)

testParseBreak :: Property
testParseBreak = do
    let len = 200
    forAll
        ((,,)
            <$> vectorOf len (chooseAny :: Gen Int)
            <*> chooseInt (1, len)
            <*> chooseInt (0, len))
        $ \(ls, clen, tlen) ->
            monadicIO $ do
                (ls1, str) <-
                    let input =
                            S.toStreamK
                                $ chunksOf clen (A.createOf clen) (S.fromList ls)
                        parser = Parser.fromFold (Fold.take tlen Fold.toList)
                     in run $ A.parseBreak (A.toParserK parser) input
                ls2 <- run $ S.fold Fold.toList (A.concat $ S.fromStreamK str)
                case ls1 of
                    Right x -> listEquals (==) (x ++ ls2) ls
                    Left _ -> assert False

-------------------------------------------------------------------------------
-- Array serialize/deserialize tests
-------------------------------------------------------------------------------

-- | Roundtrip a value through the Array serialization API. Apart from the
-- plain serialize'/deserialize roundtrip, this also exercises deserialization
-- from a slice with a non-zero start offset (to catch hardcoded 0 offsets) and
-- from a cloned slice.
serializeRoundtrip ::
       forall a. (Eq a, Show a, Serialize a) => a -> IO ()
serializeRoundtrip val = do
    val `shouldBe` fst (A.deserialize (A.serialize' val))

    -- Serialize into a buffer at a non-zero offset and build an Array slice
    -- over the serialized region.
    let sz = Serialize.addSizeTo 0 val
        off = 10
    arr <- Serialize.new (sz + off)
    end <- Serialize.serializeAt off arr val
    let slice = A.Array arr off end :: A.Array Word8
    val `shouldBe` fst (A.deserialize slice)

    clonedSlice <- A.unsafeFreeze <$> MutArray.clone (A.unsafeThaw slice)
    val `shouldBe` fst (A.deserialize clonedSlice)

testSplitOnSuffix :: Word8 -> [Word8] -> [[Word8]] -> IO ()
testSplitOnSuffix sep inp out = do
    res <-
        S.fold Fold.toList
            $ A.compactEndByByte_ sep
            $ chunksOf 2 (A.createOf 2) $ S.fromList inp
    fmap A.toList res `shouldBe` out

arrayMain :: SpecWith ()
arrayMain = do
    -- Stream of Arrays (Streamly.Internal.Data.Array)
    describe "compactEndByByte_" $ do
        it "0 [1,2,0,4,0,5,6]"
               $ testSplitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6]
                                    [[1, 2], [4], [5, 6]]
        it "0 [1,2,0,4,0,5,6,0]"
               $ testSplitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6, 0]
                                    [[1, 2], [4], [5, 6]]
        it "0 [0,1,2,0,4,0,5,6]"
               $ testSplitOnSuffix 0 [0, 1, 2, 0, 4, 0, 5, 6]
                                    [[], [1, 2], [4], [5, 6]]
    -- Parsing Stream of Arrays (Streamly.Internal.Data.Array)
    prop "parseBreak" testParseBreak
    describe "serialize/deserialize" $ do
        prop "Int"       $ \(x :: Int) -> serializeRoundtrip x
        prop "[Int]"     $ \(x :: [Int]) -> serializeRoundtrip x
        prop "Array Int" $ \(x :: [Int]) -> serializeRoundtrip (A.fromList x)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $
    describe moduleName $ do
        typeMain
        -- IMPORTANT NOTE: Before adding any test here first consider if it can
        -- be added to the Array/Common test module. Only those tests which are
        -- specific to the Unboxed Array module and do not apply to the Generic
        -- Array module should be added here.
        arrayMain
