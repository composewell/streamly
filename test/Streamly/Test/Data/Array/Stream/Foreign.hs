module Main (main) where

import Data.Word (Word8)
import Streamly.Test.Common (listEquals, chooseInt)
import Test.Hspec (hspec, describe, shouldBe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property, vectorOf, Gen)
import Test.QuickCheck.Monadic (monadicIO, run)

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrayStream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Test.Hspec as Hspec

import Prelude hiding (sequence)

#if MIN_VERSION_QuickCheck(2,14,0)

import Test.QuickCheck (chooseAny)

#else

import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

#endif

maxTestCount :: Int
maxTestCount = 100

parseBreak :: Property
parseBreak = do
    let len = 200
    -- ls = input list (stream)
    -- clen = chunk size
    -- tlen = parser take size
    forAll
        ((,,)
            <$> vectorOf len (chooseAny :: Gen Int)
            <*> chooseInt (1, len)
            <*> chooseInt (0, len))
        $ \(ls, clen, tlen) ->
            monadicIO $ do
                (ls1, str) <-
                    let input =
                            Stream.chunksOf
                                clen (Array.writeN clen) (Stream.fromList ls)
                        parser = Parser.fromFold (Fold.take tlen Fold.toList)
                     in run $ ArrayStream.parseBreak parser input
                ls2 <- run $ Stream.toList $ ArrayStream.concat str
                listEquals (==) (ls1 ++ ls2) ls

splitOnSuffix :: Word8 -> [Word8] -> [[Word8]] -> IO ()
splitOnSuffix sep inp out = do
    res <-
        Stream.toList
            $ ArrayStream.splitOnSuffix sep
            $ Stream.chunksOf 2 (Array.writeN 2) $ Stream.fromList inp
    fmap Array.toList res `shouldBe` out

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Stream.Foreign"

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream parsing" $ do
                prop "parseBreak" parseBreak
            describe "splifOnSuffix" $ do
                Hspec.it "splitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6]"
                       $ splitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6]
                                         [[1, 2], [4], [5, 6]]
                Hspec.it "splitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6, 0]"
                       $ splitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6, 0]
                                         [[1, 2], [4], [5, 6]]
                Hspec.it "splitOnSuffix 0 [0, 1, 2, 0, 4, 0, 5, 6]"
                       $ splitOnSuffix 0 [0, 1, 2, 0, 4, 0, 5, 6]
                                         [[], [1, 2], [4], [5, 6]]
