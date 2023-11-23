{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Data.Word (Word8)
import Streamly.Test.Common (listEquals, chooseInt)
import Test.Hspec (hspec, describe, shouldBe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property, vectorOf, Gen, Arbitrary (arbitrary))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Array.Stream as ArrayStream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as Stream
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

chunksOf :: Monad m
    => Int -> Fold.Fold m a b -> Stream.Stream m a -> Stream.Stream m b
chunksOf n f = Stream.foldMany (Fold.take n f)

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
                            Stream.toStreamK
                                $ chunksOf
                                clen (Array.writeN clen) (Stream.fromList ls)
                        parser = Parser.fromFold (Fold.take tlen Fold.toList)
                     in run $ ArrayStream.parseBreak parser input
                ls2 <- run $ Stream.fold Fold.toList (ArrayStream.concat $ Stream.fromStreamK str)
                case ls1 of
                    Right x -> listEquals (==) (x ++ ls2) ls
                    Left _ -> assert False

splitOnSuffix :: Word8 -> [Word8] -> [[Word8]] -> IO ()
splitOnSuffix sep inp out = do
    res <-
        Stream.fold Fold.toList
            $ ArrayStream.splitOnSuffix sep
            $ chunksOf 2 (Array.writeN 2) $ Stream.fromList inp
    fmap Array.toList res `shouldBe` out

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Stream"

-- Instead of hard coding 10000 here we can have maxStreamLength for operations
-- that use stream of arrays.

concatArrayW8 :: Property
concatArrayW8 =
    forAll (vectorOf 10000 (arbitrary :: Gen Word8))
        $ \w8List -> do
              let w8ArrList = Array.fromList . (: []) <$> w8List
              f2 <- Stream.fold Fold.toList $ ArrayStream.concat $ Stream.fromList w8ArrList
              w8List `shouldBe` f2


main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream parsing" $ do
                prop "parseBreak" parseBreak
                prop "concatArrayW8" concatArrayW8
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
