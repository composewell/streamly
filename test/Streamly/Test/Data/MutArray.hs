module Streamly.Test.Data.MutArray (main) where

import Control.Monad (void)
import Data.Char (isLower)
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.List (sort)
import Data.Proxy (Proxy(..))
import Foreign.Ptr (IntPtr, WordPtr)
import Foreign.Storable (peek)
import GHC.Exts
import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Ptr (plusPtr)
import GHC.Real (Ratio(..))
import GHC.Stable (StablePtr(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import Streamly.Internal.Data.MutArray (MutArray)
import Streamly.Internal.Data.MutByteArray (Unbox, sizeOf)
import Streamly.Test.Common (chooseInt)
import System.Mem (performMajorGC)
import Test.Hspec (hspec, describe, it, shouldBe, shouldReturn, SpecWith)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, listOf, Property, vectorOf, Gen, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, assert)
#if MIN_VERSION_base(4,15,0)
import GHC.IO.SubSystem (IoSubSystem (..))
#endif

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Test.Hspec as Hspec

moduleName :: String
moduleName = "Data.MutArray"

#include "Streamly/Test/Data/MutArray/Common.hs"

testAppend ::  Property
testAppend =
   forAll (listOf (chooseInt (-50, 100))) $ \ls0 ->
        monadicIO $ action ls0

        where

        action ls = do
            arr0 <- MArray.emptyOf' 0
            x <- Stream.fold
                    (MArray.append2 arr0)
                    (Stream.fromList (ls::[Int]))
            lst <- MArray.toList x
            assert (ls == lst)

#define TEST_IE(_type) it "_type" $ testIE ([] :: [_type])

-- XXX This should be in test/Data.Unbox
-- C-ish foreign types are platfor specific. The platform needs to be taken into
-- consideration when making instances.
testUnboxInstanceExistance :: Hspec.SpecWith ()
testUnboxInstanceExistance = do
    describe "Unbox instances" $ do
        -- TEST_IE(CBool)
        -- TEST_IE(CChar)
        -- TEST_IE(CClock)
        -- TEST_IE(CDouble)
        -- TEST_IE(CFloat)
        -- TEST_IE(CInt)
        -- TEST_IE(CIntMax)
        -- TEST_IE(CIntPtr)
        -- TEST_IE(CLLong)
        -- TEST_IE(CLong)
        -- TEST_IE(CPtrdiff)
        -- TEST_IE(CSChar)
        -- TEST_IE(CSUSeconds)
        -- TEST_IE(CShort)
        -- TEST_IE(CSigAtomic)
        -- TEST_IE(CSize)
        -- TEST_IE(CTime)
        -- TEST_IE(CUChar)
        -- TEST_IE(CUInt)
        -- TEST_IE(CUIntMax)
        -- TEST_IE(CUIntPtr)
        -- TEST_IE(CULLong)
        -- TEST_IE(CULong)
        -- TEST_IE(CUSeconds)
        -- TEST_IE(CUShort)
        -- TEST_IE(CWchar)
        TEST_IE(IntPtr)
        TEST_IE(WordPtr)
        TEST_IE(Fingerprint)
        TEST_IE(Int16)
        TEST_IE(Int32)
        TEST_IE(Int64)
        TEST_IE(Int8)
#if MIN_VERSION_base(4,15,0)
        TEST_IE(IoSubSystem)
#endif
        TEST_IE(Word16)
        TEST_IE(Word32)
        TEST_IE(Word64)
        TEST_IE(Word8)
        -- TEST_IE(CBlkCnt)
        -- TEST_IE(CBlkSize)
        -- TEST_IE(CCc)
        -- TEST_IE(CClockId)
        -- TEST_IE(CDev)
        -- TEST_IE(CFsBlkCnt)
        -- TEST_IE(CFsFilCnt)
        -- TEST_IE(CGid)
        -- TEST_IE(CId)
        -- TEST_IE(CIno)
        -- TEST_IE(CKey)
        -- TEST_IE(CMode)
        -- TEST_IE(CNfds)
        -- TEST_IE(CNlink)
        -- TEST_IE(COff)
        -- TEST_IE(CPid)
        -- TEST_IE(CRLim)
        -- TEST_IE(CSocklen)
        -- TEST_IE(CSpeed)
        -- TEST_IE(CSsize)
        -- TEST_IE(CTcflag)
        -- TEST_IE(CTimer)
        -- TEST_IE(CUid)
        -- TEST_IE(Fd)
        TEST_IE(())
        TEST_IE(Bool)
        TEST_IE(Char)
        TEST_IE(Double)
        TEST_IE(Float)
        TEST_IE(Int)
        TEST_IE(Word)
        TEST_IE(Complex Int)
        TEST_IE(Identity Int)
#if MIN_VERSION_base(4,14,0)
        TEST_IE(Down Int)
#endif
        TEST_IE(FunPtr Int)
        TEST_IE(Ptr Int)
        TEST_IE(Ratio Int)
        TEST_IE(StablePtr Int)
        TEST_IE(Const Int Int)
    where

    testIE :: Unbox a => [a] -> IO ()
    testIE lst = void $ MArray.fromList lst

getIntList :: Ptr Int -> Int -> IO [Int]
getIntList ptr byteLen = do
    performMajorGC
    getList ptr (ptr `plusPtr` byteLen)

    where

    sizeOfInt = sizeOf (Proxy :: Proxy Int)

    -- We need to be careful here. We assume Unboxed and Storable are compatible
    -- with each other. For Int, they are compatible.
    getList p limitP
        | p >= limitP = return []
    getList p limitP = do
        val <- peek p
        rest <- getList (p `plusPtr` sizeOfInt) limitP
        return $ val : rest

testUnsafeAsPtr :: IO ()
testUnsafeAsPtr = do
    arr <- MArray.fromList ([0 .. 99] :: [Int])
    arr1 <- MArray.pin arr
    MArray.unsafeAsPtr arr1 getIntList `shouldReturn` [0 .. 99]

testByteLength :: forall a. Unbox a => a -> IO ()
testByteLength _ = do
    arrA <- MArray.emptyOf' 100 :: IO (MutArray a)
    let arrW8 = MArray.unsafeCast arrA :: MutArray Word8
    MArray.byteLength arrA `shouldBe` MArray.length arrW8

testBubbleWith :: Bool -> Property
testBubbleWith asc =
    forAll (listOf (chooseInt (-50, 100))) $ \ls0 ->
        monadicIO $ action ls0

        where

        action ls = do
            x <- Stream.fold (fldm ls) $ Stream.fromList ls
            lst <- MArray.toList x
            if asc
            then assert (sort ls == lst)
            else assert (sort ls == reverse lst)

        fldm ls =
            Fold.foldlM'
                (\b a -> do
                    arr <- MArray.snoc b a
                    if asc
                    then MArray.bubble compare arr
                    else MArray.bubble (flip compare) arr
                    return arr
                )
                (MArray.emptyOf' $ length ls)

testBubbleAsc :: Property
testBubbleAsc = testBubbleWith True

testBubbleDesc :: Property
testBubbleDesc = testBubbleWith False

testReallocBytes :: Property
testReallocBytes =
    let len = 10000
        bSize = len * sizeOf (Proxy :: Proxy Char)
    in forAll (vectorOf len (arbitrary :: Gen Char)) $ \vec ->
           forAll (chooseInt (bSize - 2000, bSize + 2000)) $ \newBLen -> do
               arr <- MArray.fromList vec
               arr1 <- MArray.reallocBytes newBLen arr
               lst <- MArray.toList arr
               lst1 <- MArray.toList arr1
               lst `shouldBe` lst1

-------------------------------------------------------------------------------
-- MutArray/Type.hs (Unboxed-only functions)
-------------------------------------------------------------------------------

testPinUnpin :: IO ()
testPinUnpin = do
    arr <- MArray.fromList ([1..5] :: [Int])
    pinned <- MArray.pin arr
    MArray.isPinned pinned `shouldBe` True
    unpinned <- MArray.unpin pinned
    MArray.isPinned unpinned `shouldBe` False
    lst <- MArray.toList unpinned
    lst `shouldBe` [1..5]

testBreakEndByWord8_ :: IO ()
testBreakEndByWord8_ = do
    arr <- MArray.fromList ([1,2,0,3,4] :: [Word8])
    (before, mafter) <- MArray.breakEndByWord8_ 0 arr
    MArray.toList before >>= (`shouldBe` [1,2])
    case mafter of
        Nothing -> fail "expected Just after"
        Just after -> MArray.toList after >>= (`shouldBe` [3,4])

testBreakEndByWord8_NotFound :: IO ()
testBreakEndByWord8_NotFound = do
    arr <- MArray.fromList ([1,2,3] :: [Word8])
    (before, mafter) <- MArray.breakEndByWord8_ 0 arr
    MArray.toList before >>= (`shouldBe` [1,2,3])
    case mafter of
        Just _ -> fail "expected Nothing"
        Nothing -> return ()

testReverse :: IO ()
testReverse = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.reverse arr
    lst <- MArray.toList arr
    lst `shouldBe` [5,4,3,2,1]

testFoldl' :: IO ()
testFoldl' = do
    arr <- MArray.fromList ([1..5] :: [Int])
    result <- MArray.foldl' (+) 0 arr
    result `shouldBe` 15

testFoldr :: IO ()
testFoldr = do
    arr <- MArray.fromList ([1..5] :: [Int])
    result <- MArray.foldr (:) [] arr
    result `shouldBe` [1..5]

testFold :: IO ()
testFold = do
    arr <- MArray.fromList ([1..5] :: [Int])
    result <- MArray.fold Fold.sum arr
    result `shouldBe` 15

testSplice :: IO ()
testSplice = do
    arr1 <- MArray.fromList ([1..5] :: [Int])
    arr2 <- MArray.fromList ([6..10] :: [Int])
    arr3 <- MArray.splice arr1 arr2
    lst <- MArray.toList arr3
    lst `shouldBe` [1..10]

testSplitEndBy_ :: IO ()
testSplitEndBy_ = do
    arr <- MArray.fromList ([1,2,0,3,4,0,5] :: [Word8])
    chunks <- Stream.fold Fold.toList $ MArray.splitEndBy_ (== 0) arr
    lsts <- mapM MArray.toList chunks
    lsts `shouldBe` [[1,2],[3,4],[5]]

testGetIndexRev :: IO ()
testGetIndexRev = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.getIndexRev 0 arr `shouldReturn` 5
    MArray.getIndexRev 4 arr `shouldReturn` 1

-------------------------------------------------------------------------------
-- MutArray module
-------------------------------------------------------------------------------

testIndexerFromLen :: IO ()
testIndexerFromLen = do
    arr <- MArray.fromList ([1..10] :: [Int])
    pairs <- Stream.fold Fold.toList
        $ Stream.unfold (MArray.indexerFromLen 0 3) arr
    pairs `shouldBe` [(0,3),(3,3),(6,3),(9,1)]

testSplitterFromLen :: IO ()
testSplitterFromLen = do
    arr <- MArray.fromList ([1..10] :: [Int])
    slices <- Stream.fold Fold.toList
        $ Stream.unfold (MArray.splitterFromLen 0 3) arr
    lsts <- mapM MArray.toList slices
    lsts `shouldBe` [[1,2,3],[4,5,6],[7,8,9],[10]]

testCompactMax :: IO ()
testCompactMax = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4],[5,6],[7,8]] :: [[Int]])
    result <- Stream.fold Fold.toList
        $ MArray.compactMax 4
        $ Stream.fromList arrs
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2,3,4],[5,6,7,8]]

testCompactMax' :: IO ()
testCompactMax' = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4]] :: [[Int]])
    result <- Stream.fold Fold.toList
        $ MArray.compactMax' 4
        $ Stream.fromList arrs
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2,3,4]]

testCompactSepByByte_ :: IO ()
testCompactSepByByte_ = do
    arr1 <- MArray.fromList ([1,2,0,3] :: [Word8])
    arr2 <- MArray.fromList ([4,0,5] :: [Word8])
    result <- Stream.fold Fold.toList
        $ MArray.compactSepByByte_ 0
        $ Stream.fromList [arr1, arr2]
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2],[3,4],[5]]

testCompactEndByByte_ :: IO ()
testCompactEndByByte_ = do
    arr1 <- MArray.fromList ([1,2,0,3] :: [Word8])
    arr2 <- MArray.fromList ([4,0,5] :: [Word8])
    result <- Stream.fold Fold.toList
        $ MArray.compactEndByByte_ 0
        $ Stream.fromList [arr1, arr2]
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2],[3,4],[5]]

testCompactEndByLn_ :: IO ()
testCompactEndByLn_ = do
    arr1 <- MArray.fromList ([1,2,10,3] :: [Word8])
    arr2 <- MArray.fromList ([4,10,5,6] :: [Word8])
    result <- Stream.fold Fold.toList
        $ MArray.compactEndByLn_
        $ Stream.fromList [arr1, arr2]
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2],[3,4],[5,6]]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (max 0 (length xs - n)) xs

testCreateOfLast :: Property
testCreateOfLast =
    forAll (chooseInt (0, 20)) $ \n ->
        forAll (listOf (arbitrary :: Gen Int)) $ \ls -> do
            arr <- Stream.fold (MArray.createOfLast n) (Stream.fromList ls)
            lst <- MArray.toList arr
            lst `shouldBe` lastN n ls

testSerializeDeserialize :: IO ()
testSerializeDeserialize = do
    let val = 42 :: Int
    arr <- MArray.serialize MArray.empty val
    (result, _) <- MArray.deserialize arr :: IO (Int, MutArray Word8)
    result `shouldBe` val

testSerializeDeserializeMultiple :: IO ()
testSerializeDeserializeMultiple = do
    let x = 1 :: Int
        y = 2 :: Int
    arr <- MArray.serialize MArray.empty x
    arr1 <- MArray.serialize arr y
    (v1, arr2) <- MArray.deserialize arr1 :: IO (Int, MutArray Word8)
    (v2, _) <- MArray.deserialize arr2 :: IO (Int, MutArray Word8)
    v1 `shouldBe` x
    v2 `shouldBe` y

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            commonMain
            -- IMPORTANT NOTE: Before adding any test here first consider if it
            -- can be added to the MutArray/Common test module. Only those tests
            -- which are specific to the Unboxed MutArray module and do not
            -- apply to the Generic MutArray module should be added here.

            -- MutArray/Type.hs module (Unboxed-only)
            describe "pin/unpin/isPinned" $ do
                it "roundtrip" testPinUnpin
            describe "breakEndByWord8_" $ do
                it "separator found"     testBreakEndByWord8_
                it "separator not found" testBreakEndByWord8_NotFound
            it "reverse" testReverse
            it "foldl'" testFoldl'
            it "foldr" testFoldr
            it "fold" testFold
            it "splice" testSplice
            describe "splitEndBy_" $ do
                it "basic" testSplitEndBy_
            describe "getIndexRev" $ do
                it "first and last" testGetIndexRev
            it "unsafeAsPtr" testUnsafeAsPtr
            describe "byteLength" $ do
                it "Int" (testByteLength (undefined :: Int))
                it "Char" (testByteLength (undefined :: Char))
            describe "bubble" $ do
                prop "ascending" testBubbleAsc
                prop "descending" testBubbleDesc
            prop "reallocBytes" testReallocBytes
            describe "Stream Append" $ do
                prop "append2" testAppend

            -- MutArray module
            describe "indexerFromLen" $ do
                it "basic" testIndexerFromLen
            describe "splitterFromLen" $ do
                it "basic" testSplitterFromLen
            describe "compactMax" $ do
                it "coalesces small arrays" testCompactMax
            describe "compactMax'" $ do
                it "coalesces to pinned" testCompactMax'
            describe "compactSepByByte_" $ do
                it "splits on separator" testCompactSepByByte_
            describe "compactEndByByte_" $ do
                it "splits on suffix" testCompactEndByByte_
            describe "compactEndByLn_" $ do
                it "splits on newline" testCompactEndByLn_
            describe "createOfLast" $ do
                prop "last n elements" testCreateOfLast
            describe "serialize/deserialize" $ do
                it "roundtrip"       testSerializeDeserialize
                it "multiple values" testSerializeDeserializeMultiple
            testUnboxInstanceExistance
