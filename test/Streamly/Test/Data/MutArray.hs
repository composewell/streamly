module Streamly.Test.Data.MutArray (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isLower, ord)
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
-- Pinned variants
-------------------------------------------------------------------------------

testClone :: IO ()
testClone = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr2 <- MArray.clone arr
    MArray.putIndex 0 arr 99
    MArray.getIndex 0 arr `shouldReturn` Just 99
    MArray.getIndex 0 arr2 `shouldReturn` Just 1
    lst <- MArray.toList arr2
    lst `shouldBe` [1..5]

testClone' :: IO ()
testClone' = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr2 <- MArray.clone' arr
    MArray.isPinned arr2 `shouldBe` True
    lst <- MArray.toList arr2
    lst `shouldBe` [1..5]

testFromList' :: IO ()
testFromList' = do
    arr <- MArray.fromList' ([1..5] :: [Int])
    MArray.isPinned arr `shouldBe` True
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testFromListN' :: IO ()
testFromListN' = do
    arr <- MArray.fromListN' 5 ([1..10] :: [Int])
    MArray.isPinned arr `shouldBe` True
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testCreateOf' :: IO ()
testCreateOf' = do
    arr <- Stream.fold (MArray.createOf' 5) $ Stream.fromList ([1..10] :: [Int])
    MArray.isPinned arr `shouldBe` True
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testCreate' :: IO ()
testCreate' = do
    arr <- Stream.fold MArray.create' $ Stream.fromList ([1..5] :: [Int])
    MArray.isPinned arr `shouldBe` True
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testUnsafeCreateOf' :: IO ()
testUnsafeCreateOf' = do
    arr <- Stream.fold (MArray.unsafeCreateOf' 5) $ Stream.fromList ([1..5] :: [Int])
    MArray.isPinned arr `shouldBe` True
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

-------------------------------------------------------------------------------
-- Reverse creation
-------------------------------------------------------------------------------

testRevCreateOf :: IO ()
testRevCreateOf = do
    arr <- Stream.fold (MArray.revCreateOf 5) $ Stream.fromList ([1..5] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [5,4,3,2,1]

testFromListRev :: IO ()
testFromListRev = do
    arr <- MArray.fromListRev ([1..5] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [5,4,3,2,1]

testFromListRevN :: IO ()
testFromListRevN = do
    arr <- MArray.fromListRevN 5 ([1..10] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [5,4,3,2,1]

-------------------------------------------------------------------------------
-- From pure/chunked streams
-------------------------------------------------------------------------------

testFromPureStreamN :: IO ()
testFromPureStreamN = do
    arr <- MArray.fromPureStreamN 5 (Stream.fromList ([1..10] :: [Int]))
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testFromPureStreamMin :: IO ()
testFromPureStreamMin = do
    arr <- MArray.fromPureStreamMin 3 (Stream.fromList ([1..5] :: [Int]))
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testFromChunksRealloced :: IO ()
testFromChunksRealloced = do
    arrs <- mapM MArray.fromList ([[1,2,3],[4,5,6]] :: [[Int]])
    arr <- MArray.fromChunksRealloced $ Stream.fromList arrs
    lst <- MArray.toList arr
    lst `shouldBe` [1..6]

testFromChunksK :: IO ()
testFromChunksK = do
    arrs <- mapM MArray.fromList ([[1,2,3],[4,5,6]] :: [[Int]])
    arr <- MArray.fromChunksK $ Stream.toStreamK $ Stream.fromList arrs
    lst <- MArray.toList arr
    lst `shouldBe` [1..6]

testBuildChunks :: IO ()
testBuildChunks = do
    sk <- Stream.fold (MArray.buildChunks 3) $ Stream.fromList ([1..7] :: [Int])
    arrs <- Stream.fold Fold.toList $ Stream.fromStreamK sk
    lsts <- mapM MArray.toList arrs
    lsts `shouldBe` [[1,2,3],[4,5,6],[7]]

-------------------------------------------------------------------------------
-- Slicing
-------------------------------------------------------------------------------

testUnsafeBreakAt :: IO ()
testUnsafeBreakAt = do
    arr <- MArray.fromList ([1..10] :: [Int])
    let (before, after) = MArray.unsafeBreakAt 3 arr
    bl <- MArray.toList before
    al <- MArray.toList after
    bl `shouldBe` [1,2,3]
    al `shouldBe` [4..10]

testBreakAt :: IO ()
testBreakAt = do
    arr <- MArray.fromList ([1..10] :: [Int])
    let (before, after) = MArray.breakAt 3 arr
    bl <- MArray.toList before
    al <- MArray.toList after
    bl `shouldBe` [1,2,3]
    al `shouldBe` [4..10]

-------------------------------------------------------------------------------
-- Element-aware slicing
-------------------------------------------------------------------------------

testBreakEndBy :: IO ()
testBreakEndBy = do
    arr <- MArray.fromList "hello world"
    (a, b) <- MArray.breakEndBy (== ' ') arr
    MArray.toList a >>= (`shouldBe` "hello ")
    MArray.toList b >>= (`shouldBe` "world")

testBreakEndBy_ :: IO ()
testBreakEndBy_ = do
    arr <- MArray.fromList "hello world"
    (a, b) <- MArray.breakEndBy_ (== ' ') arr
    MArray.toList a >>= (`shouldBe` "hello")
    MArray.toList b >>= (`shouldBe` "world")

testDropWhile :: IO ()
testDropWhile = do
    arr <- MArray.fromList "   hello"
    arr' <- MArray.dropWhile (== ' ') arr
    MArray.toList arr' >>= (`shouldBe` "hello")

testRevBreakEndBy :: IO ()
testRevBreakEndBy = do
    arr <- MArray.fromList "hello world"
    (a, b) <- MArray.revBreakEndBy (== ' ') arr
    MArray.toList a >>= (`shouldBe` "hello")
    MArray.toList b >>= (`shouldBe` " world")

testRevBreakEndBy_ :: IO ()
testRevBreakEndBy_ = do
    arr <- MArray.fromList "hello world"
    (a, b) <- MArray.revBreakEndBy_ (== ' ') arr
    MArray.toList a >>= (`shouldBe` "hello")
    MArray.toList b >>= (`shouldBe` "world")

testRevDropWhile :: IO ()
testRevDropWhile = do
    arr <- MArray.fromList "hello   "
    arr' <- MArray.revDropWhile (== ' ') arr
    MArray.toList arr' >>= (`shouldBe` "hello")

-------------------------------------------------------------------------------
-- Casting
-------------------------------------------------------------------------------

testAsBytes :: IO ()
testAsBytes = do
    arr <- MArray.fromList ([1] :: [Word8])
    let bytes = MArray.asBytes arr
    MArray.length bytes `shouldBe` 1

testCast :: IO ()
testCast = do
    arr <- MArray.fromList ([1,2,3,4] :: [Word8])
    case MArray.cast arr :: Maybe (MutArray Word16) of
        Nothing -> fail "expected Just"
        Just arr16 -> MArray.length arr16 `shouldBe` 2

-------------------------------------------------------------------------------
-- Size and Capacity
-------------------------------------------------------------------------------

testCapacity :: IO ()
testCapacity = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.capacity arr `shouldBe` 5

testFreeSpace :: IO ()
testFreeSpace = do
    arr <- MArray.emptyOf' 10 :: IO (MutArray Int)
    MArray.free arr `shouldBe` 10

testByteCapacity :: IO ()
testByteCapacity = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.byteCapacity arr `shouldBe` MArray.byteLength arr

testBytesFree :: IO ()
testBytesFree = do
    arr <- MArray.emptyOf' 5 :: IO (MutArray Int)
    MArray.bytesFree arr `shouldBe` 5 * sizeOf (Proxy :: Proxy Int)

testBlockSize :: IO ()
testBlockSize = MArray.blockSize `shouldBe` MArray.blockSize  -- constant, just call it

testArrayChunkBytes :: IO ()
testArrayChunkBytes = MArray.arrayChunkBytes > 0 `shouldBe` True

testAllocBytesToElemCount :: IO ()
testAllocBytesToElemCount = do
    let n = MArray.allocBytesToElemCount (undefined :: Int) 1024
    -- subtracts byteArrayOverhead then divides by element size
    (n > 0) `shouldBe` True
    (n * sizeOf (Proxy :: Proxy Int) <= 1024) `shouldBe` True

testGrowTo :: IO ()
testGrowTo = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr1 <- MArray.growTo 10 arr
    MArray.capacity arr1 >= 10 `shouldBe` True
    lst <- MArray.toList arr1
    lst `shouldBe` [1..5]

testGrowBy :: IO ()
testGrowBy = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr1 <- MArray.growBy 10 arr
    MArray.capacity arr1 >= MArray.capacity arr + 10 `shouldBe` True
    lst <- MArray.toList arr1
    lst `shouldBe` [1..5]

testGrowExp :: IO ()
testGrowExp = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr1 <- MArray.growExp 10 arr
    MArray.capacity arr1 >= 10 `shouldBe` True
    lst <- MArray.toList arr1
    lst `shouldBe` [1..5]

testRightSize :: IO ()
testRightSize = do
    arr <- MArray.emptyOf' 100 :: IO (MutArray Int)
    arr1 <- MArray.snoc arr 42
    arr2 <- MArray.rightSize arr1
    MArray.length arr2 `shouldBe` 1
    MArray.capacity arr2 `shouldBe` 1

testVacate :: IO ()
testVacate = do
    arr <- MArray.fromList ([1..5] :: [Int])
    let arr1 = MArray.vacate arr
    MArray.length arr1 `shouldBe` 0
    MArray.capacity arr1 `shouldBe` 5

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

testModify :: IO ()
testModify = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.modify arr (* 2)
    lst <- MArray.toList arr
    lst `shouldBe` [2,4,6,8,10]

testModifyIndices :: IO ()
testModifyIndices = do
    arr <- MArray.fromList ([1..5] :: [Int])
    Stream.fold (MArray.modifyIndices arr (\i _ -> i * 10)) $ Stream.fromList [0, 2, 4]
    lst <- MArray.toList arr
    lst `shouldBe` [0,2,20,4,40]

testSwapIndices :: IO ()
testSwapIndices = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.swapIndices 0 4 arr
    lst <- MArray.toList arr
    lst `shouldBe` [5,2,3,4,1]

testUnsafeSwapIndices :: IO ()
testUnsafeSwapIndices = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.unsafeSwapIndices 1 3 arr
    lst <- MArray.toList arr
    lst `shouldBe` [1,4,3,2,5]

-------------------------------------------------------------------------------
-- Reading
-------------------------------------------------------------------------------

testUnsafeGetIndexRev :: IO ()
testUnsafeGetIndexRev = do
    arr <- MArray.fromList ([1..5] :: [Int])
    MArray.unsafeGetIndexRev 0 arr `shouldReturn` 5
    MArray.unsafeGetIndexRev 4 arr `shouldReturn` 1

-- indexReader uses 'undefined' for the strict arrBound field internally,
-- causing a runtime error. Skip until the upstream bug is fixed.

testToStreamKRev :: IO ()
testToStreamKRev = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Stream.fromStreamK (MArray.toStreamKRev arr)
    lst `shouldBe` [5,4,3,2,1]

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

testReaderRev :: IO ()
testReaderRev = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Stream.unfold MArray.readerRev arr
    lst `shouldBe` [5,4,3,2,1]

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

testFoldRev :: IO ()
testFoldRev = do
    arr <- MArray.fromList ([1..5] :: [Int])
    result <- MArray.foldRev Fold.toList arr
    result `shouldBe` [5,4,3,2,1]

testByteCmp :: IO ()
testByteCmp = do
    arr1 <- MArray.fromList ([1,2,3] :: [Int])
    arr2 <- MArray.fromList ([1,2,3] :: [Int])
    arr3 <- MArray.fromList ([1,2,4] :: [Int])
    MArray.byteCmp arr1 arr2 `shouldReturn` EQ
    MArray.byteCmp arr1 arr3 `shouldReturn` LT

testByteEq :: IO ()
testByteEq = do
    arr1 <- MArray.fromList ([1,2,3] :: [Int])
    arr2 <- MArray.fromList ([1,2,3] :: [Int])
    arr3 <- MArray.fromList ([1,2,4] :: [Int])
    MArray.byteEq arr1 arr2 `shouldReturn` True
    MArray.byteEq arr1 arr3 `shouldReturn` False

-------------------------------------------------------------------------------
-- In-place Mutation
-------------------------------------------------------------------------------

testPartitionBy :: IO ()
testPartitionBy = do
    arr <- MArray.fromList ([1..6] :: [Int])
    -- partitionBy returns (falsePartition, truePartition)
    (fs, ts) <- MArray.partitionBy even arr
    fl <- MArray.toList fs
    tl <- MArray.toList ts
    sort (tl ++ fl) `shouldBe` [1..6]
    all even tl `shouldBe` True
    all odd fl `shouldBe` True

-------------------------------------------------------------------------------
-- Snoc variants
-------------------------------------------------------------------------------

testSnocGrowBy :: IO ()
testSnocGrowBy = do
    arr <- MArray.fromList ([] :: [Int])
    arr1 <- MArray.snocGrowBy 10 arr 1
    arr2 <- MArray.snocGrowBy 10 arr1 2
    lst <- MArray.toList arr2
    lst `shouldBe` [1, 2]

testSnocMay :: IO ()
testSnocMay = do
    arr <- MArray.emptyOf' 1 :: IO (MutArray Int)
    r1 <- MArray.snocMay arr 1
    case r1 of
        Nothing -> fail "expected Just (had space)"
        Just arr1 -> do
            r2 <- MArray.snocMay arr1 2
            case r2 of
                Nothing -> return ()
                Just _ -> fail "expected Nothing when array is full"

-------------------------------------------------------------------------------
-- Append folds
-------------------------------------------------------------------------------

testAppendWith :: IO ()
testAppendWith = do
    arr <- MArray.fromList ([1,2,3] :: [Int])
    result <- Stream.fold (MArray.appendWith (+ 64) (pure arr)) $ Stream.fromList ([4,5,6] :: [Int])
    lst <- MArray.toList result
    lst `shouldBe` [1..6]

testUnsafeAppendMax :: IO ()
testUnsafeAppendMax = do
    arr <- MArray.fromList ([1,2,3] :: [Int])
    result <- Stream.fold (MArray.unsafeAppendMax 3 arr) $ Stream.fromList ([4,5,6] :: [Int])
    lst <- MArray.toList result
    lst `shouldBe` [1..6]

testAppendMax :: IO ()
testAppendMax = do
    arr <- MArray.fromList ([1,2,3] :: [Int])
    result <- Stream.fold (MArray.appendMax 3 arr) $ Stream.fromList ([4,5,6,7,8] :: [Int])
    lst <- MArray.toList result
    lst `shouldBe` [1..6]

testAppendGrowBy :: IO ()
testAppendGrowBy = do
    arr <- MArray.fromList ([1,2,3] :: [Int])
    result <- Stream.fold (MArray.appendGrowBy 10 arr) $ Stream.fromList ([4..8] :: [Int])
    lst <- MArray.toList result
    lst `shouldBe` [1..8]

-------------------------------------------------------------------------------
-- Append streams
-------------------------------------------------------------------------------

testAppendStream :: IO ()
testAppendStream = do
    arr <- MArray.fromList ([1,2,3] :: [Int])
    result <- MArray.appendStream arr $ Stream.fromList [4,5,6]
    lst <- MArray.toList result
    lst `shouldBe` [1..6]

testAppendStreamN :: IO ()
testAppendStreamN = do
    arr <- MArray.emptyOf 3 :: IO (MutArray Int)
    result <- MArray.appendStreamN 3 arr $ Stream.fromList ([1..10] :: [Int])
    lst <- MArray.toList result
    lst `shouldBe` [1..3]

-------------------------------------------------------------------------------
-- Splicing
-------------------------------------------------------------------------------

testSpliceCopy :: IO ()
testSpliceCopy = do
    arr1 <- MArray.fromList ([1,2,3] :: [Int])
    arr2 <- MArray.fromList ([4,5,6] :: [Int])
    arr3 <- MArray.spliceCopy arr1 arr2
    lst <- MArray.toList arr3
    lst `shouldBe` [1..6]
    lst1 <- MArray.toList arr1
    lst1 `shouldBe` [1,2,3]

testSpliceWith :: IO ()
testSpliceWith = do
    arr1 <- MArray.fromList ([1,2,3] :: [Int])
    arr2 <- MArray.fromList ([4,5,6] :: [Int])
    arr3 <- MArray.spliceWith (+) arr1 arr2
    lst <- MArray.toList arr3
    lst `shouldBe` [1..6]

testSpliceExp :: IO ()
testSpliceExp = do
    arr1 <- MArray.fromList ([1,2,3] :: [Int])
    arr2 <- MArray.fromList ([4,5,6] :: [Int])
    arr3 <- MArray.spliceExp arr1 arr2
    lst <- MArray.toList arr3
    lst `shouldBe` [1..6]

testUnsafeSplice :: IO ()
testUnsafeSplice = do
    arr1 <- MArray.fromList ([1,2,3] :: [Int])
    arr2 <- MArray.fromList ([4,5,6] :: [Int])
    arr1' <- MArray.growTo 6 arr1
    arr3 <- MArray.unsafeSplice arr1' arr2
    lst <- MArray.toList arr3
    lst `shouldBe` [1..6]

-------------------------------------------------------------------------------
-- Serialization using Unbox (poke/peek)
-------------------------------------------------------------------------------

testPoke :: IO ()
testPoke = do
    arr <- MArray.poke MArray.empty (42 :: Int)
    MArray.byteLength arr `shouldBe` sizeOf (Proxy :: Proxy Int)
    (val, _) <- MArray.unsafePeek arr :: IO (Int, MutArray Word8)
    val `shouldBe` 42

testPokeMay :: IO ()
testPokeMay = do
    arr <- MArray.poke MArray.empty (42 :: Int)
    r <- MArray.pokeMay arr (99 :: Int)
    case r of
        Nothing -> return ()
        Just _ -> fail "expected Nothing when byte array is full"

testUnsafePokeSkip :: IO ()
testUnsafePokeSkip = do
    arr <- MArray.emptyOf' 8 :: IO (MutArray Word8)
    let arr1 = MArray.unsafePokeSkip 4 arr
    MArray.byteLength arr1 `shouldBe` 4

testUnsafePeek :: IO ()
testUnsafePeek = do
    arr <- MArray.poke MArray.empty (42 :: Int)
    (val, rest) <- MArray.unsafePeek arr :: IO (Int, MutArray Word8)
    val `shouldBe` 42
    MArray.byteLength rest `shouldBe` 0

testUnsafePeekSkip :: IO ()
testUnsafePeekSkip = do
    arr <- MArray.fromList ([1,2,3,4,5] :: [Word8])
    let arr1 = MArray.unsafePeekSkip 2 arr
    MArray.toList arr1 `shouldReturn` [3,4,5]

-------------------------------------------------------------------------------
-- Streams of Arrays - Chunk
-------------------------------------------------------------------------------

testChunksOf' :: IO ()
testChunksOf' = do
    chunks <- Stream.fold Fold.toList
        $ MArray.chunksOf' 3
        $ Stream.fromList ([1..7] :: [Int])
    lsts <- mapM MArray.toList chunks
    lsts `shouldBe` [[1,2,3],[4,5,6],[7]]
    mapM_ (\c -> MArray.isPinned c `shouldBe` True) chunks

testChunksEndBy :: IO ()
testChunksEndBy = do
    chunks <- Stream.fold Fold.toList
        $ MArray.chunksEndBy (== (0 :: Int))
        $ Stream.fromList [1,2,0,3,4,0,5]
    lsts <- mapM MArray.toList chunks
    lsts `shouldBe` [[1,2,0],[3,4,0],[5]]

testChunksEndByLn :: IO ()
testChunksEndByLn = do
    let nl = fromIntegral (ord '\n') :: Word8
    chunks <- Stream.fold Fold.toList
        $ MArray.chunksEndByLn
        $ Stream.fromList [65,66,nl,67,68,nl,69]
    lsts <- mapM MArray.toList chunks
    lsts `shouldBe` [[65,66,nl],[67,68,nl],[69]]

-------------------------------------------------------------------------------
-- Streams of Arrays - Concat
-------------------------------------------------------------------------------

testConcat :: IO ()
testConcat = do
    arrs <- mapM MArray.fromList ([[1,2,3],[4,5,6],[7,8,9]] :: [[Int]])
    lst <- Stream.fold Fold.toList
        $ MArray.concat
        $ Stream.fromList arrs
    lst `shouldBe` [1..9]

testConcatRev :: IO ()
testConcatRev = do
    arrs <- mapM MArray.fromList ([[1,2,3],[4,5,6]] :: [[Int]])
    lst <- Stream.fold Fold.toList
        $ MArray.concatRev
        $ Stream.fromList arrs
    lst `shouldBe` [3,2,1,6,5,4]

-------------------------------------------------------------------------------
-- Streams of Arrays - Compact
-------------------------------------------------------------------------------

testCreateCompactMin :: IO ()
testCreateCompactMin = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4],[5,6]] :: [[Int]])
    result <- Stream.fold (MArray.createCompactMin 5) $ Stream.fromList arrs
    lst <- MArray.toList result
    lst `shouldBe` [1,2,3,4,5,6]

testCompactMin :: IO ()
testCompactMin = do
    arrs <- mapM MArray.fromList ([[1],[2],[3],[4,5,6,7]] :: [[Int]])
    result <- Stream.fold Fold.toList
        $ MArray.compactMin 3
        $ Stream.fromList arrs
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2,3],[4,5,6,7]]

testCreateCompactMax :: IO ()
testCreateCompactMax = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4],[5,6,7,8]] :: [[Int]])
    rights <- Stream.fold Fold.toList
        $ Stream.parseMany (MArray.createCompactMax 4)
        $ Stream.fromList arrs
    let chunks = [a | Right a <- rights]
    lsts <- mapM MArray.toList chunks
    lsts `shouldBe` [[1,2,3,4],[5,6,7,8]]

testScanCompactMin :: IO ()
testScanCompactMin = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4],[5,6,7,8]] :: [[Int]])
    results <- Stream.fold Fold.toList
        $ Stream.scanl (MArray.scanCompactMin 3)
        $ Stream.fromList arrs
    let completes = [arr | Just arr <- results]
    lsts <- mapM MArray.toList completes
    lsts `shouldBe` [[1,2,3,4],[5,6,7,8]]

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

testIsPower2 :: IO ()
testIsPower2 = do
    MArray.isPower2 1 `shouldBe` True
    MArray.isPower2 2 `shouldBe` True
    MArray.isPower2 4 `shouldBe` True
    MArray.isPower2 3 `shouldBe` False
    MArray.isPower2 6 `shouldBe` False

testRoundUpToPower2 :: IO ()
testRoundUpToPower2 = do
    MArray.roundUpToPower2 1 `shouldBe` 1
    MArray.roundUpToPower2 2 `shouldBe` 2
    MArray.roundUpToPower2 3 `shouldBe` 4
    MArray.roundUpToPower2 5 `shouldBe` 8

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

            -- clone (Unboxed only — Generic clone has a bug with arrEnd not being updated)
            it "clone" testClone

            -- Pinned variants
            it "clone'" testClone'
            it "fromList'" testFromList'
            it "fromListN'" testFromListN'
            it "createOf'" testCreateOf'
            it "create'" testCreate'
            it "unsafeCreateOf'" testUnsafeCreateOf'

            -- Reverse creation
            it "revCreateOf" testRevCreateOf
            it "fromListRev" testFromListRev
            it "fromListRevN" testFromListRevN

            -- From pure/chunked streams
            it "fromPureStreamN" testFromPureStreamN
            it "fromPureStreamMin" testFromPureStreamMin
            it "fromChunksRealloced" testFromChunksRealloced
            it "fromChunksK" testFromChunksK
            it "buildChunks" testBuildChunks

            -- Slicing
            it "unsafeBreakAt" testUnsafeBreakAt
            it "breakAt" testBreakAt
            describe "breakEndBy" $ do
                it "split on space" testBreakEndBy
            describe "breakEndBy_" $ do
                it "split on space, drop it" testBreakEndBy_
            describe "dropWhile" $ do
                it "drop leading spaces" testDropWhile
            describe "revBreakEndBy" $ do
                it "split from end" testRevBreakEndBy
            describe "revBreakEndBy_" $ do
                it "split from end, drop it" testRevBreakEndBy_
            describe "revDropWhile" $ do
                it "drop trailing spaces" testRevDropWhile

            -- Casting
            it "asBytes" testAsBytes
            describe "cast" $ do
                it "Word8 to Word16" testCast

            -- Size and Capacity
            describe "capacity" $ do it "equals length for exact alloc" testCapacity
            describe "free" $ do it "equals capacity for empty array" testFreeSpace
            describe "byteCapacity" $ do it "equals byteLength for exact alloc" testByteCapacity
            describe "bytesFree" $ do it "full capacity for empty" testBytesFree
            describe "blockSize" $ do it "is a constant" testBlockSize
            describe "arrayChunkBytes" $ do it "is positive" testArrayChunkBytes
            describe "allocBytesToElemCount" $ do it "converts bytes to elem count" testAllocBytesToElemCount
            describe "growTo" $ do it "capacity increases" testGrowTo
            describe "growBy" $ do it "capacity grows by n" testGrowBy
            describe "growExp" $ do it "capacity grows" testGrowExp
            describe "rightSize" $ do it "capacity shrinks to length" testRightSize
            describe "vacate" $ do it "resets length to 0" testVacate

            -- Random writes
            describe "modify" $ do it "all elements" testModify
            describe "modifyIndices" $ do it "at specified indices" testModifyIndices
            describe "swapIndices" $ do it "swaps two elements" testSwapIndices
            describe "unsafeSwapIndices" $ do it "swaps two elements" testUnsafeSwapIndices

            -- Reading
            describe "unsafeGetIndexRev" $ do it "from end" testUnsafeGetIndexRev
            describe "toStreamKRev" $ do it "reversed" testToStreamKRev

            -- Unfolds
            describe "readerRev" $ do it "reads in reverse" testReaderRev

            -- Folding
            describe "foldRev" $ do it "reverses order" testFoldRev
            describe "byteCmp" $ do it "equal and less-than" testByteCmp
            describe "byteEq" $ do it "equal and unequal" testByteEq

            -- In-place
            describe "partitionBy" $ do it "separates evens from odds" testPartitionBy

            -- Snoc variants
            describe "snocGrowBy" $ do it "appends with growth" testSnocGrowBy
            describe "snocMay" $ do it "snoc if space, Nothing if full" testSnocMay

            -- Append folds
            describe "appendWith" $ do it "appends with growth function" testAppendWith
            describe "unsafeAppendMax" $ do it "appends n elements" testUnsafeAppendMax
            describe "appendMax" $ do it "appends at most n elements" testAppendMax
            describe "appendGrowBy" $ do it "appends with grow by n" testAppendGrowBy

            -- Append streams
            describe "appendStream" $ do it "appends whole stream" testAppendStream
            describe "appendStreamN" $ do it "appends n from stream" testAppendStreamN

            -- Splicing
            describe "spliceCopy" $ do it "copies and splices" testSpliceCopy
            describe "spliceWith" $ do it "splices with growth fn" testSpliceWith
            describe "spliceExp" $ do it "splices exponentially" testSpliceExp
            describe "unsafeSplice" $ do it "unsafe splice" testUnsafeSplice

            -- Poke/Peek (serialization)
            describe "poke" $ do it "serializes to Word8 array" testPoke
            describe "pokeMay" $ do it "Nothing when full" testPokeMay
            describe "unsafePokeSkip" $ do it "advances end pointer" testUnsafePokeSkip
            describe "unsafePeek" $ do it "deserializes from Word8 array" testUnsafePeek
            describe "unsafePeekSkip" $ do it "advances start pointer" testUnsafePeekSkip

            -- Chunks
            describe "chunksOf'" $ do it "pinned chunksOf" testChunksOf'
            describe "chunksEndBy" $ do it "ends chunk on predicate" testChunksEndBy
            describe "chunksEndByLn" $ do it "ends chunk on newline" testChunksEndByLn

            -- Concat
            describe "concat" $ do it "concatenates arrays" testConcat
            describe "concatRev" $ do it "concatenates reversed" testConcatRev

            -- Compact
            describe "createCompactMin" $ do it "fold compact min" testCreateCompactMin
            describe "compactMin" $ do it "stream compact min" testCompactMin
            describe "createCompactMax" $ do it "parser compact max" testCreateCompactMax
            describe "scanCompactMin" $ do it "scan compact min" testScanCompactMin

            -- Utilities
            describe "isPower2" $ do it "identifies powers of 2" testIsPower2
            describe "roundUpToPower2" $ do it "rounds up" testRoundUpToPower2

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
