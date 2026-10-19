module Streamly.Test.Data.MutArray (main) where

import Test.QuickCheck (listOf)

import Control.Monad (void)
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy(..))
import Foreign.Ptr (IntPtr, WordPtr)
import GHC.Exts
import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Real (Ratio(..))
import GHC.Stable (StablePtr(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import Streamly.Internal.Data.MutByteArray (Unbox, sizeOf)
import Streamly.Test.Common (chooseInt)
import Test.Hspec
    (hspec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property)
import Test.QuickCheck.Monadic (monadicIO, assert)
#if MIN_VERSION_base(4,15,0)
import GHC.IO.SubSystem (IoSubSystem (..))
#endif

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.MutArray.Generic as GArray
import qualified Streamly.Internal.Data.MutByteArray as MBA
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Test.Hspec as Hspec

maxTestCount :: Int
maxTestCount = 100

moduleName :: String
moduleName = "Data.MutArray"

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

-- Appending to a slice starting at a non-zero offset reallocates it, the
-- capacity of the result must be within the new allocation.
testAppendMaxSlice :: IO ()
testAppendMaxSlice = do
    arr <- MArray.fromList ([0..9] :: [Int])
    let slice = MArray.sliceOffLen 5 2 arr
    result <- Stream.fold (MArray.appendMax 3 slice) $ Stream.fromList [7,8,9]
    MArray.toList result `shouldReturn` [5..9]
    n <- MBA.length (MArray.arrContents result)
    MArray.arrBound result `shouldSatisfy` (<= n)
    MArray.byteCapacity result `shouldBe` 5 * sizeOf (Proxy :: Proxy Int)

-- Growing from an initial capacity of 0 must allocate at least one element.
testCreateMinOf0 :: IO ()
testCreateMinOf0 = do
    arr <- Stream.fold (MArray.createMinOf 0) $ Stream.fromList ([1] :: [Int])
    MArray.toList arr `shouldReturn` [1]
    n <- MBA.length (MArray.arrContents arr)
    MArray.arrEnd arr `shouldSatisfy` (<= n)

testClone :: IO ()
testClone = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr2 <- MArray.clone arr
    MArray.putIndex 0 arr 99
    MArray.getIndex 0 arr `shouldReturn` Just 99
    MArray.getIndex 0 arr2 `shouldReturn` Just 1
    lst <- MArray.toList arr2
    lst `shouldBe` [1..5]

testCloneGeneric :: IO ()
testCloneGeneric = do
    arr <- GArray.fromList ([1..5] :: [Int])
    arr2 <- GArray.clone arr
    GArray.putIndex 0 arr 99
    GArray.getIndex 0 arr `shouldReturn` Just 99
    GArray.getIndex 0 arr2 `shouldReturn` Just 1
    lst <- GArray.toList arr2
    lst `shouldBe` [1..5]

testIndexReader :: IO ()
testIndexReader = do
    arr <- MArray.fromList ([10,20,30,40,50] :: [Int])
    let indices = Stream.fromList [4,0,2]
    lst <- Stream.fold Fold.toList $ Stream.unfold (MArray.indexReader indices) arr
    lst `shouldBe` [50,10,30]

-- No empty array at the end when the stream length is a multiple of the chunk
-- size or the stream is empty.
testChunksOfExact :: IO ()
testChunksOfExact = do
    let chunks xs =
            Stream.fold Fold.toList (MArray.chunksOf 3 (Stream.fromList xs))
                >>= mapM MArray.toList
    chunks ([1..9] :: [Int]) `shouldReturn` [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    chunks ([] :: [Int]) `shouldReturn` []

testChunksOfExactGeneric :: IO ()
testChunksOfExactGeneric = do
    let chunks xs =
            Stream.fold Fold.toList (GArray.chunksOf 3 (Stream.fromList xs))
                >>= mapM GArray.toList
    chunks ([1..9] :: [Int]) `shouldReturn` [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    chunks ([] :: [Int]) `shouldReturn` []

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

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream Append" $ do
                prop "testAppend" testAppend
            it "appendMax to a slice" testAppendMaxSlice
            it "createMinOf 0" testCreateMinOf0
            it "clone" testClone
            it "clone (Generic)" testCloneGeneric
            it "indexReader" testIndexReader
            it "chunksOf exact multiple" testChunksOfExact
            it "chunksOf exact multiple (Generic)" testChunksOfExactGeneric
            testUnboxInstanceExistance
