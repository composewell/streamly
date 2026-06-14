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
            testUnboxInstanceExistance
