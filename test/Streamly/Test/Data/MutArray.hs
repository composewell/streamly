module Streamly.Test.Data.MutArray (main) where

import Test.QuickCheck (listOf)

import Control.Monad (void)
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Foreign.Ptr (IntPtr, WordPtr)
import GHC.Exts
import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Real (Ratio(..))
import GHC.Stable (StablePtr(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Test.Common (chooseInt)
import Test.Hspec (hspec, describe, it)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, Property)
import Test.QuickCheck.Monadic (monadicIO, assert)
#if MIN_VERSION_base(4,15,0)
import GHC.RTS.Flags (IoSubSystem(..))
#endif

import qualified Streamly.Internal.Data.MutArray as MArray
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
            x <- Stream.fold
                    (MArray.writeAppend (MArray.pinnedNew 0))
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

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe moduleName $ do
            describe "Stream Append" $ do
                prop "testAppend" testAppend
            testUnboxInstanceExistance
