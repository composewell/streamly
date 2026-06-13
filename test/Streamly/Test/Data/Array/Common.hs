
#include "MachDeps.h"

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

genericTestFrom ::
       (Int -> Stream IO Int -> IO (Array Int))
    -> Property
genericTestFrom arrFold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- run $ arrFold len $ S.fromList list
                assert (A.length arr == len)

testLength :: Property
testLength = genericTestFrom (S.fold . A.createOf)

testLengthFromStreamN :: Property
testLengthFromStreamN = genericTestFrom A.fromStreamN

genericTestFromTo ::
       (Int -> Stream IO Int -> IO (Array Int))
    -> (Array Int -> Stream IO Int)
    -> ([Int] -> [Int] -> Bool)
    -> Property
genericTestFromTo arrFold arrUnfold listEq =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- run $ arrFold len $ S.fromList list
                xs <- run $ S.fold Fold.toList $ arrUnfold arr
                assert (listEq xs list)


testFoldNUnfold :: Property
testFoldNUnfold =
    genericTestFromTo (S.fold . A.createOf) (S.unfold A.reader) (==)

testFoldNToStream :: Property
testFoldNToStream =
    genericTestFromTo (S.fold . A.createOf) A.read (==)

testFoldNToStreamRev :: Property
testFoldNToStreamRev =
    genericTestFromTo
        (S.fold . A.createOf)
        A.readRev
        (\xs list -> xs == reverse list)

testFromStreamNUnfold :: Property
testFromStreamNUnfold =
    genericTestFromTo A.fromStreamN (S.unfold A.reader) (==)

testFromStreamNToStream :: Property
testFromStreamNToStream = genericTestFromTo A.fromStreamN A.read (==)

testFromListN :: Property
testFromListN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromListN n list
                    xs <- run $ S.fold Fold.toList $ S.unfold A.reader arr
                    listEquals (==) xs (take n list)

foldManyWith :: (Int -> Fold IO Int (Array Int)) -> Property
foldManyWith f =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                xs <- run
                    $ S.fold Fold.toList
                    $ S.unfoldEach A.reader
                    $ S.foldMany (f 240)
                    $ S.fromList list
                assert (xs == list)

testWrite :: [Char] -> IO ()
testWrite inp = do
    arr <- S.fold A.create (S.fromList inp)
    A.toList arr `shouldBe` inp

testFromToList :: [Char] -> IO ()
testFromToList inp = A.toList (A.fromList inp) `shouldBe` inp

testFoldUnfold :: Property
testFoldUnfold =
    genericTestFromTo (const (S.fold A.create)) (S.unfold A.reader) (==)

testFromList :: Property
testFromList =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    xs <- run $ S.fold Fold.toList $ S.unfold A.reader arr
                    assert (xs == list)

testLengthFromStream :: Property
testLengthFromStream = genericTestFrom (const A.fromStream)

testFromStreamToStream :: Property
testFromStreamToStream = genericTestFromTo (const A.fromStream) A.read (==)

testReadShowInstance :: Property
testReadShowInstance =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    assert (A.toList (read (show arr)) == list)

commonMain :: SpecWith ()
commonMain = do
    describe "createOf" $ do
        prop "length . createOf n === n" testLength
        prop "reader . createOf === id" testFoldNUnfold
        prop "read . createOf === id" testFoldNToStream
        prop "readRev . createOf === reverse" testFoldNToStreamRev
        prop "foldMany concats to original" (foldManyWith A.createOf)
    describe "create" $ do
        prop "reader . create === id" testFoldUnfold
        it "abc" (testWrite "abc")
        it "\\22407" (testWrite "\22407")
    describe "fromStreamN" $ do
        prop "length . fromStreamN n === n" testLengthFromStreamN
        prop "reader . fromStreamN === id" testFromStreamNUnfold
        prop "read . fromStreamN === id" testFromStreamNToStream
    describe "fromStream" $ do
        prop "length . fromStream === n" testLengthFromStream
        prop "read . fromStream === id" testFromStreamToStream
    prop "fromListN" testFromListN
    describe "fromList" $ do
        prop "reader . fromList === id" testFromList
        it "abc" (testFromToList "abc")
        it "\\22407" (testFromToList "\22407")
    prop "show/read roundtrip" testReadShowInstance
