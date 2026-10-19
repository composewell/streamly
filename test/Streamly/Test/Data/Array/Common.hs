
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

-- No empty array at the end when the stream length is a multiple of the chunk
-- size or the stream is empty.
testChunksOfExact :: IO ()
testChunksOfExact = do
    let chunks xs =
            map A.toList
                <$> S.fold Fold.toList (A.chunksOf 3 (S.fromList xs))
    chunks ([1..9] :: [Int]) `shouldReturn` [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    chunks ([] :: [Int]) `shouldReturn` []

commonMain :: SpecWith ()
commonMain = do
    describe "Stream of arrays" $ do
        it "chunksOf exact multiple" testChunksOfExact
    describe "Construction" $ do
        prop "length . writeN n === n" testLength
        prop "length . fromStreamN n === n" testLengthFromStreamN
        prop "read . writeN === id " testFoldNUnfold
        prop "toStream . writeN === id" testFoldNToStream
        prop "toStreamRev . writeN === reverse" testFoldNToStreamRev
        prop "read . fromStreamN === id" testFromStreamNUnfold
        prop "toStream . fromStreamN === id" testFromStreamNToStream
        prop "fromListN" testFromListN
        prop "foldMany with writeN concats to original"
            (foldManyWith A.createOf)
