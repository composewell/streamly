
-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

testFromListN :: IO ()
testFromListN = do
    arr <- MArray.fromListN 5 ([1..10] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testFromStream :: IO ()
testFromStream = do
    arr <- MArray.fromStream $ Stream.fromList ([1..5] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testFromStreamN :: IO ()
testFromStreamN = do
    arr <- MArray.fromStreamN 5 $ Stream.fromList ([1..10] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testCreateOf :: IO ()
testCreateOf = do
    arr <- Stream.fold (MArray.createOf 5) $ Stream.fromList ([1..10] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]
    MArray.length arr `shouldBe` 5

testCreate :: IO ()
testCreate = do
    arr <- Stream.fold MArray.create $ Stream.fromList ([1..5] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

testLength :: IO ()
testLength = do
    arr <- MArray.fromList ([1..10] :: [Int])
    MArray.length arr `shouldBe` 10

-------------------------------------------------------------------------------
-- Random access
-------------------------------------------------------------------------------

testGetIndex :: IO ()
testGetIndex = do
    arr <- MArray.fromList ([1..10] :: [Int])
    MArray.getIndex 0 arr `shouldReturn` Just 1
    MArray.getIndex 9 arr `shouldReturn` Just 10
    MArray.getIndex 10 arr `shouldReturn` Nothing

testUnsafeGetIndex :: IO ()
testUnsafeGetIndex = do
    arr <- MArray.fromList ([1..10] :: [Int])
    MArray.unsafeGetIndex 0 arr `shouldReturn` 1
    MArray.unsafeGetIndex 9 arr `shouldReturn` 10

testPutIndex :: IO ()
testPutIndex = do
    arr <- MArray.fromList ([1..10] :: [Int])
    MArray.putIndex 0 arr 99
    MArray.getIndex 0 arr `shouldReturn` Just 99

testModifyIndex :: IO ()
testModifyIndex = do
    arr <- MArray.fromList ([1..10] :: [Int])
    _ <- MArray.modifyIndex 0 arr (\x -> (x + 10, ()))
    MArray.getIndex 0 arr `shouldReturn` Just 11

testUnsafeModifyIndex :: IO ()
testUnsafeModifyIndex = do
    arr <- MArray.fromList ([1..10] :: [Int])
    _ <- MArray.unsafeModifyIndex 0 arr (\x -> (x + 10, ()))
    MArray.unsafeGetIndex 0 arr `shouldReturn` 11

unsafeWriteIndex :: [Int] -> Int -> Int -> IO Bool
unsafeWriteIndex xs i x = do
    arr <- MArray.fromList xs
    MArray.unsafePutIndex i arr x
    x1 <- MArray.unsafeGetIndex i arr
    return $ x1 == x

-------------------------------------------------------------------------------
-- Slicing
-------------------------------------------------------------------------------

testUnsafeSliceOffLen :: IO ()
testUnsafeSliceOffLen = do
    arr <- MArray.fromList ([1..10] :: [Int])
    lst <- MArray.toList $ MArray.unsafeSliceOffLen 2 4 arr
    lst `shouldBe` [3..6]

testSliceOffLen :: IO ()
testSliceOffLen = do
    arr <- MArray.fromList ([1..10] :: [Int])
    lst <- MArray.toList $ MArray.sliceOffLen 2 4 arr
    lst `shouldBe` [3..6]

-------------------------------------------------------------------------------
-- Growing
-------------------------------------------------------------------------------

testSnoc :: IO ()
testSnoc = do
    arr0 <- MArray.fromList ([] :: [Int])
    arr1 <- MArray.snoc arr0 1
    arr2 <- MArray.snoc arr1 2
    lst <- MArray.toList arr2
    lst `shouldBe` [1, 2]

-------------------------------------------------------------------------------
-- Reading / Streaming
-------------------------------------------------------------------------------

testRead :: IO ()
testRead = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ MArray.read arr
    lst `shouldBe` [1..5]

testReadRev :: IO ()
testReadRev = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ MArray.readRev arr
    lst `shouldBe` [5,4,3,2,1]

testReader :: IO ()
testReader = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Stream.unfold MArray.reader arr
    lst `shouldBe` [1..5]

-------------------------------------------------------------------------------
-- Stream of arrays
-------------------------------------------------------------------------------

testChunksOf :: IO ()
testChunksOf = do
    chunks <- Stream.fold Fold.toList
        $ MArray.chunksOf 3
        $ Stream.fromList ([1..10] :: [Int])
    lsts <- mapM MArray.toList chunks
    lsts `shouldBe` [[1,2,3],[4,5,6],[7,8,9],[10]]

-------------------------------------------------------------------------------
-- In-place mutation
-------------------------------------------------------------------------------

testDropAround :: IO Bool
testDropAround = do
    dt <- MArray.fromList "abcDEFgeh"
    dt' <- MArray.dropAround isLower dt
    x <- MArray.toList dt'
    return $ x == "DEF"

testDropAroundLeft :: IO Bool
testDropAroundLeft = do
    dt <- MArray.fromList "abcDEF"
    dt' <- MArray.dropAround isLower dt
    x <- MArray.toList dt'
    return $ x == "DEF"

testDropAroundRight :: IO Bool
testDropAroundRight = do
    dt <- MArray.fromList "DEFgeh"
    dt' <- MArray.dropAround isLower dt
    x <- MArray.toList dt'
    return $ x == "DEF"

testDropAroundNone :: IO Bool
testDropAroundNone = do
    dt <- MArray.fromList "DEF"
    dt' <- MArray.dropAround isLower dt
    x <- MArray.toList dt'
    return $ x == "DEF"

testDropAroundAll :: IO Bool
testDropAroundAll = do
    dt <- MArray.fromList "abc"
    dt' <- MArray.dropAround isLower dt
    x <- MArray.toList dt'
    return $ x == ""

testDropAroundEmpty :: IO Bool
testDropAroundEmpty = do
    dt <- MArray.fromList ""
    dt' <- MArray.dropAround isLower dt
    x <- MArray.toList dt'
    return $ x == ""

typeMain :: SpecWith ()
typeMain = do
    -- Construction
    it "fromListN" testFromListN
    it "fromStream" testFromStream
    it "fromStreamN" testFromStreamN
    describe "createOf" $ do
        it "takes n elements" testCreateOf
    it "create" testCreate
    -- Size
    it "length" testLength
    -- Random access
    describe "getIndex" $ do
        it "valid and out of bounds" testGetIndex
    describe "unsafeGetIndex" $ do
        it "valid indices" testUnsafeGetIndex
    describe "putIndex" $ do
        it "writes and reads back" testPutIndex
    describe "modifyIndex" $ do
        it "applies function" testModifyIndex
    describe "unsafeModifyIndex" $ do
        it "applies function" testUnsafeModifyIndex
    describe "unsafePutIndex" $ do
        it "first"  (unsafeWriteIndex [1..10] 0 0 `shouldReturn` True)
        it "middle" (unsafeWriteIndex [1..10] 5 0 `shouldReturn` True)
        it "last"   (unsafeWriteIndex [1..10] 9 0 `shouldReturn` True)
    -- Slicing
    it "unsafeSliceOffLen" testUnsafeSliceOffLen
    it "sliceOffLen" testSliceOffLen
    -- Growing
    it "snoc" testSnoc
    -- Reading
    it "read" testRead
    it "readRev" testReadRev
    it "reader" testReader
    -- Stream of arrays
    it "chunksOf" testChunksOf
    -- In-place mutation
    describe "dropAround" $ do
        it "both sides" (testDropAround      `shouldReturn` True)
        it "left only"  (testDropAroundLeft  `shouldReturn` True)
        it "right only" (testDropAroundRight `shouldReturn` True)
        it "no match"   (testDropAroundNone  `shouldReturn` True)
        it "all match"  (testDropAroundAll   `shouldReturn` True)
        it "empty"      (testDropAroundEmpty `shouldReturn` True)
