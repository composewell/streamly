
-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

testFromListN :: IO ()
testFromListN = do
    arr <- MArray.fromListN 5 ([1..10] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testUnsafeCreateOf :: IO ()
testUnsafeCreateOf = do
    arr <- Stream.fold (MArray.unsafeCreateOf 5) $ Stream.fromList ([1..5] :: [Int])
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testFromPureStream :: IO ()
testFromPureStream = do
    arr <- MArray.fromPureStream (Stream.fromList ([1..5] :: [Int]))
    lst <- MArray.toList arr
    lst `shouldBe` [1..5]

testClone :: IO ()
testClone = do
    arr <- MArray.fromList ([1..5] :: [Int])
    arr2 <- MArray.clone arr
    MArray.putIndex 0 arr 99
    MArray.getIndex 0 arr `shouldReturn` Just 99
    MArray.getIndex 0 arr2 `shouldReturn` Just 1
    lst <- MArray.toList arr2
    lst `shouldBe` [1..5]

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

testPutIndices :: IO ()
testPutIndices = do
    arr <- MArray.fromList ([1..10] :: [Int])
    Stream.fold (MArray.putIndices arr) $ Stream.fromList [(0, 99), (5, 88)]
    MArray.getIndex 0 arr `shouldReturn` Just 99
    MArray.getIndex 5 arr `shouldReturn` Just 88

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

testSnocWith :: IO ()
testSnocWith = do
    arr <- MArray.emptyOf 2
    arr1 <- MArray.snocWith (+ 64) arr (1 :: Int)
    arr2 <- MArray.snocWith (+ 64) arr1 2
    lst <- MArray.toList arr2
    lst `shouldBe` [1, 2]

testUnsafeSnoc :: IO ()
testUnsafeSnoc = do
    arr <- MArray.emptyOf 2
    arr1 <- MArray.unsafeSnoc arr (1 :: Int)
    arr2 <- MArray.unsafeSnoc arr1 2
    lst <- MArray.toList arr2
    lst `shouldBe` [1, 2]

-------------------------------------------------------------------------------
-- Reading / Streaming
-------------------------------------------------------------------------------

testReaderWith :: IO ()
testReaderWith = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Stream.unfold (MArray.readerWith liftIO) arr
    lst `shouldBe` [1..5]

testToStreamK :: IO ()
testToStreamK = do
    arr <- MArray.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Stream.fromStreamK (MArray.toStreamK arr)
    lst `shouldBe` [1..5]

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

-- Tests common to the mutable array variants MutArray (Unboxed) and
-- MutArray.Generic. Only tests that apply to both mutable variants should be
-- added here. Tests common to all array variants (including the immutable
-- Array) live in MutArray/Common.hs (arrayCommon).
typeCommon :: SpecWith ()
typeCommon = do
    -- Construction
    it "fromListN" testFromListN
    it "unsafeCreateOf" testUnsafeCreateOf
    it "fromPureStream" testFromPureStream
    it "clone" testClone
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
    describe "putIndices" $ do
        it "multiple writes" testPutIndices
    -- Slicing
    it "unsafeSliceOffLen" testUnsafeSliceOffLen
    it "sliceOffLen" testSliceOffLen
    -- Growing
    it "snoc" testSnoc
    it "snocWith" testSnocWith
    it "unsafeSnoc" testUnsafeSnoc
    -- Reading
    it "readerWith" testReaderWith
    it "toStreamK" testToStreamK
    -- In-place mutation
    describe "dropAround" $ do
        it "both sides" (testDropAround      `shouldReturn` True)
        it "left only"  (testDropAroundLeft  `shouldReturn` True)
        it "right only" (testDropAroundRight `shouldReturn` True)
        it "no match"   (testDropAroundNone  `shouldReturn` True)
        it "all match"  (testDropAroundAll   `shouldReturn` True)
        it "empty"      (testDropAroundEmpty `shouldReturn` True)
