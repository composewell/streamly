
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

testFromStream :: IO ()
testFromStream = do
    arr <- Arr.fromStream $ Stream.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Arr.read arr
    lst `shouldBe` [1..5]

testFromStreamN :: IO ()
testFromStreamN = do
    arr <- Arr.fromStreamN 5 $ Stream.fromList ([1..10] :: [Int])
    lst <- Stream.fold Fold.toList $ Arr.read arr
    lst `shouldBe` [1..5]

testCreateOf :: IO ()
testCreateOf = do
    arr <- Stream.fold (Arr.createOf 5) $ Stream.fromList ([1..10] :: [Int])
    lst <- Stream.fold Fold.toList $ Arr.read arr
    lst `shouldBe` [1..5]
    Arr.length arr `shouldBe` 5

testCreate :: IO ()
testCreate = do
    arr <- Stream.fold Arr.create $ Stream.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Arr.read arr
    lst `shouldBe` [1..5]

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

testLengthCommon :: IO ()
testLengthCommon = do
    arr <- Arr.fromStream $ Stream.fromList ([1..10] :: [Int])
    Arr.length arr `shouldBe` 10

-------------------------------------------------------------------------------
-- Reading / Streaming
-------------------------------------------------------------------------------

testRead :: IO ()
testRead = do
    arr <- Arr.fromStream $ Stream.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Arr.read arr
    lst `shouldBe` [1..5]

testReadRev :: IO ()
testReadRev = do
    arr <- Arr.fromStream $ Stream.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Arr.readRev arr
    lst `shouldBe` [5, 4, 3, 2, 1]

testReader :: IO ()
testReader = do
    arr <- Arr.fromStream $ Stream.fromList ([1..5] :: [Int])
    lst <- Stream.fold Fold.toList $ Stream.unfold Arr.reader arr
    lst `shouldBe` [1..5]

-------------------------------------------------------------------------------
-- Stream of arrays
-------------------------------------------------------------------------------

testChunksOf :: IO ()
testChunksOf = do
    chunks <-
        Stream.fold Fold.toList
            $ Arr.chunksOf 3
            $ Stream.fromList ([1..10] :: [Int])
    lsts <- mapM (Stream.fold Fold.toList . Arr.read) chunks
    lsts `shouldBe` [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]

-- Tests common to all array variants: unboxed/boxed and mutable/immutable. Only
-- tests whose functions are present and have the same (monadic) shape in every
-- variant should be added here. Arrays are constructed and read back via the
-- stream API so the same code works for both the pure immutable Array and the
-- monadic MutArray. See MutArray/TypeCommon.hs for tests specific to the mutable
-- variants.
arrayCommon :: SpecWith ()
arrayCommon = do
    -- Construction
    it "fromStream" testFromStream
    it "fromStreamN" testFromStreamN
    describe "createOf" $ do
        it "takes n elements" testCreateOf
    it "create" testCreate
    -- Size
    it "length" testLengthCommon
    -- Reading
    it "read" testRead
    it "readRev" testReadRev
    it "reader" testReader
    -- Stream of arrays
    it "chunksOf" testChunksOf
