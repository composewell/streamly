
-- Coverage build takes too long with default number of tests
maxTestCount :: Int
maxTestCount = 100

unsafeWriteIndex :: [Int] -> Int -> Int -> IO Bool
unsafeWriteIndex xs i x = do
    arr <- MArray.fromList xs
    MArray.unsafePutIndex i arr x
    x1 <- MArray.unsafeGetIndex i arr
    return $ x1 == x

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

commonMain :: SpecWith ()
commonMain = do
    describe "unsafePutIndex" $ do
        it "first"  (unsafeWriteIndex [1..10] 0 0 `shouldReturn` True)
        it "middle" (unsafeWriteIndex [1..10] 5 0 `shouldReturn` True)
        it "last"   (unsafeWriteIndex [1..10] 9 0 `shouldReturn` True)
    describe "dropAround" $ do
        it "both sides" (testDropAround      `shouldReturn` True)
        it "left only"  (testDropAroundLeft  `shouldReturn` True)
        it "right only" (testDropAroundRight `shouldReturn` True)
        it "no match"   (testDropAroundNone  `shouldReturn` True)
        it "all match"  (testDropAroundAll   `shouldReturn` True)
        it "empty"      (testDropAroundEmpty `shouldReturn` True)
