--
--  See test/Streamly/Test/Data/Scanl/Type.hs for Scanl/Fold test sharing
--  mechanism.
--
-------------------------------------------------------------------------------
-- Accumulators
-------------------------------------------------------------------------------

lengthS :: [Int] -> Expectation
lengthS ls = check F.length ls (Prelude.scanl (\c _ -> c + 1) (0 :: Int) ls)

genericLengthS :: [Int] -> Expectation
genericLengthS ls =
    check (F.genericLength :: Op IO Int Int) ls
        (Prelude.scanl (\c _ -> c + 1) 0 ls)

toListS :: [Int] -> Expectation
toListS ls = check F.toList ls (Prelude.scanl (\acc x -> acc ++ [x]) [] ls)

latestS :: [Int] -> Expectation
latestS ls = check F.latest ls (Prelude.scanl (\_ x -> Just x) Nothing ls)

drainS :: [Int] -> Expectation
drainS ls = check F.drain ls (Prelude.scanl (\_ _ -> ()) () ls)

maximumS :: [Int] -> Expectation
maximumS ls =
    check F.maximum ls
        (Prelude.scanl (\acc x -> Just (Prelude.maybe x (max x) acc)) Nothing ls)

maximumByS :: [Int] -> Expectation
maximumByS ls =
    check (F.maximumBy compare) ls
        (Prelude.scanl (\acc x -> Just (Prelude.maybe x (max x) acc)) Nothing ls)

minimumS :: [Int] -> Expectation
minimumS ls =
    check F.minimum ls
        (Prelude.scanl (\acc x -> Just (Prelude.maybe x (min x) acc)) Nothing ls)

minimumByS :: [Int] -> Expectation
minimumByS ls =
    check (F.minimumBy compare) ls
        (Prelude.scanl (\acc x -> Just (Prelude.maybe x (min x) acc)) Nothing ls)

rangeS :: [Int] -> Expectation
rangeS ls =
    check F.range ls
        (Prelude.scanl
            (\acc x ->
                Just (Prelude.maybe (x, x) (\(lo, hi) -> (min lo x, max hi x)) acc))
            Nothing
            ls)

rangeByS :: [Int] -> Expectation
rangeByS ls =
    check (F.rangeBy compare) ls
        (Prelude.scanl
            (\acc x ->
                Just (Prelude.maybe (x, x) (\(lo, hi) -> (min lo x, max hi x)) acc))
            Nothing
            ls)

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

lmapS :: [Int] -> Expectation
lmapS ls =
    check (F.lmap (* 2) F.sum) ls (Prelude.scanl (\acc x -> acc + x * 2) 0 ls)

lmapMS :: [Int] -> Expectation
lmapMS ls =
    check (F.lmapM (\x -> return (x * 2)) F.sum) ls
        (Prelude.scanl (\acc x -> acc + x * 2) 0 ls)

rmapMS :: [Int] -> Expectation
rmapMS ls =
    check (F.rmapM (\x -> return (x + Prelude.length ls)) F.sum) ls
        (Prelude.map (+ Prelude.length ls) (Prelude.scanl (+) 0 ls))

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

filterS :: [Int] -> Expectation
filterS ls =
    check (F.filter even F.toList) ls
        (Prelude.scanl (\acc x -> if even x then acc ++ [x] else acc) [] ls)

filterMS :: [Int] -> Expectation
filterMS ls =
    check (F.filterM (return . even) F.toList) ls
        (Prelude.scanl (\acc x -> if even x then acc ++ [x] else acc) [] ls)

filteringS :: [Int] -> Expectation
filteringS ls =
    check (F.filtering even) ls
        (Prelude.scanl (\_ x -> if even x then Just x else Nothing) Nothing ls)

catMaybesS :: Expectation
catMaybesS =
    check
        (F.catMaybes F.toList)
        ([Just 1, Nothing, Just 3, Nothing, Just 5] :: [Maybe Int])
        [[], [1], [1], [1, 3], [1, 3], [1, 3, 5]]

catLeftsS :: Expectation
catLeftsS =
    check
        (F.catLefts F.toList)
        ([Left 1, Right "a", Left 3, Right "b"] :: [Either Int String])
        [[], [1], [1], [1, 3], [1, 3]]

catRightsS :: Expectation
catRightsS =
    check
        (F.catRights F.toList)
        ([Left "a", Right 2, Left "b", Right 4] :: [Either String Int])
        [[], [], [2], [2], [2, 4]]

catEithersS :: Expectation
catEithersS =
    check
        (F.catEithers F.toList)
        ([Left 1, Right 2, Left 3, Right 4] :: [Either Int Int])
        [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]

-------------------------------------------------------------------------------
-- Trimming (terminating scans truncate at the terminating step)
-------------------------------------------------------------------------------

takeS :: [Int] -> Property
takeS ls =
    forAll (chooseInt (-1, len + 2)) $ \n ->
        -- scanl: initial, then one output per consumed input. n<=0 is
        -- Done-at-initial, emitting just the initial extract.
        let sc = if n <= 0
                 then [[]]
                 else [Prelude.take k ls | k <- [0 .. min n len]]
         in check (F.take n F.toList) ls sc
  where
    len = Prelude.length ls

takingS :: Expectation
takingS = do
    check (F.taking 3) ([1, 2, 3, 4, 5] :: [Int]) [Nothing, Just 1, Just 2, Just 3]
    check (F.taking 0) ([1, 2, 3] :: [Int]) [Nothing]
    check (F.taking 3) ([] :: [Int]) [Nothing]

droppingS :: Expectation
droppingS = do
    check (F.dropping 2) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Nothing, Nothing, Just 3, Just 4, Just 5]
    check (F.dropping 0) ([1, 2, 3] :: [Int])
        [Nothing, Just 1, Just 2, Just 3]
    check (F.dropping 10) ([1, 2, 3] :: [Int])
        [Nothing, Nothing, Nothing, Nothing]

takeEndBy_S :: Expectation
takeEndBy_S =
    check (F.takeEndBy_ (== 1) F.toList) ([3, 2, 1, 4] :: [Int])
        [[], [3], [3, 2], [3, 2]]

takeEndByS :: Expectation
takeEndByS =
    check (F.takeEndBy (== 1) F.toList) ([3, 2, 1, 4] :: [Int])
        [[], [3], [3, 2], [3, 2, 1]]

-------------------------------------------------------------------------------
-- Tuple result
-------------------------------------------------------------------------------

teeWithS :: [Int] -> Expectation
teeWithS ls =
    check
        (F.teeWith (,) F.sum F.length)
        ls
        (Prelude.zip (Prelude.scanl (+) 0 ls) [0 .. Prelude.length ls])

-------------------------------------------------------------------------------
-- Transforming inner monad
-------------------------------------------------------------------------------

morphInnerS :: [Int] -> Expectation
morphInnerS ls =
    check
        (F.morphInner (return . runIdentity) (F.sum :: Op Identity Int Int))
        ls
        (Prelude.scanl (+) 0 ls)

generalizeInnerS :: [Int] -> Expectation
generalizeInnerS ls =
    check
        (F.generalizeInner (F.sum :: Op Identity Int Int))
        ls
        (Prelude.scanl (+) 0 ls)

-------------------------------------------------------------------------------
-- Spec registered identically by both suites
-------------------------------------------------------------------------------

commonTypeSpec :: Spec
commonTypeSpec = do
    prop "length" lengthS
    prop "genericLength" genericLengthS
    prop "toList" toListS
    prop "latest" latestS
    prop "drain" drainS
    prop "maximum" maximumS
    prop "maximumBy" maximumByS
    prop "minimum" minimumS
    prop "minimumBy" minimumByS
    prop "range" rangeS
    prop "rangeBy" rangeByS
    prop "lmap" lmapS
    prop "lmapM" lmapMS
    prop "rmapM" rmapMS
    prop "filter" filterS
    prop "filterM" filterMS
    prop "filtering" filteringS
    it "catMaybes" catMaybesS
    it "catLefts" catLeftsS
    it "catRights" catRightsS
    it "catEithers" catEithersS
    prop "take" takeS
    it "taking" takingS
    it "dropping" droppingS
    it "takeEndBy_" takeEndBy_S
    it "takeEndBy" takeEndByS
    prop "teeWith" teeWithS
    prop "morphInner" morphInnerS
    prop "generalizeInner" generalizeInnerS
