--
--  See test/Streamly/Test/Data/Scanl/Type.hs for Scanl/Fold test sharing
--  mechanism.

-------------------------------------------------------------------------------
-- Accumulators
-------------------------------------------------------------------------------

-- 'product' short-circuits at a 0 input (0 is absorbing), terminating the scan
-- at, and including, that zero. Fixed examples are used to avoid Int-overflow
-- artifacts that a property over arbitrary Ints would hit.
productS :: Expectation
productS = do
    check F.product ([2, 3, 4] :: [Int]) [1, 2, 6, 24]
    check F.product ([2, 0, 3] :: [Int]) [1, 2, 0]
    check F.product ([0] :: [Int]) [1, 0]
    check F.product ([] :: [Int]) [1]
    -- Short-circuit: the element after the first 0 is bottom, so this throws
    -- (and fails) if 'product' consumes past the 0.
    checkNoLaw
        F.product ([2, 0, error "product consumed past 0"] :: [Int]) [1, 2, 0]

sumS :: [Int] -> Expectation
sumS ls = check F.sum ls (Prelude.scanl (+) 0 ls)

toListRevS :: [Int] -> Expectation
toListRevS ls = check F.toListRev ls (Prelude.scanl (flip (:)) [] ls)

mconcatS :: [Int] -> Expectation
mconcatS ls =
    check F.mconcat (Prelude.map Sum ls)
        (Prelude.scanl (<>) (Sum 0) (Prelude.map Sum ls))

foldMapS :: [Int] -> Expectation
foldMapS ls =
    check (F.foldMap Sum) ls (Prelude.scanl (\acc x -> acc <> Sum x) (Sum 0) ls)

foldMapMS :: [Int] -> Expectation
foldMapMS ls =
    check (F.foldMapM (return . Sum)) ls
        (Prelude.scanl (\acc x -> acc <> Sum x) (Sum 0) ls)

theS :: Expectation
theS = do
    check F.the ([3, 3, 3] :: [Int]) [Nothing, Just 3, Just 3, Just 3]
    check F.the ([3, 3, 4] :: [Int]) [Nothing, Just 3, Just 3, Nothing]
    check F.the ([] :: [Int]) [Nothing]
    -- Short-circuit: the element after the first mismatch is bottom, so this
    -- throws (and fails) if 'the' consumes past the mismatch.
    checkNoLaw F.the ([3, 3, 4, error "the consumed past mismatch"] :: [Int])
        [Nothing, Just 3, Just 3, Nothing]

-- Polynomial rolling hash:
--
--   H = salt * k ^ n + c1 * k ^ (n - 1) + c2 * k ^ (n - 2) + ... + cn * k ^ 0
--
-- where c1..cn are the inputs and k is a fixed multiplier. This is computed
-- incrementally by the step @cksum * k + fromEnum a@ starting from @salt@, so
-- the inclusive prescan over the input is the reference @expected@.
--
-- 'rollingHashK' and 'rollingHashDefaultSalt' deliberately duplicate the source
-- constants (rather than importing them) so this test fails if either changes:
-- the hash values are an output contract, and a change to 'k' or the default
-- salt alters every computed hash.
rollingHashK :: Int64
rollingHashK = 2891336453

rollingHashDefaultSalt :: Int64
rollingHashDefaultSalt = -2578643520546668380

rollingHashRef :: Int64 -> [Int] -> [Int64]
rollingHashRef salt =
    Prelude.scanl
        (\cksum a -> cksum * rollingHashK + fromIntegral (fromEnum a)) salt

rollingHashWithSaltS :: [Int] -> Expectation
rollingHashWithSaltS ls =
    check (F.rollingHashWithSalt salt) ls (rollingHashRef salt ls)

    where

    salt = 0

rollingHashS :: [Int] -> Expectation
rollingHashS ls = check F.rollingHash ls (rollingHashRef rollingHashDefaultSalt ls)

-- 'rollingHashFirstN n' is 'rollingHash' truncated to the first n inputs, so its
-- reference is the rollingHash prescan capped at n+1 outputs (the initial value
-- plus one per consumed input). Checked for every n from 0 up to past the end of
-- the stream.
rollingHashFirstNS :: [Int] -> Expectation
rollingHashFirstNS ls =
    Prelude.mapM_
        (\n ->
            check
                (F.rollingHashFirstN n)
                ls
                (Prelude.take (n + 1) (rollingHashRef rollingHashDefaultSalt ls)))
        [0 .. Prelude.length ls + 2]

-------------------------------------------------------------------------------
-- Statistical
-------------------------------------------------------------------------------

chooseFloat :: (Float, Float) -> Gen Float
chooseFloat = choose

-- 'mean' produces a numerically stable floating-point average that is only
-- approximately equal to the naive sum/count reference, so it uses
-- 'checkApprox' (epsilon comparison) rather than the exact-equality 'check'.
-- The mean of the empty prefix is 0 (the scan's initial value).
meanS :: Property
meanS =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls -> withNumTests 1000 $ checkApprox F.mean ls (expected ls)

    where

    expected ls =
        [ if k == 0
          then 0
          else Prelude.sum (Prelude.take k ls) / fromIntegral k
        | k <- [0 .. Prelude.length ls]
        ]

-------------------------------------------------------------------------------
-- Filtering / mapping
-------------------------------------------------------------------------------

-- A filtered-out (Nothing) element emits no output.
mapMaybeS :: [Int] -> Expectation
mapMaybeS ls =
    check
        (F.mapMaybe (\x -> if even x then Just x else Nothing) F.toList)
        ls
        (Prelude.scanl (\acc x -> acc ++ [x]) [] (Prelude.filter even ls))

drainMapMS :: [Int] -> Expectation
drainMapMS ls = check (F.drainMapM return) ls (Prelude.scanl (\_ _ -> ()) () ls)

-------------------------------------------------------------------------------
-- Distributing / unzipping (fixed examples)
-------------------------------------------------------------------------------

distributeS :: [Int] -> Expectation
distributeS ls =
    check (F.distribute [F.sum, F.length]) ls
        [[Prelude.sum (Prelude.take k ls), k] | k <- [0 .. Prelude.length ls]]

unzipWithS :: Expectation
unzipWithS =
    check (F.unzipWith (\x -> (x, x * 2)) F.sum F.sum) ([1, 2, 3] :: [Int])
        [(0, 0), (1, 2), (3, 6), (6, 12)]

unzipWithMS :: Expectation
unzipWithMS =
    check (F.unzipWithM (\x -> return (x, x * 2)) F.sum F.sum) ([1, 2, 3] :: [Int])
        [(0, 0), (1, 2), (3, 6), (6, 12)]

indexedS :: Expectation
indexedS =
    check (F.indexed F.toList) "abc"
        [ []
        , [(0, 'a')]
        , [(0, 'a'), (1, 'b')]
        , [(0, 'a'), (1, 'b'), (2, 'c')]
        ]

sampleFromthenS :: Expectation
sampleFromthenS =
    check (F.sampleFromthen 0 2 F.toList) ([1 .. 6] :: [Int])
        [[], [1], [1, 3], [1, 3, 5]]

sconcatS :: Expectation
sconcatS =
    check (F.sconcat (Sum 10)) (Prelude.map Sum ([1, 2, 3] :: [Int]))
        [Sum 10, Sum 11, Sum 13, Sum 16]

unzipS :: Expectation
unzipS =
    check (F.unzip F.sum F.toList) ([(1, 'a'), (2, 'b'), (3, 'c')] :: [(Int, Char)])
        [(0, ""), (1, "a"), (3, "ab"), (6, "abc")]

drainNS :: Expectation
drainNS = check (F.drainN 3) ([1, 2, 3, 4, 5] :: [Int]) [(), (), (), ()]

-------------------------------------------------------------------------------
-- Scanners (emit Maybe; the per-input view, verified in full)
-------------------------------------------------------------------------------

deleteByS :: Expectation
deleteByS =
    check (F.deleteBy (==) 3) ([1, 2, 3, 4, 3, 5] :: [Int])
        [Nothing, Just 1, Just 2, Nothing, Just 4, Just 3, Just 5]

findIndicesS :: Expectation
findIndicesS =
    check (F.findIndices even) ([1, 2, 3, 4, 5, 6] :: [Int])
        [Nothing, Nothing, Just 1, Nothing, Just 3, Nothing, Just 5]

elemIndicesS :: Expectation
elemIndicesS =
    check (F.elemIndices 3) ([1, 3, 2, 3, 4, 3] :: [Int])
        [Nothing, Nothing, Just 1, Nothing, Just 3, Nothing, Just 5]

droppingWhileS :: Expectation
droppingWhileS =
    check (F.droppingWhile (< 3)) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Nothing, Nothing, Just 3, Just 4, Just 5]

droppingWhileMS :: Expectation
droppingWhileMS =
    check (F.droppingWhileM (return . (< 3))) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Nothing, Nothing, Just 3, Just 4, Just 5]

takingEndByS :: Expectation
takingEndByS =
    check (F.takingEndBy (== 3)) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Just 1, Just 2, Just 3]

takingEndByMS :: Expectation
takingEndByMS =
    check (F.takingEndByM (return . (== 3))) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Just 1, Just 2, Just 3]

takingEndByM_S :: Expectation
takingEndByM_S =
    check (F.takingEndByM_ (return . (== 3))) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Just 1, Just 2, Nothing]

-------------------------------------------------------------------------------
-- Non-Eq results: convert the emitted value to a list via rmapM so the
-- per-step output can be compared. toStream/toStreamRev emit a Stream;
-- top/bottom emit a MutArray.
-------------------------------------------------------------------------------

toStreamS :: [Int] -> Expectation
toStreamS ls =
    check (F.rmapM Stream.toList F.toStream) ls
        (Prelude.scanl (\acc x -> acc ++ [x]) [] ls)

toStreamRevS :: [Int] -> Expectation
toStreamRevS ls =
    check (F.rmapM Stream.toList F.toStreamRev) ls (Prelude.scanl (flip (:)) [] ls)

topS :: Expectation
topS = do
    check (F.rmapM MArray.toList (F.top 3)) ([5, 1, 4, 2, 3] :: [Int])
        [[], [5], [5, 1], [5, 4, 1], [5, 4, 2], [5, 4, 3]]
    -- top 0 is Done at the initial step (keeps nothing)
    check (F.rmapM MArray.toList (F.top 0)) ([5, 1, 4] :: [Int]) [[]]

bottomS :: Expectation
bottomS =
    check (F.rmapM MArray.toList (F.bottom 3)) ([5, 1, 4, 2, 3] :: [Int])
        [[], [5], [1, 5], [1, 4, 5], [1, 2, 4], [1, 2, 3]]

-------------------------------------------------------------------------------
-- Postscan-only scanners (scanl initial is undefined: error "Empty stream").
-- Use checkPostscanl; the @expected@ is the postscanl output.
-------------------------------------------------------------------------------

rollingMapS :: Expectation
rollingMapS =
    checkPostscanl (F.rollingMap (\prev cur -> Prelude.maybe 0 (cur -) prev))
        ([1, 3, 6] :: [Int]) [0, 2, 3]

rollingMapMS :: Expectation
rollingMapMS =
    checkPostscanl
        (F.rollingMapM (\prev cur -> return (Prelude.maybe 0 (cur -) prev)))
        ([1, 3, 6] :: [Int]) [0, 2, 3]

uniqS :: Expectation
uniqS =
    checkPostscanl F.uniq ([1, 1, 2, 3, 3, 3, 4] :: [Int])
        [Just 1, Nothing, Just 2, Just 3, Nothing, Nothing, Just 4]

uniqByS :: Expectation
uniqByS =
    checkPostscanl (F.uniqBy (==)) ([1, 1, 2, 3, 3, 3, 4] :: [Int])
        [Just 1, Nothing, Just 2, Just 3, Nothing, Nothing, Just 4]

-------------------------------------------------------------------------------
-- Common test spec for Scanl and Fold
-------------------------------------------------------------------------------

commonCombinatorsSpec :: Spec
commonCombinatorsSpec = do
    prop "sum" sumS
    prop "toListRev" toListRevS
    it "product" productS
    prop "mconcat" mconcatS
    prop "foldMap" foldMapS
    prop "foldMapM" foldMapMS
    it "the" theS
    prop "mean" meanS
    prop "rollingHashWithSalt" rollingHashWithSaltS
    prop "rollingHash" rollingHashS
    prop "rollingHashFirstN" rollingHashFirstNS
    prop "mapMaybe" mapMaybeS
    prop "drainMapM" drainMapMS
    prop "distribute" distributeS
    it "unzipWith" unzipWithS
    it "unzipWithM" unzipWithMS
    it "indexed" indexedS
    it "sampleFromthen" sampleFromthenS
    it "sconcat" sconcatS
    it "unzip" unzipS
    it "drainN" drainNS
    it "deleteBy" deleteByS
    it "findIndices" findIndicesS
    it "elemIndices" elemIndicesS
    it "droppingWhile" droppingWhileS
    it "droppingWhileM" droppingWhileMS
    it "takingEndBy" takingEndByS
    it "takingEndByM" takingEndByMS
    it "takingEndByM_" takingEndByM_S
    it "rollingMap" rollingMapS
    it "rollingMapM" rollingMapMS
    it "uniq" uniqS
    it "uniqBy" uniqByS
    prop "toStream" toStreamS
    prop "toStreamRev" toStreamRevS
    it "top" topS
    it "bottom" bottomS
