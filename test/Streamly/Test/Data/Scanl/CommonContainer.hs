--  See test/Streamly/Test/Data/Scanl/Type.hs for Scanl/Fold test sharing
--  mechanism.
--
toSetS :: Expectation
toSetS =
    check F.toSet ([1, 2, 3, 2, 1] :: [Int])
        [ Set.empty
        , Set.fromList [1]
        , Set.fromList [1, 2]
        , Set.fromList [1, 2, 3]
        , Set.fromList [1, 2, 3]
        , Set.fromList [1, 2, 3]
        ]

toIntSetS :: Expectation
toIntSetS =
    check F.toIntSet ([1, 2, 3, 2, 1] :: [Int])
        [ IntSet.empty
        , IntSet.fromList [1]
        , IntSet.fromList [1, 2]
        , IntSet.fromList [1, 2, 3]
        , IntSet.fromList [1, 2, 3]
        , IntSet.fromList [1, 2, 3]
        ]

countDistinctS :: Expectation
countDistinctS =
    check F.countDistinct ([1, 2, 3, 2, 1] :: [Int]) [0, 1, 2, 3, 3, 3]

countDistinctIntS :: Expectation
countDistinctIntS =
    check F.countDistinctInt ([1, 2, 3, 2, 1] :: [Int]) [0, 1, 2, 3, 3, 3]

nubS :: Expectation
nubS =
    checkPostscanl F.nub ([1, 1, 2, 3, 3] :: [Int])
        [Just 1, Nothing, Just 2, Just 3, Nothing]

nubIntS :: Expectation
nubIntS =
    checkPostscanl F.nubInt ([1, 1, 2, 3, 3] :: [Int])
        [Just 1, Nothing, Just 2, Just 3, Nothing]

commonContainerSpec :: Spec
commonContainerSpec = do
    it "toSet" toSetS
    it "toIntSet" toIntSetS
    it "countDistinct" countDistinctS
    it "countDistinctInt" countDistinctIntS
    it "nub" nubS
    it "nubInt" nubIntS
