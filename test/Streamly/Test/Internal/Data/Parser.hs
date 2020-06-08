module Main (main) where

import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

-- import Data.List (partition)

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, chooseInt, Property, property, listOf, vectorOf)

min_value :: Int
min_value = 0

mid_value :: Int
mid_value = 5000

max_value :: Int
max_value = 10000

max_length :: Int
max_length = 1000

-- Accumulator Tests

fromFold :: Property
fromFold =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case (==) <$> (S.parse (P.fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

any :: Property
any =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parse (P.any (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> mid_value) ls)
            Left _ -> False

all :: Property
all =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parse (P.all (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> mid_value) ls)
            Left _ -> False

yield :: Property
yield = 
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parse (P.yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

yieldM :: Property
yieldM =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parse (P.yieldM $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

die :: Property
die =
    property $
    case S.parse (P.die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

dieM :: Property
dieM =
    property $
    case S.parse (P.dieM (Right "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

-- Element Parser Tests

peekPass :: Property
peekPass = 
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parse P.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False

peekFail :: Property
peekFail =
    property (case S.parse P.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eofPass :: Property
eofPass = 
    property (case S.parse P.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

eofFail :: Property
eofFail = 
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parse P.eof (S.fromList ls) of
                Right _ -> False
                Left _ -> True

satisfyPass :: Property
satisfyPass = 
    forAll (chooseInt (mid_value, max_value)) $ \first_element ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls_tail ->
            let
                ls = first_element : ls_tail
                predicate = (>= mid_value)
            in
                case S.parse (P.satisfy predicate) (S.fromList ls) of
                    Right r -> r == first_element
                    Left _ -> False

satisfy :: Property
satisfy = 
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        case S.parse (P.satisfy predicate) (S.fromList ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= mid_value)

-- Sequence Parsers Tests

take :: Property
take = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            case S.parse (P.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> parsed_list == Prelude.take n ls
                Left _ -> False

takeEQPass :: Property
takeEQPass = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                case S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> parsed_list == Prelude.take n ls
                    Left _ -> False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == Prelude.take n ls)
                    Left _ -> n > list_length

takeGEPass :: Property
takeGEPass = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                case S.parse (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> parsed_list == ls
                    Left _ -> False

takeGE :: Property
takeGE =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parse (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == ls)
                    Left _ -> n > list_length

lookAheadPass :: Property
lookAheadPass =
    forAll (chooseInt (min_value + 1, max_value)) $ \n -> 
        let
            takeWithoutConsume = P.lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (chooseInt (n, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                    case S.parse parseTwice (S.fromList ls) of
                        Right (ls_1, ls_2) -> (ls_1 == ls_2) && (ls_1 == Prelude.take n ls)
                        Left _ -> False

lookAheadFail :: Property
lookAheadFail =
    forAll (chooseInt (min_value + 1, max_value)) $ \n -> 
        let
            takeWithoutConsume = P.lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (chooseInt (min_value, n - 1)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                    case S.parse parseTwice (S.fromList ls) of
                        Right _ -> False
                        Left _ -> True

lookAhead :: Property
lookAhead =
    forAll (chooseInt (min_value, max_value)) $ \n -> 
        let
            takeWithoutConsume = P.lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                case S.parse parseTwice (S.fromList ls) of
                    Right (ls_1, ls_2) -> (ls_1 == ls_2) && (ls_1 == Prelude.take n ls)
                    Left _ -> (list_length < n) || (list_length == n && n == 0)
                        where
                            list_length = Prelude.length ls

takeWhile :: Property
takeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parse (P.takeWhile predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> parsed_list == Prelude.takeWhile predicate ls
            Left _ -> False
        where
            predicate = (== 0)

takeWhile1 :: Property
takeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parse (P.takeWhile1 predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> case ls of
                [] -> False
                (x : _) -> predicate x && (parsed_list == Prelude.takeWhile predicate ls)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (== 0)
    
sliceSepBy :: Property
sliceSepBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.sliceSepBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> parsed_list == Prelude.takeWhile (not . predicate) ls
            Left _ -> False
        where
            predicate = (== 1)

-- sliceSepByMax :: Property
-- sliceSepByMax = 
--     forAll (chooseInt (min_value, max_value)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             case S.parse (P.sliceSepByMax predicate n FL.toList) (S.fromList ls) of
--                 Right parsed_list -> parsed_list == Prelude.take n (Prelude.takeWhile (not . predicate) ls)
--                 Left _ -> False
--             where
--                 predicate = (== 1)

-- splitWithPass :: Property
-- splitWithPass =
--     forAll (listOf (chooseInt (0, 1))) $ \ls ->
--         case S.parse (P.splitWith (,) (P.satisfy (== 0)) (P.satisfy (== 1))) (S.fromList ls) of
--             Right (result_first, result_second) -> case ls of
--                 0 : 1 : _ -> (result_first == 0) && (result_second == 1)
--                 _ -> False
--             Left _ -> case ls of
--                 0 : 1 : _ -> False
--                 _ -> True

-- splitWithFailLeft :: Property
-- splitWithFailLeft =
--     property (case S.parse (P.splitWith (,) (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- splitWithFailRight :: Property
-- splitWithFailRight =
--     property (case S.parse (P.splitWith (,) (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- splitWithFailBoth :: Property
-- splitWithFailBoth =
--     property (case S.parse (P.splitWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithPass :: Property
-- teeWithPass = 
--     forAll (chooseInt (0, 10000)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             let
--                 prsr = P.take n FL.toList
--             in
--                 case S.parse (P.teeWith (,) prsr prsr) (S.fromList ls) of
--                     Right (ls_1, ls_2) -> (Prelude.take n ls == ls_1) && (ls_1 == ls_2)
--                     Left _ -> False

-- teeWithFailLeft :: Property
-- teeWithFailLeft = 
--     property (case S.parse (P.teeWith (,) (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailRight :: Property
-- teeWithFailRight = 
--     property (case S.parse (P.teeWith (,) (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailBoth :: Property
-- teeWithFailBoth = 
--     property (case S.parse (P.teeWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- deintercalate :: Property
-- deintercalate = 
--     forAll (listOf (chooseInt (0, 1))) $ \ls ->
--         case S.parse (P.deintercalate concatFold prsr_1 concatFold prsr_2) (S.fromList ls) of
--             Right parsed_list_tuple -> parsed_list_tuple == partition (== 0) ls
--             Left _ -> False

--         where
--             prsr_1 = (P.takeWhile (== 0) FL.toList)
--             prsr_2 = (P.takeWhile (== 1) FL.toList)
--             concatFold = FL.Fold (\concatList curr_list -> return $ concatList ++ curr_list) (return []) return

-- shortestPass :: Property
-- shortestPass =
--     forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
--         let
--             prsr_1 = P.takeWhile (<= (mid_value `Prelude.div` 2)) FL.toList
--             prsr_2 = P.takeWhile (<= mid_value) FL.toList
--             prsr_shortest = P.shortest prsr_1 prsr_2
--         in
--             case S.parse prsr_shortest (S.fromList ls) of
--                 Right short_list -> short_list == Prelude.takeWhile (<= 2500) ls
--                 Left _ -> False

-- shortestFailLeft :: Property
-- shortestFailLeft =
--     property (case S.parse (P.shortest (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailRight :: Property
-- shortestFailRight =
--     property (case S.parse (P.shortest (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailBoth :: Property
-- shortestFailBoth =
--     property (case S.parse (P.shortest (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

main :: IO ()
main = hspec $ do
    describe "test for accumulator" $ do
        prop "sum after parsing = sum after folding" fromFold
        prop "compare parser any with prelude any on predicate: list > mid_value" Main.any
        prop "compare parser all with prelude all on predicate: list > mid_value" Main.all
        prop "yield value provided" yield
        prop "yield monadic value provided" yieldM
        prop "always fail" die
        prop "always fail but monadic" dieM
    
    describe "test for element parser" $ do
        prop "parsed value = head list when length list >= 1" peekPass
        prop "peek fail on empty list" peekFail
        prop "eof pass on empty list" eofPass
        prop "eof fail on non-empty list" eofFail
        prop "first element exists and >= mid_value" satisfyPass
        prop "check first element exists and satisfies predicate" satisfy

    describe "test for sequence parser" $ do
        prop "compare parser take with prelude take with value n and list" Main.take
        prop "takeEQ on list of length >= n" takeEQPass
        prop "takeEQ on arbitrary sized list and n" Main.takeEQ
        prop "takeGE on list of length >= n" takeGEPass
        prop "takeGE on arbitrary sized list and n" Main.takeGE
        prop "parse stream twice without exceeding length of list, then check eq of lists" lookAheadPass
        prop "parse stream and exceed the length of stream while parsing" lookAheadFail
        prop "parse stream twice, then check eq of lists if not failed, else check why failed" lookAhead
        prop "compare takeWhile of parser and prelude on list and predicate" Main.takeWhile
        prop "compare with prelude.takeWhile if taken something, else check why failed" takeWhile1
        prop "collect zeros until we see one, do not include one" sliceSepBy
        -- prop "test for sliceSepByMax function" sliceSepByMax
        -- prop "pass test for splitWith function" splitWithPass
        -- prop "left fail test for splitWith function" splitWithFailLeft
        -- prop "right fail test for splitWith function" splitWithFailRight
        -- prop "both fail test for splitWith function" splitWithFailBoth
        -- prop "pass test for teeWith function" teeWithPass
        -- prop "left fail test for teeWith function" teeWithFailLeft
        -- prop "right fail test for teeWith function" teeWithFailRight
        -- prop "both fail test for teeWith function" teeWithFailBoth
        -- prop "test for deintercalate function" deintercalate
        -- prop "pass test for shortest function" shortestPass
        -- prop "left fail test for shortest function" shortestFailLeft
        -- prop "right fail test for shortest function" shortestFailRight
        -- prop "both fail test for shortest function" shortestFailBoth