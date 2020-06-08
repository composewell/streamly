module Main (main) where

import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

-- import Data.List (partition)

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, choose, Property, property, listOf, vectorOf, counterexample, (.&&.), Gen)

import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.List ((\\))

min_value :: Int
min_value = 0

mid_value :: Int
mid_value = 5000

max_value :: Int
max_value = 10000

max_length :: Int
max_length = 1000

listEquals :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq parsed_list list = do
    when (not $ parsed_list `eq` list) $ liftIO $ putStrLn $
                  "parsed list " ++ show parsed_list
             ++ "\nlist   " ++ show list
             ++ "\nparsed list \\\\ list " ++ show (parsed_list \\ list)
             ++ "\nlist \\\\ parsed list " ++ show (list \\ parsed_list)
    when (not $ parsed_list `eq` list) $
        monitor
            (counterexample $
                  "parsed list " ++ show parsed_list
             ++ "\nlist   " ++ show list
             ++ "\nparsed list \\\\ list " ++ show (parsed_list \\ list)
             ++ "\nlist \\\\ parsed list " ++ show (list \\ parsed_list)
             )
    assert (parsed_list `eq` list)

checkListEqual :: (Show a, Eq a) => [a] -> [a] -> Property
checkListEqual ls_1 ls_2 = monadicIO (listEquals (==) ls_1 ls_2)

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

-- Accumulator Tests

fromFold :: Property
fromFold =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case (==) <$> (S.parseD (D.fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

any :: Property
any =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parseD (D.any (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> mid_value) ls)
            Left _ -> False

all :: Property
all =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parseD (D.all (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> mid_value) ls)
            Left _ -> False

yield :: Property
yield = 
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parseD (D.yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

yieldM :: Property
yieldM =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parseD (D.yieldM $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

die :: Property
die =
    property $
    case S.parseD (D.die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

dieM :: Property
dieM =
    property $
    case S.parseD (D.dieM (Right "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

-- Element Parser Tests

peekPass :: Property
peekPass = 
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parseD D.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False

peekFail :: Property
peekFail =
    property (case S.parseD D.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eofPass :: Property
eofPass = 
    property (case S.parseD D.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

eofFail :: Property
eofFail = 
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parseD D.eof (S.fromList ls) of
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
                case S.parseD (D.satisfy predicate) (S.fromList ls) of
                    Right r -> r == first_element
                    Left _ -> False

satisfy :: Property
satisfy = 
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        case S.parseD (D.satisfy predicate) (S.fromList ls) of
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
            case S.parseD (D.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                Left _ -> property False

takeEQPass :: Property
takeEQPass = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                case S.parseD (D.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                    Left _ -> property False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parseD (D.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> 
                        if (n <= list_length) then
                            checkListEqual parsed_list (Prelude.take n ls)
                        else
                            property False
                    Left _ -> property (n > list_length)

takeGEPass :: Property
takeGEPass = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                case S.parseD (D.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> checkListEqual parsed_list ls
                    Left _ -> property False

takeGE :: Property
takeGE =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parseD (D.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> 
                        if (n <= list_length) then
                            checkListEqual parsed_list ls
                        else
                            property False
                    Left _ -> property (n > list_length)

lookAheadPass :: Property
lookAheadPass =
    forAll (chooseInt (min_value + 1, max_value)) $ \n -> 
        let
            takeWithoutConsume = D.lookAhead $ D.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (chooseInt (n, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                    case S.parseD parseTwice (S.fromList ls) of
                        Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                        Left _ -> property $ False

lookAheadFail :: Property
lookAheadFail =
    forAll (chooseInt (min_value + 1, max_value)) $ \n -> 
        let
            takeWithoutConsume = D.lookAhead $ D.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (chooseInt (min_value, n - 1)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                    case S.parseD parseTwice (S.fromList ls) of
                        Right _ -> False
                        Left _ -> True

lookAhead :: Property
lookAhead =
    forAll (chooseInt (min_value, max_value)) $ \n -> 
        let
            takeWithoutConsume = D.lookAhead $ D.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                case S.parseD parseTwice (S.fromList ls) of
                    Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                    Left _ -> property ((list_length < n) || (list_length == n && n == 0))
                        where
                            list_length = Prelude.length ls

takeWhile :: Property
takeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parseD (D.takeWhile predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> checkListEqual parsed_list (Prelude.takeWhile predicate ls)
            Left _ -> property False
        where
            predicate = (== 0)

takeWhile1 :: Property
takeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parseD (D.takeWhile1 predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> case ls of
                [] -> property False
                (x : _) -> 
                    if predicate x then
                        checkListEqual parsed_list (Prelude.takeWhile predicate ls)
                    else
                        property False
            Left _ -> case ls of
                [] -> property True
                (x : _) -> property (not $ predicate x)
        where
            predicate = (== 0)
    
sliceSepBy :: Property
sliceSepBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (D.sliceSepBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> checkListEqual parsed_list (Prelude.takeWhile (not . predicate) ls)
            Left _ -> property False
        where
            predicate = (== 1)

sliceSepByMax :: Property
sliceSepByMax = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            case S.parseD (D.sliceSepByMax predicate n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n (Prelude.takeWhile (not . predicate) ls))
                Left _ -> property False
            where
                predicate = (== 1)

splitWith :: Property
splitWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (D.splitWith (,) (D.satisfy (== 0)) (D.satisfy (== 1))) (S.fromList ls) of
            Right (result_first, result_second) -> case ls of
                0 : 1 : _ -> (result_first == 0) && (result_second == 1)
                _ -> False
            Left _ -> case ls of
                0 : 1 : _ -> False
                _ -> True

splitWithFailLeft :: Property
splitWithFailLeft =
    property (case S.parseD (D.splitWith (,) (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailRight :: Property
splitWithFailRight =
    property (case S.parseD (D.splitWith (,) (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailBoth :: Property
splitWithFailBoth =
    property (case S.parseD (D.splitWith (,) (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWithPass :: Property
teeWithPass = 
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            let
                prsr = D.take n FL.toList
            in
                case S.parseD (D.teeWith (,) prsr prsr) (S.fromList ls) of
                    Right (ls_1, ls_2) -> checkListEqual (Prelude.take n ls) ls_1 .&&. checkListEqual ls_1 ls_2
                    Left _ -> property False

teeWithFailLeft :: Property
teeWithFailLeft = 
    property (case S.parseD (D.teeWith (,) (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWithFailRight :: Property
teeWithFailRight = 
    property (case S.parseD (D.teeWith (,) (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWithFailBoth :: Property
teeWithFailBoth = 
    property (case S.parseD (D.teeWith (,) (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

shortestPass :: Property
shortestPass =
    forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
        let
            half_mid_value = mid_value `Prelude.div` 2
            prsr_1 = D.takeWhile (<= half_mid_value) FL.toList
            prsr_2 = D.takeWhile (<= mid_value) FL.toList
            prsr_shortest = D.shortest prsr_1 prsr_2
        in
            case S.parseD prsr_shortest (S.fromList ls) of
                Right short_list -> checkListEqual short_list (Prelude.takeWhile (<= half_mid_value) ls)
                Left _ -> property False

shortestPassLeft :: Property
shortestPassLeft =
    property (case S.parseD (D.shortest (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

shortestPassRight :: Property
shortestPassRight =
    property (case S.parseD (D.shortest (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

shortestFailBoth :: Property
shortestFailBoth =
    property (case S.parseD (D.shortest (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

longestPass :: Property
longestPass =
    forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
        let
            half_mid_value = mid_value `Prelude.div` 2
            prsr_1 = D.takeWhile (<= half_mid_value) FL.toList
            prsr_2 = D.takeWhile (<= mid_value) FL.toList
            prsr_longest = D.longest prsr_1 prsr_2
        in
            case S.parseD prsr_longest (S.fromList ls) of
                Right long_list -> long_list == Prelude.takeWhile (<= mid_value) ls
                Left _ -> False

longestPassLeft :: Property
longestPassLeft =
    property (case S.parseD (D.shortest (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

longestPassRight :: Property
longestPassRight =
    property (case S.parseD (D.shortest (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

longestFailBoth :: Property
longestFailBoth =
    property (case S.parseD (D.shortest (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

many :: Property
many = 
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let
            concatFold = FL.Fold (\concatList curr_list -> return $ concatList ++ curr_list) (return []) return
            prsr = D.many concatFold $ D.sliceSepBy (== 1) FL.toList
        in
            case S.parseD prsr (S.fromList ls) of
                Right res_list -> checkListEqual res_list (Prelude.filter (== 0) ls)
                Left _ -> property False

many_empty :: Property
many_empty = 
    property (case S.parseD (D.many FL.toList (D.die "die")) (S.fromList [1 :: Int]) of
        Right res_list -> checkListEqual res_list ([] :: [Int])
        Left _ -> property False)

-- some :: Property
-- some = 
--     forAll (listOf (chooseInt (0, 1))) $ \ls ->
--         let
--             concatFold = FL.Fold (\concatList curr_list -> return $ concatList ++ curr_list) (return []) return
--             prsr = D.some concatFold $ D.sliceSepBy (== 1) FL.toList
--         in
--             case S.parseD prsr (S.fromList ls) of
--                 Right res_list -> res_list == Prelude.filter (== 0) ls
--                 Left _ -> False

-- someFail :: Property
-- someFail = 
--     property (case S.parseD (D.some FL.toList (D.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

main :: IO ()
main = hspec $ do
    describe "test for accumulator" $ do
        prop "D.fromFold FL.sum = FL.sum" fromFold
        prop "D.any = Prelude.any" Main.any
        prop "D.all = Prelude.all" Main.all
        prop "yield value provided" yield
        prop "yield monadic value provided" yieldM
        prop "always fail" die
        prop "always fail but monadic" dieM
    
    describe "test for element parser" $ do
        prop "peek = head with list length > 0" peekPass
        prop "peek fail on []" peekFail
        prop "eof pass on []" eofPass
        prop "eof fail on non-empty list" eofFail
        prop "first element exists and >= mid_value" satisfyPass
        prop "check first element exists and satisfies predicate" satisfy

    describe "test for sequence parser" $ do
        prop "D.take = Prelude.take" Main.take
        prop "D.takeEQ = Prelude.take when len >= n" takeEQPass
        prop "D.takeEQ = Prelude.take when len >= n and fail otherwise" Main.takeEQ
        prop "D.takeGE n ls = ls when len >= n" takeGEPass
        prop "D.takeGE n ls = ls when len >= n and fail otherwise" Main.takeGE
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" lookAheadPass
        prop "Fail when stream length exceeded" lookAheadFail
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" lookAhead
        prop "D.takeWhile = Prelude.takeWhile" Main.takeWhile
        prop "D.takeWhile = Prelude.takeWhile if taken something, else check why failed" takeWhile1
        prop "D.sliceSepBy = Prelude.takeWhile (not . predicate)" sliceSepBy
        prop "D.sliceSepByMax = Prelude.take n (Prelude.takeWhile (not . predicate)" sliceSepByMax
        prop "parse 0, then 1, else fail" splitWith
        prop "fail due to die as left parser" splitWithFailLeft
        prop "fail due to die as right parser" splitWithFailRight
        prop "fail due to die as both parsers" splitWithFailBoth
        prop "parsed two lists should be equal" teeWithPass
        prop "fail due to die as left parser" teeWithFailLeft
        prop "fail due to die as right parser" teeWithFailRight
        prop "fail due to die as both parsers" teeWithFailBoth
        prop "D.takeWhile (<= half_mid_value) = Prelude.takeWhile half_mid_value" shortestPass
        prop "pass even if die is left parser" shortestPassLeft
        prop "pass even if die is right parser" shortestPassRight
        prop "fail due to die as both parsers" shortestFailBoth
        prop "D.takeWhile (<= mid_value) = Prelude.takeWhile (<= mid_value)" longestPass
        prop "pass even if die is left parser" longestPassLeft
        prop "pass even if die is right parser" longestPassRight
        prop "fail due to die as both parsers" longestFailBoth
        prop "D.many concatFold $ D.sliceSepBy (== 1) FL.toList = Prelude.filter (== 0)" many
        prop "[] due to parser being die" many_empty
        -- prop "" some
        -- prop "fail due to parser being die" someFail