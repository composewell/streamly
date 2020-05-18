module Main (main) where

import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, chooseInt, Property, property, listOf, vectorOf, (.&&.))

-- Accumulator Tests

fromFold :: Property
fromFold =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case (==) <$> (S.parseD (D.fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

any :: Property
any =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parseD (D.any (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> 5000) ls)
            Left _ -> False

all :: Property
all =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parseD (D.all (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> 5000) ls)
            Left _ -> False

yield :: Property
yield = 
    forAll (chooseInt (0, 10000)) $ \x ->
        case S.parseD (D.yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

yieldM :: Property
yieldM =
    forAll (chooseInt (0, 10000)) $ \x ->
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

peek :: Property
peek = 
    forAll (chooseInt (1, 100)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (0, 10000))) $ \ls ->
            case S.parseD D.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False
    .&&.
    property (case S.parseD D.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eof :: Property
eof = 
    forAll (chooseInt (1, 100)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (0, 10000))) $ \ls ->
            case S.parseD D.eof (S.fromList ls) of
                Right _ -> False
                Left _ -> True
    .&&.
    property (case S.parseD D.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

satisfy :: Property
satisfy = 
    forAll (listOf (chooseInt (0, 10000))) $ \ls ->
        case S.parseD (D.satisfy predicate) (S.fromList ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= 5000)

-- Sequence Parsers Tests

take :: Property
take = 
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            case S.parseD (D.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> parsed_list == Prelude.take n ls
                Left _ -> False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parseD (D.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == Prelude.take n ls)
                    Left _ -> n > list_length

takeGE :: Property
takeGE =
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parseD (D.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == ls)
                    Left _ -> n > list_length

lookAhead :: Property
lookAhead =
    forAll (chooseInt (0, 10000)) $ \n -> 
        let
            takeWithoutConsume = D.lookAhead $ D.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (0, 10000))) $ \ls ->
                case S.parseD parseTwice (S.fromList ls) of
                    Right (ls_1, ls_2) -> (ls_1 == ls_2) && (ls_1 == Prelude.take n ls)
                    Left _ -> (list_length < n) || (list_length == n && n == 0)
                        where
                            list_length = Prelude.length ls

takeWhile :: Property
takeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parseD (D.takeWhile predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> parsed_list == Prelude.takeWhile predicate ls
            Left _ -> False
        where
            predicate = (== 0)

takeWhile1 :: Property
takeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parseD (D.takeWhile1 predicate  FL.toList) (S.fromList ls) of
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
        case S.parseD (D.sliceSepBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> parsed_list == Prelude.takeWhile (not . predicate) ls
            Left _ -> False
        where
            predicate = (== 1)

-- sliceSepByMax :: Property
-- sliceSepByMax = 
--     forAll (chooseInt (0, 10000)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             case S.parseD (D.sliceSepByMax predicate n FL.toList) (S.fromList ls) of
--                 Right parsed_list -> parsed_list == Prelude.take n (Prelude.takeWhile (not . predicate) ls)
--                 Left _ -> False
--             where
--                 predicate = (== 1)

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
    .&&.
    property (case S.parseD (D.splitWith (,) (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)
    .&&.
    property (case S.parseD (D.splitWith (,) (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)
    .&&.
    property (case S.parseD (D.splitWith (,) (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWith :: Property
teeWith = 
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            let
                prsr = D.take n FL.toList
            in
                case S.parseD (D.teeWith (,) prsr prsr) (S.fromList ls) of
                    Right (ls_1, ls_2) -> (Prelude.take n ls == ls_1) && (ls_1 == ls_2)
                    Left _ -> False
    .&&.
    property (case S.parseD (D.teeWith (,) (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)
    .&&.
    property (case S.parseD (D.teeWith (,) (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)
    .&&.
    property (case S.parseD (D.teeWith (,) (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

shortest :: Property
shortest =
    forAll (listOf (chooseInt(0, 10000))) $ \ls ->
        let
            prsr_1 = D.takeWhile (<= 2500) FL.toList
            prsr_2 = D.takeWhile (<= 5000) FL.toList
            prsr_shortest = D.shortest prsr_1 prsr_2
        in
            case S.parseD prsr_shortest (S.fromList ls) of
                Right short_list -> short_list == Prelude.takeWhile (<= 2500) ls
                Left _ -> False
    .&&.
    property (case S.parseD (D.shortest (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)
    .&&.
    property (case S.parseD (D.shortest (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)
    .&&.
    property (case S.parseD (D.shortest (D.die "die") (D.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

longest :: Property
longest =
    forAll (listOf (chooseInt(0, 10000))) $ \ls ->
        let
            prsr_1 = D.takeWhile (<= 2500) FL.toList
            prsr_2 = D.takeWhile (<= 5000) FL.toList
            prsr_longest = D.longest prsr_1 prsr_2
        in
            case S.parseD prsr_longest (S.fromList ls) of
                Right long_list -> long_list == Prelude.takeWhile (<= 5000) ls
                Left _ -> False
    .&&.
    property (case S.parseD (D.shortest (D.die "die") (D.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)
    .&&.
    property (case S.parseD (D.shortest (D.yield (1 :: Int)) (D.die "die")) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)
    .&&.
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
                Right res_list -> res_list == Prelude.filter (== 0) ls
                Left _ -> False
    .&&.
    property (case S.parseD (D.many FL.toList (D.die "die")) (S.fromList [1 :: Int]) of
        Right res_list -> res_list == ([] :: [Int])
        Left _ -> False)

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
--     .&&.
--     property (case S.parseD (D.some FL.toList (D.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

main :: IO ()
main = hspec $ do
    describe "test for accumulator" $ do
        prop "test fromFold function" fromFold
        prop "test any function" Main.any
        prop "test all function" Main.all
        prop "test yield function" yield
        prop "test yieldM function" yieldM
        prop "test die function" die
        prop "test dieM function" dieM
    
    describe "test for element parser" $ do
        prop "test for peek function" peek
        prop "test for eof function" eof
        prop "test for satisfy function" satisfy

    describe "test for sequence parser" $ do
        prop "test for take function" Main.take
        prop "test for takeEq function" Main.takeEQ
        prop "test for takeGE function" Main.takeGE
        prop "test for LookAhead function" lookAhead
        prop "test for takeWhile function" Main.takeWhile
        prop "test for takeWhile1 function" takeWhile1
        prop "test for sliceSepBy function" sliceSepBy
        -- prop "test for sliceSepByMax function" sliceSepByMax
        prop "test for splitWith function" splitWith
        prop "test for teeWith function" teeWith
        prop "test for shortest function" shortest
        prop "test for longest function" longest
        prop "test for many function" many
        -- prop "test for some function" some