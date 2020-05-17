module Main (main) where

import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, chooseInt, Property, property, listOf, vectorOf, (.&&.))

-- Accumulator Tests

fromFold :: Property
fromFold =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case (==) <$> (S.parse (P.fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

any :: Property
any =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parse (P.any (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> 5000) ls)
            Left _ -> False

all :: Property
all =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parse (P.all (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> 5000) ls)
            Left _ -> False

yield :: Property
yield = 
    forAll (chooseInt (0, 10000)) $ \x ->
        case S.parse (P.yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

yieldM :: Property
yieldM =
    forAll (chooseInt (0, 10000)) $ \x ->
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

peek :: Property
peek = 
    forAll (chooseInt (1, 100)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (0, 10000))) $ \ls ->
            case S.parse P.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False
    .&&.
    property (case S.parse P.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eof :: Property
eof = 
    forAll (chooseInt (1, 100)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (0, 10000))) $ \ls ->
            case S.parse P.eof (S.fromList ls) of
                Right _ -> False
                Left _ -> True
    .&&.
    property (case S.parse P.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

satisfy :: Property
satisfy = 
    forAll (listOf (chooseInt (0, 10000))) $ \ls ->
        case S.parse (P.satisfy predicate) (S.fromList ls) of
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
            case S.parse (P.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> parsed_list == Prelude.take n ls
                Left _ -> False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == Prelude.take n ls)
                    Left _ -> n > list_length

takeGE :: Property
takeGE =
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parse (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == ls)
                    Left _ -> n > list_length

lookAhead :: Property
lookAhead =
    forAll (chooseInt (0, 10000)) $ \n -> 
        let
            takeWithoutConsume = P.lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (0, 10000))) $ \ls ->
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