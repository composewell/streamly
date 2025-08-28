{-# Language NoMonoLocalBinds #-}
-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
module Streamly.Test.Data.Parser.Common (main) where

import Control.Applicative ((<|>))
import Control.Exception (displayException, try, evaluate, SomeException)
import Data.List (isSuffixOf)
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Test.Common (listEquals, checkListEqual, chooseInt)
import Streamly.Internal.Data.Parser (ParseErrorPos(..))
import Test.QuickCheck
       (arbitrary, forAll, elements, Property, property, listOf,
        vectorOf, Gen, (.&&.), ioProperty)
import Test.QuickCheck.Monadic (monadicIO, assert, run, PropertyM)

import Prelude hiding (sequence)

import qualified Control.Monad.Fail as Fail
import qualified Data.List as List
import qualified Prelude
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Array.Generic as GA
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.ParserK as PK
import qualified Streamly.Internal.Data.StreamK as K

import Test.Hspec
import Test.Hspec.QuickCheck

#if MIN_VERSION_QuickCheck(2,14,0)

import Test.QuickCheck (chooseAny)
import Control.Monad.Identity (Identity(runIdentity, Identity))

#else

import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

#endif

min_value :: Int
min_value = 0

mid_value :: Int
mid_value = 5000

max_value :: Int
max_value = 10000

max_length :: Int
max_length = 1000

-- Accumulator Tests

-- TODO: Replace ParserTestCase_Temp with ParserTestCase in all the test cases.

type ParserTestCase a m b c =
        (P.Parser a m b -> [a] -> m (Either ParseErrorPos b, [a])) -> c

type ParserTestCase_Temp a m b c =
        forall t. ([a] -> t)
        -> (P.Parser a m b -> t -> m (Either ParseErrorPos b))
        -> c

fromFold :: ParserTestCase Int IO Int Property
fromFold consumer =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        ioProperty $ do
        (s1, rest) <- consumer (P.fromFold FL.sum) ls
        o2 <- S.fold FL.sum (S.fromList ls)
        pure $ rest == [] &&
            case s1 of
                Right o1 -> o1 == o2
                Left _ -> False

fromPure :: ParserTestCase Int Identity Int Property
fromPure consumer =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case runIdentity $ consumer (P.fromPure x) [1 :: Int] of
            (Right r, rest) -> r == x && rest == [1 :: Int]
            (Left _, _) -> False

fromEffect :: ParserTestCase Int Identity Int Property
fromEffect consumer =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case runIdentity $ consumer (P.fromEffect $ return x) [1 :: Int] of
            (Right r, rest) -> r == x && rest == [1 :: Int]
            (Left _, _) -> False

die :: ParserTestCase Int Identity Int Property
die consumer =
    property $
    case runIdentity $ consumer (P.die "die test") [0 :: Int] of
        (Right _, _) -> False
        (Left _, rest) -> rest == [0 :: Int]

dieM :: ParserTestCase Int Identity Int Property
dieM consumer =
    property $
    case runIdentity $ consumer (P.dieM (Identity "die test")) [0 :: Int] of
        (Right _, _) -> False
        (Left _, rest) -> rest == [0 :: Int]

parserFail :: ParserTestCase Int Identity Int Property
parserFail consumer =
    property $
        case runIdentity $ consumer (Fail.fail err) [0 :: Int] of
            (Right _, _) -> False
            (Left (ParseErrorPos _ e), rest) -> err == e && rest == [0 :: Int]
    where
    err = "Testing MonadFail.fail."

-- Element Parser Tests

peekPass :: ParserTestCase Int Identity Int Property
peekPass consumer =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ consumer P.peek ls of
                (Right head_value, rest) -> case ls of
                    head_ls : _ -> head_value == head_ls && rest == ls
                    _ -> False
                (Left _, _) -> False

peekFail :: ParserTestCase Int Identity Int Property
peekFail consumer =
    property (case runIdentity $ consumer P.peek [] of
        (Right _, _) -> False
        (Left _, rest) -> rest == [])

eofPass :: ParserTestCase_Temp Int Identity () Property
eofPass producer consumer =
    property (case runIdentity $ consumer P.eof (producer []) of
        Right _ -> True
        Left _ -> False)

eofFail :: ParserTestCase_Temp Int Identity () Property
eofFail producer consumer =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ consumer P.eof (producer ls) of
                Right _ -> False
                Left _ -> True

satisfyPass :: ParserTestCase_Temp Int Identity Int Property
satisfyPass producer consumer =
    forAll (chooseInt (mid_value, max_value)) $ \first_element ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls_tail ->
            let
                ls = first_element : ls_tail
                predicate = (>= mid_value)
            in
                case runIdentity $ consumer (P.satisfy predicate) (producer ls) of
                    Right r -> r == first_element
                    Left _ -> False

satisfy :: ParserTestCase_Temp Int Identity Int Property
satisfy producer consumer =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        case runIdentity $ consumer (P.satisfy predicate) (producer ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= mid_value)

onePass :: ParserTestCase_Temp Int Identity Int Property
onePass producer consumer =
    forAll (chooseInt (1, max_value)) $ \int ->
        property (case runIdentity $ consumer P.one (producer [int]) of
            Right i -> i  == int
            Left _ -> False)

one :: ParserTestCase_Temp Int Identity Int Property
one producer consumer =
    property $
        case runIdentity $ consumer P.one (producer []) of
            Left _ -> True
            Right _ -> False

-- Sequence Parsers Tests
takeBetweenPass :: ParserTestCase_Temp Int Identity [Int] Property
takeBetweenPass producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \m ->
        forAll (chooseInt (m, max_value)) $ \n ->
            forAll (chooseInt (m, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                    $ \ls ->
                        case runIdentity $ consumer (P.takeBetween m n FL.toList)
                                (producer ls) of
                            Right parsed_list ->
                                let lpl = Prelude.length parsed_list
                                in checkListEqual parsed_list
                                    $ Prelude.take lpl ls
                            Left _ -> property False

takeBetween :: ParserTestCase_Temp Int Identity [Int] Property
takeBetween producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \m ->
        forAll (chooseInt (min_value, max_value)) $ \n ->
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                ioProperty $ go m n ls

    where

    go m n ls = do
        let inputLen = Prelude.length ls
        let p = P.takeBetween m n FL.toList
        eres <- try $ evaluate $ runIdentity $ consumer p (producer ls)
        pure $ case eres of
            Left (_ :: SomeException) -> m >= 0 && n >= 0 && m > n
            Right (Right xs) ->
                let parsedLen = Prelude.length xs
                in (inputLen >= m && parsedLen >= m && parsedLen <= n)
                       && (xs == Prelude.take parsedLen ls)
            Right (Left _) -> inputLen < m


take :: ParserTestCase_Temp Int Identity [Int] Property
take producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ consumer (P.fromFold $ FL.take n FL.toList) (producer ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                Left _ -> property False

takeEQPass :: ParserTestCase_Temp Int Identity [Int] Property
takeEQPass producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length
                        (chooseInt (min_value, max_value))) $ \ls ->
                case runIdentity $ consumer (P.takeEQ n FL.toList) (producer ls) of
                    Right parsed_list ->
                        checkListEqual parsed_list (Prelude.take n ls)
                    Left _ -> property False

takeEQ :: ParserTestCase_Temp Int Identity [Int] Property
takeEQ producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case runIdentity $ consumer (P.takeEQ n FL.toList) (producer ls) of
                    Right parsed_list ->
                        if n <= list_length
                        then checkListEqual parsed_list (Prelude.take n ls)
                        else property False
                    Left _ -> property (n > list_length)

takeGEPass :: ParserTestCase_Temp Int Identity [Int] Property
takeGEPass producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                $ \ls ->
                    case runIdentity $ consumer (P.takeGE n FL.toList) (producer ls) of
                        Right parsed_list -> checkListEqual parsed_list ls
                        Left _ -> property False

takeGE :: ParserTestCase_Temp Int Identity [Int] Property
takeGE producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case runIdentity $ consumer (P.takeGE n FL.toList) (producer ls) of
                    Right parsed_list ->
                        if n <= list_length
                        then checkListEqual parsed_list ls
                        else property False
                    Left _ -> property (n > list_length)

nLessThanEqual0 ::
       (  Int
       -> FL.Fold Identity Int [Int]
       -> P.Parser Int Identity [Int]
       )
    -> (Int -> [Int] -> [Int])
    -> ParserTestCase_Temp Int Identity [Int] Property
nLessThanEqual0 tk ltk producer consumer =
    forAll (elements [0, (-1)]) $ \n ->
        forAll (listOf arbitrary) $ \ls ->
            case runIdentity $ consumer (tk n FL.toList) (producer ls) of
                Right parsed_list -> checkListEqual parsed_list (ltk n ls)
                Left _ -> property False

takeProperties :: ParserTestCase_Temp Int Identity [Int] Spec
takeProperties producer consumer =
    describe "take combinators when n <= 0/" $ do
        prop "takeEQ n FL.toList = []" $
            nLessThanEqual0 P.takeEQ (\_ -> const []) producer consumer
        prop "takeGE n FL.toList xs = xs" $
            nLessThanEqual0 P.takeGE (\_ -> id) producer consumer

-- XXX lookAhead can't deal with EOF which in this case means when
-- n==list_length, this test will fail. So excluding that case for now.
lookAheadPass :: ParserTestCase_Temp Int Identity ([Int], [Int]) Property
lookAheadPass producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        let
            takeWithoutConsume = P.lookAhead $ P.fromFold $ FL.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (chooseInt (n+1, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                    case runIdentity $ consumer parseTwice (producer ls) of
                        Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                        Left _ -> property $ False

-- lookAheadFail :: ParserTestCase_Temp Int Identity Int Property
-- lookAheadFail producer consumer =
--     forAll (chooseInt (min_value + 1, max_value)) $ \n ->
--         let
--             takeWithoutConsume = P.lookAhead $ P.take n FL.toList
--             parseTwice = do
--                 parsed_list_1 <- takeWithoutConsume
--                 parsed_list_2 <- takeWithoutConsume
--                 return (parsed_list_1, parsed_list_2)
--         in
--             forAll (chooseInt (min_value, n - 1)) $ \list_length ->
--                 forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
--                     case consumer parseTwice (producer ls) of
--                         Right _ -> False
--                         Left _ -> True

lookAhead :: ParserTestCase_Temp Int Identity ([Int], [Int]) Property
lookAhead producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        let
            takeWithoutConsume = P.lookAhead $ P.fromFold $ FL.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                case runIdentity $ consumer parseTwice (producer ls) of
                    Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                    Left _ -> property ((list_length < n) || (list_length == n && n == 0))
                        where
                            list_length = Prelude.length ls

listTakeEndBy_ :: (a -> Bool) -> [a] -> ([a], [a])
listTakeEndBy_ predicate ls =
    case break predicate ls of
        (a, []) -> (a, [])
        (a, b) -> (a, tail b)

takeEndBy_ :: ParserTestCase Int Identity [Int] Property
takeEndBy_ consumer =
    forAll (listOf (chooseInt (min_value, max_value )))  $ \ls ->
        case runIdentity $ consumer (P.takeEndBy_ predicate prsr) ls of
            (Right parsed_list, rest) -> monadicIO $ do
                let (ls1, ls2) = listTakeEndBy_ predicate ls
                listEquals (==) parsed_list ls1
                listEquals (==) rest ls2
            _ -> property False
        where
            predicate = (>= 100)
            prsr = P.many (P.satisfy (const True)) FL.toList

takeEndByOrMax_ :: ParserTestCase Int Identity [Int] Property
takeEndByOrMax_ consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            case runIdentity $ consumer (P.fromFold $ FL.takeEndBy_ predicate (FL.take n FL.toList)) ls of
                (Right parsed_list, rest) -> monadicIO $ do
                    let (lsa, lsb) = listTakeEndBy_ predicate ls
                        (ls1, ls2) = Prelude.splitAt n ls
                    if length lsa < length ls1
                    then do
                        listEquals (==) parsed_list lsa
                        listEquals (==) rest lsb
                    else do
                        listEquals (==) parsed_list ls1
                        listEquals (==) rest ls2
                _ -> property False
            where
                predicate = (== 1)

takeStartBy :: ParserTestCase Int Identity [Int] Property
takeStartBy consumer =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let ls1 = 1:ls
        in
            case runIdentity $ consumer parser ls1 of
                (Right parsed_list, rest) ->
                  if not $ Prelude.null ls1
                  then
                    let (rls1, rls2) = Prelude.break predicate (tail ls1)
                    in monadicIO $ do
                           listEquals (==) parsed_list (head ls1 : rls1)
                           listEquals (==) rest rls2
                  else property $ Prelude.null parsed_list && Prelude.null rest
                _ -> property False
            where
                predicate = odd
                parser = P.takeBeginBy predicate FL.toList

takeWhile :: ParserTestCase Int Identity [Int] Property
takeWhile consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case runIdentity $ consumer (P.takeWhile predicate FL.toList) ls of
            (Right parsed_list, rest) -> monadicIO $ do
                let (ls1, ls2) = Prelude.span predicate ls
                listEquals (==) parsed_list ls1
                listEquals (==) rest ls2
            _ -> property False
        where
            predicate = (== 0)

takeP :: ParserTestCase Int Identity [Int] Property
takeP consumer =
    forAll
        ((,) <$> chooseInt (min_value, max_value)
             <*> listOf (chooseInt (0, 1)))
        $ \(takeNum, ls) ->
              case runIdentity $ consumer
                       (P.takeP takeNum (P.fromFold FL.toList)) ls of
                  (Right parsed_list, rest) -> monadicIO $ do
                      let (ls1, ls2) = Prelude.splitAt takeNum ls
                      listEquals (==) parsed_list ls1
                      listEquals (==) rest ls2
                  _ -> property False

takeWhile1 :: ParserTestCase Int Identity [Int] Property
takeWhile1 consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case runIdentity $ consumer (P.takeWhile1 predicate  FL.toList) ls of
            (Right parsed_list, rest) -> case ls of
                [] -> property False
                (x : _) ->
                    if predicate x
                    then monadicIO $ do
                        let (ls1, ls2) = span predicate ls
                        listEquals (==) parsed_list ls1
                        listEquals (==) rest ls2
                    else
                        property False
            (Left _, rest) -> monadicIO $ do
                listEquals (==) rest ls
                case ls of
                    [] -> assert True
                    (x : _) -> assert (not $ predicate x)
        where
            predicate = (== 0)

takeWhileP :: ParserTestCase Int Identity [Int] Property
takeWhileP consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        forAll (chooseInt (min_value, max_value)) $ \n ->
            let
                predicate = (== 1)

                prsr =
                    P.takeWhileP predicate
                        $ P.fromFold (FL.take n FL.toList)

                takeWhileTillLen maxLen prd list =
                    Prelude.take maxLen $ Prelude.takeWhile prd list
            in
                case runIdentity $ consumer prsr ls of
                    (Right parsed_list, rest) -> monadicIO $ do
                        let ls1 = takeWhileTillLen n predicate ls
                            ls2 = drop (length ls1) ls
                        listEquals (==) parsed_list ls1
                        listEquals (==) rest ls2
                    _ -> property False

{-
choice :: ParserTestCase_Temp Int Identity Int Property
choice producer consumer =
    forAll
        ((,,) <$> chooseInt (min_value, max_value)
             <*> chooseInt (min_value, max_value)
             <*> listOf (chooseInt (0, 1)))
        $ \(i, j, ls) ->
              case consumer (P.choice [parser i, parser j]) (producer ls) of
                  Right parsed_list ->
                      checkListEqual parsed_list $ take (min i j) ls
                  Left _ -> property False

    where

    parser i = P.fromFold (FL.take i FL.toList)
-}

groupBy :: ParserTestCase_Temp Int Identity [Int] Property
groupBy producer consumer =
    forAll (listOf (chooseInt (0, 1)))
        $ \ls ->
              case runIdentity $ consumer parser (producer ls) of
                  Right parsed -> checkListEqual parsed (groupByLF ls)
                  Left _ -> property False

    where

    cmp = (==)
    parser = P.groupBy cmp FL.toList
    groupByLF lst
        | null lst = []
        | otherwise = head $ List.groupBy cmp lst

groupByRolling :: ParserTestCase_Temp Int Identity [Int] Property
groupByRolling producer consumer =
    forAll (listOf (chooseInt (0, 1)))
        $ \ls ->
              case runIdentity $ consumer parser (producer ls) of
                  Right parsed -> checkListEqual parsed (groupByLF Nothing ls)
                  Left _ -> property False

    where

    cmp = (==)
    parser = P.groupBy cmp FL.toList
    groupByLF _ [] = []
    groupByLF Nothing (x:xs) = x : groupByLF (Just x) xs
    groupByLF (Just y) (x:xs) =
        if cmp y x
        then x : groupByLF (Just x) xs
        else []

wordBy :: ParserTestCase_Temp Char Identity [String] Property
wordBy producer consumer =
    forAll (listOf (elements [' ', 's']))
        $ \ls ->
              case runIdentity $ consumer parser (producer ls) of
                  Right parsed -> checkListEqual parsed (words' ls)
                  Left _ -> property False

    where

    predicate = (== ' ')
    parser = P.many (P.wordBy predicate FL.toList) FL.toList
    words' lst =
        let wrds = words lst
         in if wrds == [] && length lst > 0 then [""] else wrds

splitWith :: ParserTestCase_Temp Int Identity (Int, Int) Property
splitWith producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case runIdentity $ consumer (P.splitWith (,) (P.satisfy (== 0)) (P.satisfy (== 1))) (producer ls) of
            Right (result_first, result_second) -> case ls of
                0 : 1 : _ -> (result_first == 0) && (result_second == 1)
                _ -> False
            Left _ -> case ls of
                0 : 1 : _ -> False
                _ -> True

splitWithFailLeft :: ParserTestCase_Temp Int Identity (Int, Int) Property
splitWithFailLeft producer consumer =
    property (case runIdentity $ consumer (P.splitWith (,) (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailRight :: ParserTestCase_Temp Int Identity (Int, Int) Property
splitWithFailRight producer consumer =
    property (case runIdentity $ consumer (P.splitWith (,) (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailBoth :: ParserTestCase_Temp Int Identity (Int, Int) Property
splitWithFailBoth producer consumer =
    property (case runIdentity $ consumer (P.splitWith (,) (P.die "die") (P.die "die")) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

-- teeWithPass :: ParserTestCase_Temp Int Identity Int Property
-- teeWithPass producer consumer =
--     forAll (chooseInt (min_value, max_value)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             let
--                 prsr = P.fromFold $ FL.take n FL.toList
--             in
--                 case consumer (P.teeWith (,) prsr prsr) (producer ls) of
--                     Right (ls_1, ls_2) -> checkListEqual (Prelude.take n ls) ls_1 .&&. checkListEqual ls_1 ls_2
--                     Left _ -> property False

-- teeWithFailLeft :: ParserTestCase_Temp Int Identity Int Property
-- teeWithFailLeft producer consumer =
--     property (case consumer (P.teeWith (,) (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailRight :: ParserTestCase_Temp Int Identity Int Property
-- teeWithFailRight producer consumer =
--     property (case consumer (P.teeWith (,) (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailBoth :: ParserTestCase_Temp Int Identity Int Property
-- teeWithFailBoth producer consumer =
--     property (case consumer (P.teeWith (,) (P.die "die") (P.die "die")) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

{-
deintercalate :: ParserTestCase_Temp Int Identity Int Property
deintercalate producer consumer =
    forAll (listOf (chooseAny :: Gen Int)) $ \ls ->
        case runIdentity $ consumer p (producer ls) of
            Right evenOdd -> evenOdd == List.partition even ls
            Left _ -> False

        where
            p1 = P.takeWhile even FL.toList
            p2 = P.takeWhile odd FL.toList
            partition =
                FL.tee (fmap concat $ FL.catLefts FL.toList)
                       (fmap concat $ FL.catRights FL.toList)
            p = P.deintercalate p1 p2 partition
-}

-- shortestPass :: ParserTestCase_Temp Int Identity Int Property
-- shortestPass producer consumer =
--     forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
--         let
--             half_mid_value = mid_value `Prelude.div` 2
--             prsr_1 = P.takeWhile (<= half_mid_value) FL.toList
--             prsr_2 = P.takeWhile (<= mid_value) FL.toList
--             prsr_shortest = P.shortest prsr_1 prsr_2
--         in
--             case consumer prsr_shortest (producer ls) of
--                 Right short_list -> checkListEqual short_list (Prelude.takeWhile (<= half_mid_value) ls)
--                 Left _ -> property False

-- shortestPassLeft :: ParserTestCase_Temp Int Identity Int Property
-- shortestPassLeft producer consumer =
--     property (case consumer (P.shortest (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- shortestPassRight :: ParserTestCase_Temp Int Identity Int Property
-- shortestPassRight producer consumer =
--     property (case consumer (P.shortest (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailBoth :: ParserTestCase_Temp Int Identity Int Property
-- shortestFailBoth producer consumer =
--     property
--         (case consumer
--                   (P.shortest (P.die "die") (P.die "die"))
--                   (producer [1 :: Int]) of
--              Right _ -> False
--              Left _ -> True)
--
-- longestPass :: ParserTestCase_Temp Int Identity Int Property
-- longestPass producer consumer =
--     forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
--         let
--             half_mid_value = mid_value `Prelude.div` 2
--             prsr_1 = P.takeWhile (<= half_mid_value) FL.toList
--             prsr_2 = P.takeWhile (<= mid_value) FL.toList
--             prsr_longest = P.longest prsr_1 prsr_2
--         in
--             case consumer prsr_longest (producer ls) of
--                 Right long_list -> long_list == Prelude.takeWhile (<= mid_value) ls
--                 Left _ -> False
--
-- longestPassLeft :: ParserTestCase_Temp Int Identity Int Property
-- longestPassLeft producer consumer =
--     property (case consumer (P.shortest (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- longestPassRight :: ParserTestCase_Temp Int Identity Int Property
-- longestPassRight producer consumer =
--     property (case consumer (P.shortest (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- longestFailBoth :: ParserTestCase_Temp Int Identity Int Property
-- longestFailBoth producer consumer =
--     property
--         (case consumer (P.shortest (P.die "die") (P.die "die")) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

many :: ParserTestCase_Temp Int Identity [Int] Property
many producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let fldstp conL currL = return $ FL.Partial (conL ++ currL)
            concatFold = FL.Fold fldstp (return (FL.Partial [])) return return
            prsr =
                flip P.many concatFold
                    $ P.fromFold $ FL.takeEndBy_ (== 1) FL.toList
         in case runIdentity $ consumer prsr (producer ls) of
                Right res_list ->
                    checkListEqual res_list (Prelude.filter (== 0) ls)
                Left _ -> property False

many_empty :: ParserTestCase_Temp Int Identity [Int] Property
many_empty producer consumer =
    property (case runIdentity $ consumer (flip P.many FL.toList (P.die "die")) (producer [1 :: Int]) of
        Right res_list -> checkListEqual res_list ([] :: [Int])
        Left _ -> property False)

some :: ParserTestCase_Temp Int Identity [Int] Property
some producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \genLs ->
        let
            ls = 0 : genLs
            fldstp conL currL = return $ FL.Partial $ conL ++ currL
            concatFold = FL.Fold fldstp (return (FL.Partial [])) return return
            prsr =
                flip P.some concatFold
                    $ P.fromFold $ FL.takeEndBy_ (== 1) FL.toList
         in case runIdentity $ consumer prsr (producer ls) of
                Right res_list -> res_list == Prelude.filter (== 0) ls
                Left _ -> False

someFail :: ParserTestCase_Temp Int Identity [Int] Property
someFail producer consumer =
    property (case runIdentity $ consumer (P.some (P.die "die") FL.toList) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

applicative :: ParserTestCase_Temp Int Identity ([Int], [Int]) Property
applicative producer consumer =
    forAll (listOf (chooseAny :: Gen Int)) $ \ list1 ->
        forAll (listOf (chooseAny :: Gen Int)) $ \ list2 ->
            let parser =
                    (,)
                        <$> P.fromFold (FL.take (length list1) FL.toList)
                        <*> P.fromFold (FL.take (length list2) FL.toList)
             in
                case runIdentity $ consumer parser (producer $ list1 ++ list2) of
                    Right (olist1, olist2) -> olist1 == list1 && olist2 == list2
                    Left _ -> False

sequence :: ParserTestCase_Temp Int IO [[Int]] Property
sequence producer consumer =
    forAll (vectorOf 11 (listOf (chooseAny :: Gen Int))) $ \ ins ->
        let p xs = P.fromFold (FL.take (length xs) FL.toList)
         in monadicIO $ do
                outs <- run $
                        consumer
                            (Prelude.sequence $ fmap p ins)
                            (producer $ concat ins)
                return $
                    case outs of
                        Right ls -> ls == ins
                        Left _ -> False

altEOF1 :: ParserTestCase_Temp Int (PropertyM IO) Int Property
altEOF1 producer consumer =
    monadicIO $ do
    s1 <- consumer
        (P.satisfy (> 0) <|> return 66)
        (producer ([]::[Int]))
    return $
        case s1 of
            Right x -> x == 66
            Left _ -> False

altEOF2 :: ParserTestCase_Temp Int (PropertyM IO) [Int] Property
altEOF2 producer consumer =
    monadicIO $ do
    s1 <- consumer
        ((P.takeEQ 2 FL.toList) <|> (P.takeEQ 1 FL.toList))
        (producer ([51]::[Int]))
    return $
        case s1 of
            Right x -> x == [51]
            Left _ -> False

monad :: ParserTestCase_Temp Int (PropertyM IO) ([Int], [Int]) Property
monad producer consumer =
    forAll (listOf (chooseAny :: Gen Int)) $ \ list1 ->
        forAll (listOf (chooseAny :: Gen Int)) $ \ list2 ->
            let parser = do
                    olist1 <- P.fromFold (FL.take (length list1) FL.toList)
                    olist2 <- P.fromFold (FL.take (length list2) FL.toList)
                    return (olist1, olist2)
             in monadicIO $ do
                    s <- consumer parser (producer $ list1 ++ list2)
                    return $
                        case s of
                            Right (olist1, olist2) -> olist1 == list1 && olist2 == list2
                            Left _ -> False

takeEndBy1 :: ParserTestCase_Temp Int Identity [Int] Property
takeEndBy1 producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case runIdentity $ consumer (P.takeEndBy predicate prsr) (producer ls) of
            Right parsed_list ->
                checkListEqual
                parsed_list
                (takeWhileAndFirstFail (not . predicate) ls)
            Left _ -> property False
        where
            prsr = P.many (P.satisfy (const True)) FL.toList

            predicate = (== 1)

            takeWhileAndFirstFail prd (x : xs) =
                if prd x
                then x : takeWhileAndFirstFail prd xs
                else [x]
            takeWhileAndFirstFail _ [] = []

takeEndByEsc :: ParserTestCase_Temp Int Identity [Int] Property
takeEndByEsc producer consumer =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let
            msg = "takeEndByEsc: trailing escape"

            isSep = even

            isEsc x = x `mod` 6 == 0

            prsr = P.takeEndByEsc isEsc isSep prsr0

                where

                prsr0 = P.many (P.satisfy (const True)) FL.toList

            escapeSep maybePrevEsc [] =
                case maybePrevEsc of
                    Nothing -> []
                    Just prevEsc -> [prevEsc]
            escapeSep maybePrevEsc (x : xs) =
                 case maybePrevEsc of
                    Nothing ->
                        if isEsc x
                        then escapeSep (Just x) xs
                        else
                            if isSep x
                            then [x]
                            else x : escapeSep Nothing xs
                    Just _ ->
                            x : escapeSep Nothing xs
        in
            case runIdentity $ consumer prsr (producer ls) of
                Right parsed_list -> checkListEqual parsed_list $ escapeSep Nothing ls
                Left err -> property (msg `isSuffixOf` displayException err)

takeFramedByEsc_ :: ParserTestCase_Temp Int Identity [Int] Property
takeFramedByEsc_ producer consumer =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let
            isBegin = (== 0)

            isEnd = (== 1)

            isEsc = (== 2)

            prsr = P.takeFramedByEsc_ isEsc isBegin isEnd  FL.toList

            checkPass (x : xs) maybePrevEsc openMinusClose =
                case maybePrevEsc of
                    Nothing ->
                        if isEsc x
                        then checkPass xs (Just x) openMinusClose
                        else
                            if isBegin x
                            then checkPass xs Nothing (openMinusClose + 1)
                            else
                                if isEnd x
                                then
                                    case openMinusClose of
                                        0 -> False
                                        1 -> True
                                        _ ->
                                            checkPass
                                            xs
                                            Nothing
                                            (openMinusClose - 1)
                                else
                                    checkPass xs Nothing openMinusClose
                    Just _ -> checkPass xs Nothing openMinusClose
            checkPass [] _ _ = False

            checkPassBeg [] = False
            checkPassBeg xxs@(x:_)
                | isBegin x = checkPass xxs Nothing (0 :: Int)
                | otherwise = False

            escapeFrame begin end escape l =
                let
                    helper (x : xs) maybePrevEsc openMinusClose =
                        case maybePrevEsc of
                            Nothing ->
                                if escape x
                                then helper xs (Just x) openMinusClose
                                else
                                    if begin x
                                    then helper xs Nothing (openMinusClose + 1)
                                    else
                                        if end x
                                        then
                                            if openMinusClose - 1 == 0
                                            then []
                                            else
                                                helper
                                                xs
                                                Nothing
                                                (openMinusClose - 1)
                                        else
                                            x : helper xs Nothing openMinusClose
                            Just prevEsc ->
                                if escape x || begin x || end x
                                then x : helper xs Nothing openMinusClose
                                else
                                    prevEsc : x : helper xs Nothing openMinusClose
                    helper [] _ _ = error "Cannot Reach Here"
                in
                    helper l Nothing (0 :: Int)
        in
            case runIdentity $ consumer prsr (producer ls) of
                Right parsed_list ->
                    if checkPassBeg ls
                    then checkListEqual parsed_list $
                        escapeFrame isBegin isEnd isEsc ls
                    else property False
                Left _ ->
                    if checkPassBeg ls
                    then property False
                    else property True

takeFramedByEsc_Pass :: ParserTestCase_Temp Int Identity [Int] Property
takeFramedByEsc_Pass producer consumer =
    forAll (listOf (chooseInt (min_value, max_value))) $ \list ->
        let
            ls = (0 : list) ++ (Prelude.replicate (Prelude.length list + 1) 1)

            isBegin = (== 0)

            isEnd = (== 1)

            isEsc = (== 2)

            prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

            escapeFrame begin end escape l =
                let
                    helper (x : xs) maybePrevEsc openMinusClose =
                        case maybePrevEsc of
                            Nothing ->
                                if escape x
                                then helper xs (Just x) openMinusClose
                                else
                                    if begin x
                                    then
                                        if openMinusClose == 0
                                        then helper xs Nothing (openMinusClose + 1)
                                        else x : helper xs Nothing (openMinusClose + 1)
                                    else
                                        if end x
                                        then
                                            if openMinusClose - 1 == 0
                                            then []
                                            else
                                                x :
                                                helper
                                                xs
                                                Nothing
                                                (openMinusClose - 1)
                                        else
                                            x : helper xs Nothing openMinusClose
                            Just _ ->
                                x : helper xs Nothing openMinusClose
                    helper [] _ _ = error "Cannot Reach Here"
                in
                    helper l Nothing (0 :: Int)
        in
            case runIdentity $ consumer prsr (producer ls) of
                Right parsed_list -> checkListEqual parsed_list $ escapeFrame isBegin isEnd isEsc ls
                _ -> property False

takeFramedByEsc_Fail1 :: ParserTestCase_Temp Int Identity [Int] Property
takeFramedByEsc_Fail1 producer consumer =
    let
        msg = "takeFramedByEsc_: missing frame end"

        isBegin = (== 0)

        isEnd = (== 0)

        isEsc = (== 2)

        prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

        ls = [0 :: Int]
    in
        case runIdentity $ consumer prsr (producer ls) of
            Right _ -> property False
            Left err -> property (msg `isSuffixOf` displayException err)

takeFramedByEsc_Fail2 :: ParserTestCase_Temp Int Identity [Int] Property
takeFramedByEsc_Fail2 producer consumer =
    let
        msg = "takeFramedByEsc_: missing frame start"

        isBegin = (== 0)

        isEnd = (== 1)

        isEsc = (== 1)

        prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

        ls = [1 :: Int]
    in
        case runIdentity $ consumer prsr (producer ls) of
            Right _ -> property False
            Left err -> property (msg `isSuffixOf` displayException err)

takeFramedByEsc_Fail3 :: ParserTestCase_Temp Int Identity [Int] Property
takeFramedByEsc_Fail3 producer consumer =
    let
        msg = "takeFramedByEsc_: missing frame end"

        isBegin = (== 2)

        isEnd = (== 1)

        isEsc = (== 2)

        prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

        ls = [2 :: Int]
    in
        case runIdentity $ consumer prsr (producer ls) of
            Right _ -> property False
            Left err -> property (msg `isSuffixOf` displayException err)

takeStartBy_ :: ParserTestCase_Temp Int Identity [Int] Property
takeStartBy_ producer consumer =
     forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let ls1 = 1:ls
            msg = "takeFramedByGeneric: empty token"
        in
            case runIdentity $ consumer parser (producer ls1) of
                Right parsed_list ->
                  if not $ Prelude.null ls1
                  then
                    let tls = Prelude.takeWhile (not . predicate) (tail ls1)
                    in checkListEqual parsed_list $
                      if predicate (head ls1)
                      then tls
                      else Prelude.takeWhile (not . predicate) ls1
                  else property $ Prelude.null parsed_list
                Left err -> property (msg `isSuffixOf` displayException err)
            where
                predicate = odd
                parser = P.takeBeginBy_ predicate FL.toList

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

data TestMode
    = TMParserStream
    | TMParserKStreamK
    | TMParserKStreamKChunks
    | TMParserKStreamKChunksGeneric
    deriving (Show)

runParserTC :: (Unbox a, Monad m) => TestMode -> ParserTestCase a m b c -> c
runParserTC tm runner =
    case tm of
        TMParserStream ->
            runner $ \p -> mapMTup S.toList . S.parseBreakPos p . S.fromList
        TMParserKStreamK ->
            runner $ \p ->
                mapMTup K.toList . K.parseBreakPos (PK.toParserK p) . K.fromList
        TMParserKStreamKChunks ->
            runner $ \p ->
                mapMTup
                    (fmap (concatMap A.toList) . K.toList)
                        . A.parseBreakPos (A.toParserK p)
                        . producerChunks A.fromList
        TMParserKStreamKChunksGeneric ->
            runner $ \p ->
                mapMTup
                    (fmap (concatMap GA.toList) . K.toList)
                        . GA.parseBreakPos (GA.toParserK p)
                        . producerChunks GA.fromList

    where
    mapMTup f tupM = do
        (t, a) <- tupM
        (t,) <$> f a

    cSize = 50
    -- Not using A.createOf here because of the MonadIO constraint
    producerChunks fl =
        K.fromStream
             . S.groupsOf cSize (fl <$> FL.toList)
             . S.fromList

runParserTC_temp :: (Unbox a, Monad m) => TestMode -> ParserTestCase_Temp a m b c -> c
runParserTC_temp tm runner =
    case tm of
        TMParserStream -> runner S.fromList S.parsePos
        TMParserKStreamK -> runner K.fromList (K.parsePos . PK.toParserK)
        TMParserKStreamKChunks ->
            runner (producerChunks A.fromList) (A.parsePos . A.toParserK)
        TMParserKStreamKChunksGeneric ->
            runner
                (producerChunks GA.fromList)
                (GA.parsePos . GA.toParserK)

    where
    cSize = 50
    -- Not using A.createOf here because of the MonadIO constraint
    producerChunks fl =
        K.fromStream
             . S.groupsOf cSize (fl <$> FL.toList)
             . S.fromList

{-# NOINLINE mainCommon #-}
mainCommon :: TestMode -> Spec
mainCommon ptt = do
  describe (show ptt) $ do
    describe "Instances" $ do
        prop "applicative" $ runParserTC_temp ptt applicative
        prop "Alternative: end of input 1" $ runParserTC_temp ptt altEOF1
        prop "Alternative: end of input 2" $ runParserTC_temp ptt altEOF2
        prop "monad" $ runParserTC_temp ptt monad
        prop "sequence" $ runParserTC_temp ptt sequence

    describe "test for accumulator" $ do
        prop "P.fromFold FL.sum = FL.sum" $ runParserTC ptt fromFold
        prop "fromPure value provided" $ runParserTC ptt fromPure
        prop "fromPure monadic value provided" $ runParserTC ptt fromEffect
        prop "fail err = Left (SomeException (ParseError err))" $ runParserTC ptt parserFail
        prop "always fail" $ runParserTC ptt die
        prop "always fail but monadic" $ runParserTC ptt dieM

    describe "test for element parser" $ do
        prop "peek = head with list length > 0" $ runParserTC ptt peekPass
        prop "peek fail on []" $ runParserTC ptt peekFail
        prop "eof pass on []" $ runParserTC_temp ptt eofPass
        prop "eof fail on non-empty list" $ runParserTC_temp ptt eofFail
        prop "first element exists and >= mid_value" $ runParserTC_temp ptt satisfyPass
        prop "one pass on [Int]" $ runParserTC_temp ptt onePass
        prop "one fail on []" $ runParserTC_temp ptt one
        prop "check first element exists and satisfies predicate" $ runParserTC_temp ptt satisfy
    describe "test for sequence parser" $ do
        prop "P.takeBetween = Prelude.take when len >= m and len <= n"
            $ runParserTC_temp ptt takeBetweenPass
        prop ("P.takeBetween = Prelude.take when len >= m and len <= n and fail"
              ++ "otherwise fail") $ runParserTC_temp ptt takeBetween
        prop "P.take = Prelude.take" $ runParserTC_temp ptt Streamly.Test.Data.Parser.Common.take
        prop "P.takeEQ = Prelude.take when len >= n" $ runParserTC_temp ptt takeEQPass
        prop "P.takeEQ = Prelude.take when len >= n and fail otherwise"
            $ runParserTC_temp ptt Streamly.Test.Data.Parser.Common.takeEQ
        prop "P.takeGE n ls = ls when len >= n" $ runParserTC_temp ptt takeGEPass
        prop "P.takeGE n ls = ls when len >= n and fail otherwise" $ runParserTC_temp ptt Streamly.Test.Data.Parser.Common.takeGE
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" $ runParserTC_temp ptt lookAheadPass
        -- prop "Fail when stream length exceeded" lookAheadFail
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" $ runParserTC_temp ptt lookAhead
        prop ("P.takeStartBy pred = head : Prelude.takeWhile (not . pred)"
                ++ " tail") $ runParserTC ptt takeStartBy
        prop "P.takeWhile = Prelude.takeWhile" $ runParserTC ptt Streamly.Test.Data.Parser.Common.takeWhile
        prop ("P.takeWhile1 = Prelude.takeWhile if taken something,"
                ++ " else check why failed") $ runParserTC ptt takeWhile1
        prop "takeWhileP prd P.take = takeWhileMaxLen prd" $ runParserTC ptt takeWhileP
        prop ("P.takeP = Prelude.take") $ runParserTC ptt takeP
        prop "P.groupBy = Prelude.head . Prelude.groupBy" $ runParserTC_temp ptt groupBy
        prop "groupByRolling" $ runParserTC_temp ptt groupByRolling
        prop "many (P.wordBy ' ') = words'" $ runParserTC_temp ptt wordBy
        -- prop "choice" choice
        prop "parse 0, then 1, else fail" $ runParserTC_temp ptt splitWith
        prop "fail due to die as left parser" $ runParserTC_temp ptt splitWithFailLeft
        prop "fail due to die as right parser" $ runParserTC_temp ptt splitWithFailRight
        prop "fail due to die as both parsers" $ runParserTC_temp ptt splitWithFailBoth
        -- prop "" teeWithPass
        -- prop "" teeWithFailLeft
        -- prop "" teeWithFailRight
        -- prop "" teeWithFailBoth
        -- prop "deintercalate" deintercalate
        -- prop "" shortestPass
        -- prop "" shortestFailLeft
        -- prop "" shortestFailRight
        -- prop "" shortestFailBoth
        prop ("P.many concatFold $ P.takeEndBy_ (== 1) FL.toList ="
                ++ "Prelude.filter (== 0)") $ runParserTC_temp ptt many
        prop "[] due to parser being die" $ runParserTC_temp ptt many_empty
        prop ("P.some concatFold $ P.takeEndBy_ (== 1) FL.toList ="
                ++ "Prelude.filter (== 0)") $ runParserTC_temp ptt some
        prop "fail due to parser being die" $ runParserTC_temp ptt someFail
        prop "takeEndBy_" $ runParserTC ptt takeEndBy_
        prop "takeEndByOrMax_" $ runParserTC ptt takeEndByOrMax_
        prop "takeEndBy1" $ runParserTC_temp ptt takeEndBy1
        prop "takeEndByEsc" $ runParserTC_temp ptt takeEndByEsc
        prop "takeFramedByEsc_" $ runParserTC_temp ptt takeFramedByEsc_
        prop "takeFramedByEsc_Pass" $ runParserTC_temp ptt takeFramedByEsc_Pass
        prop "takeFramedByEsc_Fail1" $ runParserTC_temp ptt takeFramedByEsc_Fail1
        prop "takeFramedByEsc_Fail2" $ runParserTC_temp ptt takeFramedByEsc_Fail2
        prop "takeFramedByEsc_Fail3" $ runParserTC_temp ptt takeFramedByEsc_Fail3
        prop "takeStartBy_" $ runParserTC_temp ptt takeStartBy_

    runParserTC_temp ptt takeProperties

main :: Spec
main = do
        -- We keep Parser and ParserK tests in the same (Parser) executable for 2
        -- reasons:
        -- 1. We almost always write Parser tests hence we prioritize Parser over
        --    ParserK
        -- 2. This results in minimal compilation overhead compared to duplicating
        --    or keeping the common part in the library.
        --    2.1. Duplication will result in compilation of this code twice
        --    2.2. Keeping the common part in the library will compile the Parser
        --         code even when it's not necessary. For example, if we are running
        --         non-parser test suites.
        --
        -- One problem is that this module becomes very big for compilation. We can
        -- break this further and keep them as a part of "other-modules" in
        -- Test.Parser test-suite.
        mainCommon TMParserStream
        mainCommon TMParserKStreamKChunks
        mainCommon TMParserKStreamK
        mainCommon TMParserKStreamKChunksGeneric
