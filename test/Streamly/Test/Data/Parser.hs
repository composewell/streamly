-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Data.Foldable (for_)
import Data.Word (Word8, Word32, Word64)
import Streamly.Test.Common (listEquals, checkListEqual, chooseInt)
import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck
       (arbitrary, forAll, elements, Property, property, listOf,
        vectorOf, Gen, (.&&.))
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Prelude hiding (sequence)

import qualified Control.Monad.Fail as Fail
import qualified Data.List as List
import qualified Prelude
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Test.Hspec as H

#if MIN_VERSION_QuickCheck(2,14,0)

import Test.QuickCheck (chooseAny)
import Control.Monad.Identity (Identity(runIdentity, Identity))
import Streamly.Internal.Data.Parser (ParseError(..))

#else

import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

#endif

maxTestCount :: Int
maxTestCount = 100

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
        monadicIO $ do
        s1 <- S.parse (P.fromFold FL.sum) (S.fromList ls)
        o2 <- S.fold FL.sum (S.fromList ls)
        return $
            case s1 of
                Right o1 -> o1 == o2
                Left _ -> False

fromPure :: Property
fromPure =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case runIdentity $ S.parse (P.fromPure x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

fromEffect :: Property
fromEffect =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case runIdentity $ S.parse (P.fromEffect $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

die :: Property
die =
    property $
    case runIdentity $ S.parse (P.die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

dieM :: Property
dieM =
    property $
    case runIdentity $ S.parse (P.dieM (Identity "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

parserFail :: Property
parserFail =
    property $
        case runIdentity $ S.parse (Fail.fail err) (S.fromList [0 :: Int]) of
            Right _ -> False
            Left (ParseError e) -> err == e
    where
    err = "Testing MonadFail.fail."

-- Element Parser Tests

peekPass :: Property
peekPass =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ S.parse P.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False

peekFail :: Property
peekFail =
    property (case runIdentity $ S.parse P.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eofPass :: Property
eofPass =
    property (case runIdentity $ S.parse P.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

eofFail :: Property
eofFail =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ S.parse P.eof (S.fromList ls) of
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
                case runIdentity $ S.parse (P.satisfy predicate) (S.fromList ls) of
                    Right r -> r == first_element
                    Left _ -> False

satisfy :: Property
satisfy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        case runIdentity $ S.parse (P.satisfy predicate) (S.fromList ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= mid_value)

onePass :: Property
onePass =
    forAll (chooseInt (1, max_value)) $ \int ->
        property (case runIdentity $ S.parse P.one (S.fromList [int]) of
            Right i -> i  == int
            Left _ -> False)

one :: Property
one =
    property $
        case runIdentity $ S.parse P.one (S.fromList []) of
            Left _ -> True
            Right _ -> False

-- Sequence Parsers Tests
takeBetweenPass :: Property
takeBetweenPass =
    forAll (chooseInt (min_value, max_value)) $ \m ->
        forAll (chooseInt (m, max_value)) $ \n ->
            forAll (chooseInt (m, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                    $ \ls ->
                        case runIdentity $ S.parse (P.takeBetween m n FL.toList)
                                (S.fromList ls) of
                            Right parsed_list ->
                                let lpl = Prelude.length parsed_list
                                in checkListEqual parsed_list
                                    $ Prelude.take lpl ls
                            Left _ -> property False

_takeBetween :: Property
_takeBetween =
    forAll (chooseInt (min_value, max_value)) $ \m ->
        forAll (chooseInt (min_value, max_value)) $ \n ->
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                go m n ls

    where

    go m n ls =
        let inputLen = Prelude.length ls
         in do
            let p = P.takeBetween m n FL.toList
            case runIdentity $ S.parse p (S.fromList ls) of
                Right xs ->
                    let parsedLen = Prelude.length xs
                    in if inputLen >= m && parsedLen >= m && parsedLen <= n
                        then checkListEqual xs $ Prelude.take parsedLen ls
                        else property False
                Left _ ->
                    property ((m >= 0 && n >= 0 && m > n) || inputLen < m)

take :: Property
take =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ S.parse (P.fromFold $ FL.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                Left _ -> property False

takeEQPass :: Property
takeEQPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length
                        (chooseInt (min_value, max_value))) $ \ls ->
                case runIdentity $ S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        checkListEqual parsed_list (Prelude.take n ls)
                    Left _ -> property False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case runIdentity $ S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        if n <= list_length
                        then checkListEqual parsed_list (Prelude.take n ls)
                        else property False
                    Left _ -> property (n > list_length)

takeGEPass :: Property
takeGEPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                $ \ls ->
                    case runIdentity $ S.parse (P.takeGE n FL.toList) (S.fromList ls) of
                        Right parsed_list -> checkListEqual parsed_list ls
                        Left _ -> property False

takeGE :: Property
takeGE =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case runIdentity $ S.parse (P.takeGE n FL.toList) (S.fromList ls) of
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
    -> Property
nLessThanEqual0 tk ltk =
    forAll (elements [0, (-1)]) $ \n ->
        forAll (listOf arbitrary) $ \ls ->
            case runIdentity $ S.parse (tk n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (ltk n ls)
                Left _ -> property False

takeProperties :: Spec
takeProperties =
    describe "take combinators when n <= 0/" $ do
        prop "takeEQ n FL.toList = []" $
            nLessThanEqual0 P.takeEQ (\_ -> const [])
        prop "takeGE n FL.toList xs = xs" $
            nLessThanEqual0 P.takeGE (\_ -> id)

-- XXX lookAhead can't deal with EOF which in this case means when
-- n==list_length, this test will fail. So excluding that case for now.
lookAheadPass :: Property
lookAheadPass =
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
                    case runIdentity $ S.parse parseTwice (S.fromList ls) of
                        Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                        Left _ -> property $ False

-- lookAheadFail :: Property
-- lookAheadFail =
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
--                     case S.parse parseTwice (S.fromList ls) of
--                         Right _ -> False
--                         Left _ -> True

lookAhead :: Property
lookAhead =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        let
            takeWithoutConsume = P.lookAhead $ P.fromFold $ FL.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                case runIdentity $ S.parse parseTwice (S.fromList ls) of
                    Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                    Left _ -> property ((list_length < n) || (list_length == n && n == 0))
                        where
                            list_length = Prelude.length ls

takeEndBy_ :: Property
takeEndBy_ =
    forAll (listOf (chooseInt (min_value, max_value )))  $ \ls ->
        case runIdentity $ S.parse (P.takeEndBy_ predicate prsr) (S.fromList ls) of
            Right parsed_list ->
                checkListEqual parsed_list (tkwhl ls)
            Left _ -> property False
        where
            predicate = (>= 100)
            prsr = P.many (P.satisfy (const True)) FL.toList
            tkwhl ls = Prelude.takeWhile (not . predicate) ls

takeEndByOrMax_ :: Property
takeEndByOrMax_ =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            case runIdentity $ S.parse (P.fromFold $ FL.takeEndBy_ predicate (FL.take n FL.toList)) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n (Prelude.takeWhile (not . predicate) ls))
                Left _ -> property False
            where
                predicate = (== 1)

takeStartBy :: Property
takeStartBy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let ls1 = 1:ls
        in
            case runIdentity $ S.parse parser (S.fromList ls1) of
                Right parsed_list ->
                  if not $ Prelude.null ls1
                  then
                    let tls = Prelude.takeWhile (not . predicate) (tail ls1)
                    in checkListEqual parsed_list $
                      if predicate (head ls1)
                      then head ls1 : tls
                      else Prelude.takeWhile (not . predicate) ls1
                  else property $ Prelude.null parsed_list
                Left _ -> property False
            where
                predicate = odd
                parser = P.takeStartBy predicate FL.toList

takeWhile :: Property
takeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case runIdentity $ S.parse (P.takeWhile predicate FL.toList) (S.fromList ls) of
            Right parsed_list ->
                checkListEqual parsed_list (Prelude.takeWhile predicate ls)
            Left _ -> property False
        where
            predicate = (== 0)

takeP :: Property
takeP =
    forAll
        ((,) <$> chooseInt (min_value, max_value)
             <*> listOf (chooseInt (0, 1)))
        $ \(takeNum, ls) ->
              case runIdentity $ S.parse
                       (P.takeP takeNum (P.fromFold FL.toList))
                       (S.fromList ls) of
                  Right parsed_list ->
                      checkListEqual parsed_list (Prelude.take takeNum ls)
                  Left _ -> property False

takeWhile1 :: Property
takeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case runIdentity $ S.parse (P.takeWhile1 predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> case ls of
                [] -> property False
                (x : _) ->
                    if predicate x
                    then
                        checkListEqual parsed_list
                           $ Prelude.takeWhile predicate ls
                    else
                        property False
            Left _ -> case ls of
                [] -> property True
                (x : _) -> property (not $ predicate x)
        where
            predicate = (== 0)

takeWhileP :: Property
takeWhileP =
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
                case runIdentity $ S.parse prsr (S.fromList ls) of
                    Right parsed_list ->
                        checkListEqual
                        parsed_list
                        (takeWhileTillLen n predicate ls)
                    Left _ -> property False

{-
choice :: Property
choice =
    forAll
        ((,,) <$> chooseInt (min_value, max_value)
             <*> chooseInt (min_value, max_value)
             <*> listOf (chooseInt (0, 1)))
        $ \(i, j, ls) ->
              case S.parse (P.choice [parser i, parser j]) (S.fromList ls) of
                  Right parsed_list ->
                      checkListEqual parsed_list $ take (min i j) ls
                  Left _ -> property False

    where

    parser i = P.fromFold (FL.take i FL.toList)
-}

groupBy :: Property
groupBy =
    forAll (listOf (chooseInt (0, 1)))
        $ \ls ->
              case runIdentity $ S.parse parser (S.fromList ls) of
                  Right parsed -> checkListEqual parsed (groupByLF ls)
                  Left _ -> property False

    where

    cmp = (==)
    parser = P.groupBy cmp FL.toList
    groupByLF lst
        | null lst = []
        | otherwise = head $ List.groupBy cmp lst

groupByRolling :: Property
groupByRolling =
    forAll (listOf (chooseInt (0, 1)))
        $ \ls ->
              case runIdentity $ S.parse parser (S.fromList ls) of
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

wordBy :: Property
wordBy =
    forAll (listOf (elements [' ', 's']))
        $ \ls ->
              case runIdentity $ S.parse parser (S.fromList ls) of
                  Right parsed -> checkListEqual parsed (words' ls)
                  Left _ -> property False

    where

    predicate = (== ' ')
    parser = P.many (P.wordBy predicate FL.toList) FL.toList
    words' lst =
        let wrds = words lst
         in if wrds == [] && length lst > 0 then [""] else wrds

parseManyWordQuotedBy :: H.SpecWith ()
parseManyWordQuotedBy =
    describe "parseMany wordQuotedBy"
        $ for_ testCases
        $ \c@(kQ, isQ, input, expected) -> do
              let inpStrm = S.fromList input

                  esc = '\\'

                  spc ' ' = True
                  spc _ = False

                  tr _ _ = Nothing

                  parser = P.wordWithQuotes kQ tr esc isQ spc FL.toList
              result <- H.runIO $ S.fold FL.toList $ S.catRights $ S.parseMany parser inpStrm
              H.it (showCase c) $ result `H.shouldBe` expected

    where

    showCase (kQ, _, input, expected) =
        show kQ ++ ", " ++ input ++ " -> " ++ show expected

    testCases =
        [ ( True
          , \x -> if x == '\'' then Just '\'' else Nothing
          , "The quick brown fox"
          , ["The", "quick", "brown", "fox"])
        , ( True
          , \x -> if x == '\'' then Just '\'' else Nothing
          , "The' quick brown' fox"
          , ["The' quick brown'", "fox"])
        , ( False
          , \x -> if x == '\'' then Just '\'' else Nothing
          , "The' quick brown' fox"
          , ["The quick brown", "fox"])
        , ( True
          , \x -> if x == '[' then Just ']' else Nothing
          , "The[ quick brown] fox"
          , ["The[ quick brown]", "fox"])
        , ( True
          , \x -> if x == '[' then Just ']' else Nothing
          , "The[ qui[ck] brown] \\ f[  ox]"
          , ["The[ qui[ck] brown]", " f[  ox]"])
        , ( False
          , \x -> if x == '[' then Just ']' else Nothing
          , "The[ qui[ck] brown] fox"
          , ["The qui[ck] brown", "fox"])
        ]

splitWith :: Property
splitWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case runIdentity $ S.parse (P.splitWith (,) (P.satisfy (== 0)) (P.satisfy (== 1))) (S.fromList ls) of
            Right (result_first, result_second) -> case ls of
                0 : 1 : _ -> (result_first == 0) && (result_second == 1)
                _ -> False
            Left _ -> case ls of
                0 : 1 : _ -> False
                _ -> True

splitWithFailLeft :: Property
splitWithFailLeft =
    property (case runIdentity $ S.parse (P.splitWith (,) (P.die "die") (P.fromPure (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailRight :: Property
splitWithFailRight =
    property (case runIdentity $ S.parse (P.splitWith (,) (P.fromPure (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailBoth :: Property
splitWithFailBoth =
    property (case runIdentity $ S.parse (P.splitWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

-- teeWithPass :: Property
-- teeWithPass =
--     forAll (chooseInt (min_value, max_value)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             let
--                 prsr = P.fromFold $ FL.take n FL.toList
--             in
--                 case S.parse (P.teeWith (,) prsr prsr) (S.fromList ls) of
--                     Right (ls_1, ls_2) -> checkListEqual (Prelude.take n ls) ls_1 .&&. checkListEqual ls_1 ls_2
--                     Left _ -> property False

-- teeWithFailLeft :: Property
-- teeWithFailLeft =
--     property (case S.parse (P.teeWith (,) (P.die "die") (P.fromPure (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailRight :: Property
-- teeWithFailRight =
--     property (case S.parse (P.teeWith (,) (P.fromPure (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailBoth :: Property
-- teeWithFailBoth =
--     property (case S.parse (P.teeWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

{-
deintercalate :: Property
deintercalate =
    forAll (listOf (chooseAny :: Gen Int)) $ \ls ->
        case runIdentity $ S.parse p (S.fromList ls) of
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

-- shortestPass :: Property
-- shortestPass =
--     forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
--         let
--             half_mid_value = mid_value `Prelude.div` 2
--             prsr_1 = P.takeWhile (<= half_mid_value) FL.toList
--             prsr_2 = P.takeWhile (<= mid_value) FL.toList
--             prsr_shortest = P.shortest prsr_1 prsr_2
--         in
--             case S.parse prsr_shortest (S.fromList ls) of
--                 Right short_list -> checkListEqual short_list (Prelude.takeWhile (<= half_mid_value) ls)
--                 Left _ -> property False

-- shortestPassLeft :: Property
-- shortestPassLeft =
--     property (case S.parse (P.shortest (P.die "die") (P.fromPure (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- shortestPassRight :: Property
-- shortestPassRight =
--     property (case S.parse (P.shortest (P.fromPure (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailBoth :: Property
-- shortestFailBoth =
--     property
--         (case S.parse
--                   (P.shortest (P.die "die") (P.die "die"))
--                   (S.fromList [1 :: Int]) of
--              Right _ -> False
--              Left _ -> True)
--
-- longestPass :: Property
-- longestPass =
--     forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
--         let
--             half_mid_value = mid_value `Prelude.div` 2
--             prsr_1 = P.takeWhile (<= half_mid_value) FL.toList
--             prsr_2 = P.takeWhile (<= mid_value) FL.toList
--             prsr_longest = P.longest prsr_1 prsr_2
--         in
--             case S.parse prsr_longest (S.fromList ls) of
--                 Right long_list -> long_list == Prelude.takeWhile (<= mid_value) ls
--                 Left _ -> False
--
-- longestPassLeft :: Property
-- longestPassLeft =
--     property (case S.parse (P.shortest (P.die "die") (P.fromPure (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- longestPassRight :: Property
-- longestPassRight =
--     property (case S.parse (P.shortest (P.fromPure (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- longestFailBoth :: Property
-- longestFailBoth =
--     property
--         (case S.parse (P.shortest (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

many :: Property
many =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let fldstp conL currL = return $ FL.Partial (conL ++ currL)
            concatFold = FL.Fold fldstp (return (FL.Partial [])) return return
            prsr =
                flip P.many concatFold
                    $ P.fromFold $ FL.takeEndBy_ (== 1) FL.toList
         in case runIdentity $ S.parse prsr (S.fromList ls) of
                Right res_list ->
                    checkListEqual res_list (Prelude.filter (== 0) ls)
                Left _ -> property False

many_empty :: Property
many_empty =
    property (case runIdentity $ S.parse (flip P.many FL.toList (P.die "die")) (S.fromList [1 :: Int]) of
        Right res_list -> checkListEqual res_list ([] :: [Int])
        Left _ -> property False)

some :: Property
some =
    forAll (listOf (chooseInt (0, 1))) $ \genLs ->
        let
            ls = 0 : genLs
            fldstp conL currL = return $ FL.Partial $ conL ++ currL
            concatFold = FL.Fold fldstp (return (FL.Partial [])) return return
            prsr =
                flip P.some concatFold
                    $ P.fromFold $ FL.takeEndBy_ (== 1) FL.toList
         in case runIdentity $ S.parse prsr (S.fromList ls) of
                Right res_list -> res_list == Prelude.filter (== 0) ls
                Left _ -> False

someFail :: Property
someFail =
    property (case runIdentity $ S.parse (P.some (P.die "die") FL.toList) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

applicative :: Property
applicative =
    forAll (listOf (chooseAny :: Gen Int)) $ \ list1 ->
        forAll (listOf (chooseAny :: Gen Int)) $ \ list2 ->
            let parser =
                    (,)
                        <$> P.fromFold (FL.take (length list1) FL.toList)
                        <*> P.fromFold (FL.take (length list2) FL.toList)
             in
                case runIdentity $ S.parse parser (S.fromList $ list1 ++ list2) of
                    Right (olist1, olist2) -> olist1 == list1 && olist2 == list2
                    Left _ -> False

sequence :: Property
sequence =
    forAll (vectorOf 11 (listOf (chooseAny :: Gen Int))) $ \ ins ->
        let p xs = P.fromFold (FL.take (length xs) FL.toList)
         in monadicIO $ do
                outs <- run $
                        S.parse
                            (Prelude.sequence $ fmap p ins)
                            (S.fromList $ concat ins)
                return $
                    case outs of
                        Right ls -> ls == ins
                        Left _ -> False

altEOF1 :: Property
altEOF1 =
    monadicIO $ do
    s1 <- S.parse
        (P.satisfy (> 0) <|> return 66)
        (S.fromList ([]::[Int]))
    return $
        case s1 of
            Right x -> x == 66
            Left _ -> False

altEOF2 :: Property
altEOF2 =
    monadicIO $ do
    s1 <- S.parse
        ((P.takeEQ 2 FL.toList) <|> (P.takeEQ 1 FL.toList))
        (S.fromList ([51]::[Int]))
    return $
        case s1 of
            Right x -> x == [51]
            Left _ -> False

monad :: Property
monad =
    forAll (listOf (chooseAny :: Gen Int)) $ \ list1 ->
        forAll (listOf (chooseAny :: Gen Int)) $ \ list2 ->
            let parser = do
                    olist1 <- P.fromFold (FL.take (length list1) FL.toList)
                    olist2 <- P.fromFold (FL.take (length list2) FL.toList)
                    return (olist1, olist2)
             in monadicIO $ do
                    s <- S.parse parser (S.fromList $ list1 ++ list2)
                    return $
                        case s of
                            Right (olist1, olist2) -> olist1 == list1 && olist2 == list2
                            Left _ -> False

-------------------------------------------------------------------------------
-- Stream parsing
-------------------------------------------------------------------------------

parseMany :: Property
parseMany =
    forAll (chooseInt (1,100)) $ \len ->
        forAll (listOf (vectorOf len (chooseAny :: Gen Int))) $ \ ins ->
            monadicIO $ do
                outs <- do
                    let p = P.fromFold $ FL.take len FL.toList
                    run
                        $ S.fold FL.toList
                        $ S.catRights
                        $ S.parseMany p (S.fromList $ concat ins)
                listEquals (==) outs ins

-- basic sanity test for parsing from arrays
parseUnfold :: Property
parseUnfold = do
    let len = 200
    -- ls = input list (stream)
    -- clen = chunk size
    -- tlen = parser take size
    forAll
        ((,,)
            <$> vectorOf len (chooseAny :: Gen Int)
            <*> chooseInt (1, len)
            <*> chooseInt (1, len)) $ \(ls, clen, tlen) ->
        monadicIO $ do
            arrays <- S.toList $ S.chunksOf clen (S.fromList ls)
            let src = Producer.source (Just (Producer.OuterLoop arrays))
            let parser = P.fromFold (FL.take tlen FL.toList)
            let readSrc =
                    Producer.producer
                        $ Producer.concat Producer.fromList A.producer
            let streamParser =
                    Producer.simplify (Producer.parseManyD parser readSrc)
            xs <- run
                $ S.toList
                $ S.unfoldMany Unfold.fromList
                $ S.catRights
                $ S.unfold streamParser src

            listEquals (==) xs ls

parserSequence :: Property
parserSequence =
  forAll (vectorOf 11 (listOf (chooseAny :: Gen Int))) $ \ins ->
    monadicIO $ do
    let parsers = S.fromList
            $ fmap (\xs -> P.fromFold $ FL.take (length xs) FL.sum) ins
    let sequencedParser = P.sequence parsers FL.sum
    outs <-
        S.parse sequencedParser $ S.concatMap S.fromList (S.fromList ins)
    return $
        case outs of
            Right x -> x == sum (map sum ins)
            Left _ -> False

-------------------------------------------------------------------------------
-- Test for a particular case hit during fs events testing
-------------------------------------------------------------------------------

evId :: [Word8]
evId = [96,238,17,9,0,0,0,0]

evFlags :: [Word8]
evFlags = [0,4,1,0,0,0,0,0]

evPathLen :: [Word8]
evPathLen = [71,0,0,0,0,0,0,0]

evPath :: [Word8]
evPath =
    [47,85,115,101,114,115,47,118,111,108,47,118,101,109,98,97,47,99,111,109
    ,112,111,115,101,119,101 ,108,108,45,116,101,99,104,47,69,110,103,47,112
    ,114,111,106,101,99,116,115,47,115,116,114,101,97,109,108,121,47,115,116
    ,114,101,97,109,108,121,47,116,109,112,47,122,122
    ]

event :: [Word8]
event = evId ++ evFlags ++ evPathLen ++ evPath

data Event = Event
   { eventId :: Word64
   , eventFlags :: Word32
   , eventAbsPath :: A.Array Word8
   } deriving (Show, Ord, Eq)

readOneEvent :: P.Parser Word8 IO Event
readOneEvent = do
    arr <- P.takeEQ 24 (A.writeN 24)
    let arr1 = A.castUnsafe arr :: A.Array Word64
        eid = A.getIndexUnsafe 0 arr1
        eflags = A.getIndexUnsafe 1 arr1
        pathLen = fromIntegral $ A.getIndexUnsafe 2 arr1
    -- XXX handle if pathLen is 0
    path <- P.takeEQ pathLen (A.writeN pathLen)
    return $ Event
        { eventId = eid
        , eventFlags = fromIntegral eflags
        , eventAbsPath = path
        }

parseMany2Events :: Property
parseMany2Events =
    monadicIO $ do
        xs <-
            ( run
            $ S.fold FL.toList
            $ S.catRights
            $ S.parseMany readOneEvent
            $ S.fromList (concat (replicate 2 event))
            )
        assert (length xs == 2)
        -- XXX assuming little endian machine
        let ev = Event
                { eventId = 152170080
                , eventFlags = 66560
                , eventAbsPath = A.fromList evPath
                }
         in listEquals (==) xs (replicate 2 ev)

manyEqParseMany :: Property
manyEqParseMany =
    forAll (listOf (chooseInt (0, 100))) $ \lst ->
    forAll (chooseInt (1, 100)) $ \i ->
        monadicIO $ do
            let strm = S.fromList lst
            r1 <- run $ S.parse (P.many (split i) FL.toList) strm
            r2 <- run $ S.fold FL.toList $ S.catRights $ S.parseMany (split i) strm
            return $
                case r1 of
                    Right o1 -> o1 == r2
                    Left _ -> False

    where

    split i = P.fromFold (FL.take i FL.toList)


takeEndBy1 :: Property
takeEndBy1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case runIdentity $ S.parse (P.takeEndBy predicate prsr) (S.fromList ls) of
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

splitWithSuffix
    :: Monad m
    => (a -> Bool) -> FL.Fold m a b -> S.Stream m a -> S.Stream m b
splitWithSuffix predicate f =  S.foldMany (FL.takeEndBy predicate f)

takeEndBy2 :: Property
takeEndBy2 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let
            strm = S.fromList ls

            predicate = (==0)

            eitherParsedList =
                S.fold FL.toList
                    $ S.catRights
                    $ S.parseMany (P.takeEndBy predicate prsr) strm

                    where

                    prsr = P.many (P.satisfy (const True)) FL.toList

            eitherSplitList =
                case ls of
                    [] -> return []
                    _ ->
                        if last ls == 0
                        then S.fold FL.toList $ S.append strm1 (S.fromList [])
                        else S.fold FL.toList strm1

                        where

                        strm1 = splitWithSuffix predicate FL.toList strm
        in
            case eitherParsedList of
                Left _ -> property False
                Right parsedList ->
                    case eitherSplitList of
                        Left _ -> property False
                        Right splitList -> checkListEqual parsedList splitList

takeEndByEsc :: Property
takeEndByEsc =
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
            case runIdentity $ S.parse prsr (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list $ escapeSep Nothing ls
                Left err -> property (displayException err == msg)

takeFramedByEsc_ :: Property
takeFramedByEsc_ =
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
            case runIdentity $ S.parse prsr (S.fromList ls) of
                Right parsed_list ->
                    if checkPassBeg ls
                    then checkListEqual parsed_list $
                        escapeFrame isBegin isEnd isEsc ls
                    else property False
                Left _ ->
                    if checkPassBeg ls
                    then property False
                    else property True

takeFramedByEsc_Pass :: Property
takeFramedByEsc_Pass =
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
            case runIdentity $ S.parse prsr (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list $ escapeFrame isBegin isEnd isEsc ls
                _ -> property False

takeFramedByEsc_Fail1 :: Property
takeFramedByEsc_Fail1 =
    let
        msg = "takeFramedByEsc_: missing frame end"

        isBegin = (== 0)

        isEnd = (== 0)

        isEsc = (== 2)

        prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

        ls = [0 :: Int]
    in
        case runIdentity $ S.parse prsr (S.fromList ls) of
            Right _ -> property False
            Left err -> property (displayException err == msg)

takeFramedByEsc_Fail2 :: Property
takeFramedByEsc_Fail2 =
    let
        msg = "takeFramedByEsc_: missing frame start"

        isBegin = (== 0)

        isEnd = (== 1)

        isEsc = (== 1)

        prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

        ls = [1 :: Int]
    in
        case runIdentity $ S.parse prsr (S.fromList ls) of
            Right _ -> property False
            Left err -> property (displayException err == msg)

takeFramedByEsc_Fail3 :: Property
takeFramedByEsc_Fail3 =
    let
        msg = "takeFramedByEsc_: missing frame end"

        isBegin = (== 2)

        isEnd = (== 1)

        isEsc = (== 2)

        prsr = P.takeFramedByEsc_ isEsc isBegin isEnd FL.toList

        ls = [2 :: Int]
    in
        case runIdentity $ S.parse prsr (S.fromList ls) of
            Right _ -> property False
            Left err -> property $ (displayException err == msg)

takeStartBy_ :: Property
takeStartBy_ =
     forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let ls1 = 1:ls
            msg = "takeFramedByGeneric: empty token"
        in
            case runIdentity $ S.parse parser (S.fromList ls1) of
                Right parsed_list ->
                  if not $ Prelude.null ls1
                  then
                    let tls = Prelude.takeWhile (not . predicate) (tail ls1)
                    in checkListEqual parsed_list $
                      if predicate (head ls1)
                      then tls
                      else Prelude.takeWhile (not . predicate) ls1
                  else property $ Prelude.null parsed_list
                Left err -> property (displayException err == msg)
            where
                predicate = odd
                parser = P.takeStartBy_ predicate FL.toList
-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

main :: IO ()
main =
  hspec $
  H.parallel $
  modifyMaxSuccess (const maxTestCount) $ do
  describe moduleName $ do

    describe "Instances" $ do
        prop "applicative" applicative
        prop "Alternative: end of input 1" altEOF1
        prop "Alternative: end of input 2" altEOF2
        prop "monad" monad
        prop "sequence" sequence
    describe "Stream parsing" $ do
        prop "parseMany" parseMany
        prop "parseMany2Events" parseMany2Events
        prop "parseUnfold" parseUnfold
        prop "parserSequence" parserSequence

    describe "test for accumulator" $ do
        prop "P.fromFold FL.sum = FL.sum" fromFold
        prop "fromPure value provided" fromPure
        prop "fromPure monadic value provided" fromEffect
        prop "fail err = Left (SomeException (ParseError err))" parserFail
        prop "always fail" die
        prop "always fail but monadic" dieM

    describe "test for element parser" $ do
        prop "peek = head with list length > 0" peekPass
        prop "peek fail on []" peekFail
        prop "eof pass on []" eofPass
        prop "eof fail on non-empty list" eofFail
        prop "first element exists and >= mid_value" satisfyPass
        prop "one pass on [Int]" onePass
        prop "one fail on []" one
        prop "check first element exists and satisfies predicate" satisfy

    describe "test for sequence parser" $ do
        prop "P.takeBetween = Prelude.take when len >= m and len <= n"
            takeBetweenPass
        -- XXX This test fails
        -- XXX cabal run test:Data.Parser -- --match "/Data.Parser/test for sequence parser/P.takeBetween = Prelude.take when len >= m and len <= n and failotherwise fail/" --seed 1563586298
        -- prop ("P.takeBetween = Prelude.take when len >= m and len <= n and fail"
              -- ++ "otherwise fail") Main._takeBetween
        prop "P.take = Prelude.take" Main.take
        prop "P.takeEQ = Prelude.take when len >= n" takeEQPass
        prop "P.takeEQ = Prelude.take when len >= n and fail otherwise"
            Main.takeEQ
        prop "P.takeGE n ls = ls when len >= n" takeGEPass
        prop "P.takeGE n ls = ls when len >= n and fail otherwise" Main.takeGE
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" lookAheadPass
        -- prop "Fail when stream length exceeded" lookAheadFail
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" lookAhead
        prop ("P.takeStartBy pred = head : Prelude.takeWhile (not . pred)"
                ++ " tail") takeStartBy
        prop "P.takeWhile = Prelude.takeWhile" Main.takeWhile
        prop ("P.takeWhile1 = Prelude.takeWhile if taken something,"
                ++ " else check why failed") takeWhile1
        prop "takeWhileP prd P.take = takeWhileMaxLen prd" takeWhileP
        prop ("P.takeP = Prelude.take") takeP
        prop "P.groupBy = Prelude.head . Prelude.groupBy" groupBy
        prop "groupByRolling" groupByRolling
        prop "many (P.wordBy ' ') = words'" wordBy
        parseManyWordQuotedBy
        -- prop "choice" choice
        prop "parse 0, then 1, else fail" splitWith
        prop "fail due to die as left parser" splitWithFailLeft
        prop "fail due to die as right parser" splitWithFailRight
        prop "fail due to die as both parsers" splitWithFailBoth
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
                ++ "Prelude.filter (== 0)") many
        prop "[] due to parser being die" many_empty
        prop ("P.some concatFold $ P.takeEndBy_ (== 1) FL.toList ="
                ++ "Prelude.filter (== 0)") some
        prop "fail due to parser being die" someFail
        prop "P.many == S.parseMany" manyEqParseMany

        prop "takeEndBy_" takeEndBy_
        prop "takeEndByOrMax_" takeEndByOrMax_
        prop "takeEndBy1" takeEndBy1
        prop "takeEndBy2" takeEndBy2
        prop "takeEndByEsc" takeEndByEsc
        prop "takeFramedByEsc_" takeFramedByEsc_
        prop "takeFramedByEsc_Pass" takeFramedByEsc_Pass
        prop "takeFramedByEsc_Fail1" takeFramedByEsc_Fail1
        prop "takeFramedByEsc_Fail2" takeFramedByEsc_Fail2
        prop "takeFramedByEsc_Fail3" takeFramedByEsc_Fail3
        prop "takeStartBy_" takeStartBy_

    takeProperties
