{-# Language NoMonoLocalBinds #-}
-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (displayException, try, evaluate, SomeException)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Word (Word8, Word32, Word64)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Parser (Parser(..), Step(..), Initial(..), Final(..))
import Streamly.Test.Common (listEquals, checkListEqual, chooseInt)
import Streamly.Internal.Data.Parser (ParseError(..))
import Test.QuickCheck
       (arbitrary, forAll, elements, Property, property, listOf,
        vectorOf, Gen, (.&&.), ioProperty)
import Test.QuickCheck.Monadic (monadicIO, assert, run, PropertyM)

import Prelude hiding (sequence)

import qualified Control.Monad.Fail as Fail
import qualified Data.List as List
import qualified Prelude
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Array.Generic as GA
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.ParserK as PK
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Stream as SI
import qualified Streamly.Internal.Data.StreamK as K
import qualified Test.Hspec as H

import Test.Hspec
import Test.Hspec.QuickCheck
import Streamly.Test.Parser.Common

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

type ParserTestCase a m b c =
        forall t. ([a] -> t)
        -> (P.Parser a m b -> t -> m (Either ParseError b))
        -> c

fromFold :: ParserTestCase Int IO Int Property
fromFold producer consumer =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        ioProperty $ do
        s1 <- consumer (P.fromFold FL.sum) (producer ls)
        o2 <- S.fold FL.sum (S.fromList ls)
        return $
            case s1 of
                Right o1 -> o1 == o2
                Left _ -> False

fromPure :: ParserTestCase Int Identity Int Property
fromPure producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case runIdentity $ consumer (P.fromPure x) (producer [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

fromEffect :: ParserTestCase Int Identity Int Property
fromEffect producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case runIdentity $ consumer (P.fromEffect $ return x) (producer [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

die :: ParserTestCase Int Identity Int Property
die producer consumer =
    property $
    case runIdentity $ consumer (P.die "die test") (producer [0 :: Int]) of
        Right _ -> False
        Left _ -> True

dieM :: ParserTestCase Int Identity Int Property
dieM producer consumer =
    property $
    case runIdentity $ consumer (P.dieM (Identity "die test")) (producer [0 :: Int]) of
        Right _ -> False
        Left _ -> True

parserFail :: ParserTestCase Int Identity Int Property
parserFail producer consumer =
    property $
        case runIdentity $ consumer (Fail.fail err) (producer [0 :: Int]) of
            Right _ -> False
            Left (ParseError e) -> err == e
    where
    err = "Testing MonadFail.fail."

-- Element Parser Tests

peekPass :: ParserTestCase Int Identity Int Property
peekPass producer consumer =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ consumer P.peek (producer ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False

peekFail :: ParserTestCase Int Identity Int Property
peekFail producer consumer =
    property (case runIdentity $ consumer P.peek (producer []) of
        Right _ -> False
        Left _ -> True)

eofPass :: ParserTestCase Int Identity () Property
eofPass producer consumer =
    property (case runIdentity $ consumer P.eof (producer []) of
        Right _ -> True
        Left _ -> False)

eofFail :: ParserTestCase Int Identity () Property
eofFail producer consumer =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ consumer P.eof (producer ls) of
                Right _ -> False
                Left _ -> True

satisfyPass :: ParserTestCase Int Identity Int Property
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

satisfy :: ParserTestCase Int Identity Int Property
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

onePass :: ParserTestCase Int Identity Int Property
onePass producer consumer =
    forAll (chooseInt (1, max_value)) $ \int ->
        property (case runIdentity $ consumer P.one (producer [int]) of
            Right i -> i  == int
            Left _ -> False)

one :: ParserTestCase Int Identity Int Property
one producer consumer =
    property $
        case runIdentity $ consumer P.one (producer []) of
            Left _ -> True
            Right _ -> False

-- Sequence Parsers Tests
takeBetweenPass :: ParserTestCase Int Identity [Int] Property
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

takeBetween :: ParserTestCase Int Identity [Int] Property
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


take :: ParserTestCase Int Identity [Int] Property
take producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            case runIdentity $ consumer (P.fromFold $ FL.take n FL.toList) (producer ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                Left _ -> property False

takeEQPass :: ParserTestCase Int Identity [Int] Property
takeEQPass producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length
                        (chooseInt (min_value, max_value))) $ \ls ->
                case runIdentity $ consumer (P.takeEQ n FL.toList) (producer ls) of
                    Right parsed_list ->
                        checkListEqual parsed_list (Prelude.take n ls)
                    Left _ -> property False

takeEQ :: ParserTestCase Int Identity [Int] Property
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

takeGEPass :: ParserTestCase Int Identity [Int] Property
takeGEPass producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                $ \ls ->
                    case runIdentity $ consumer (P.takeGE n FL.toList) (producer ls) of
                        Right parsed_list -> checkListEqual parsed_list ls
                        Left _ -> property False

takeGE :: ParserTestCase Int Identity [Int] Property
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
    -> ParserTestCase Int Identity [Int] Property
nLessThanEqual0 tk ltk producer consumer =
    forAll (elements [0, (-1)]) $ \n ->
        forAll (listOf arbitrary) $ \ls ->
            case runIdentity $ consumer (tk n FL.toList) (producer ls) of
                Right parsed_list -> checkListEqual parsed_list (ltk n ls)
                Left _ -> property False

takeProperties :: ParserTestCase Int Identity [Int] Spec
takeProperties producer consumer =
    describe "take combinators when n <= 0/" $ do
        prop "takeEQ n FL.toList = []" $
            nLessThanEqual0 P.takeEQ (\_ -> const []) producer consumer
        prop "takeGE n FL.toList xs = xs" $
            nLessThanEqual0 P.takeGE (\_ -> id) producer consumer

-- XXX lookAhead can't deal with EOF which in this case means when
-- n==list_length, this test will fail. So excluding that case for now.
lookAheadPass :: ParserTestCase Int Identity ([Int], [Int]) Property
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

-- lookAheadFail :: ParserTestCase Int Identity Int Property
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

lookAhead :: ParserTestCase Int Identity ([Int], [Int]) Property
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

takeEndBy_ :: ParserTestCase Int Identity [Int] Property
takeEndBy_ producer consumer =
    forAll (listOf (chooseInt (min_value, max_value )))  $ \ls ->
        case runIdentity $ consumer (P.takeEndBy_ predicate prsr) (producer ls) of
            Right parsed_list ->
                checkListEqual parsed_list (tkwhl ls)
            Left _ -> property False
        where
            predicate = (>= 100)
            prsr = P.many (P.satisfy (const True)) FL.toList
            tkwhl ls = Prelude.takeWhile (not . predicate) ls

takeEndByOrMax_ :: ParserTestCase Int Identity [Int] Property
takeEndByOrMax_ producer consumer =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            case runIdentity $ consumer (P.fromFold $ FL.takeEndBy_ predicate (FL.take n FL.toList)) (producer ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n (Prelude.takeWhile (not . predicate) ls))
                Left _ -> property False
            where
                predicate = (== 1)

takeStartBy :: ParserTestCase Int Identity [Int] Property
takeStartBy producer consumer =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let ls1 = 1:ls
        in
            case runIdentity $ consumer parser (producer ls1) of
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
                parser = P.takeBeginBy predicate FL.toList

takeWhile :: ParserTestCase Int Identity [Int] Property
takeWhile producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case runIdentity $ consumer (P.takeWhile predicate FL.toList) (producer ls) of
            Right parsed_list ->
                checkListEqual parsed_list (Prelude.takeWhile predicate ls)
            Left _ -> property False
        where
            predicate = (== 0)

takeP :: ParserTestCase Int Identity [Int] Property
takeP producer consumer =
    forAll
        ((,) <$> chooseInt (min_value, max_value)
             <*> listOf (chooseInt (0, 1)))
        $ \(takeNum, ls) ->
              case runIdentity $ consumer
                       (P.takeP takeNum (P.fromFold FL.toList))
                       (producer ls) of
                  Right parsed_list ->
                      checkListEqual parsed_list (Prelude.take takeNum ls)
                  Left _ -> property False

takeWhile1 :: ParserTestCase Int Identity [Int] Property
takeWhile1 producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case runIdentity $ consumer (P.takeWhile1 predicate  FL.toList) (producer ls) of
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

takeWhileP :: ParserTestCase Int Identity [Int] Property
takeWhileP producer consumer =
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
                case runIdentity $ consumer prsr (producer ls) of
                    Right parsed_list ->
                        checkListEqual
                        parsed_list
                        (takeWhileTillLen n predicate ls)
                    Left _ -> property False

{-
choice :: ParserTestCase Int Identity Int Property
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

groupBy :: ParserTestCase Int Identity [Int] Property
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

groupByRolling :: ParserTestCase Int Identity [Int] Property
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

wordBy :: ParserTestCase Char Identity [String] Property
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

splitWith :: ParserTestCase Int Identity (Int, Int) Property
splitWith producer consumer =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case runIdentity $ consumer (P.splitWith (,) (P.satisfy (== 0)) (P.satisfy (== 1))) (producer ls) of
            Right (result_first, result_second) -> case ls of
                0 : 1 : _ -> (result_first == 0) && (result_second == 1)
                _ -> False
            Left _ -> case ls of
                0 : 1 : _ -> False
                _ -> True

splitWithFailLeft :: ParserTestCase Int Identity (Int, Int) Property
splitWithFailLeft producer consumer =
    property (case runIdentity $ consumer (P.splitWith (,) (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailRight :: ParserTestCase Int Identity (Int, Int) Property
splitWithFailRight producer consumer =
    property (case runIdentity $ consumer (P.splitWith (,) (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailBoth :: ParserTestCase Int Identity (Int, Int) Property
splitWithFailBoth producer consumer =
    property (case runIdentity $ consumer (P.splitWith (,) (P.die "die") (P.die "die")) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

-- teeWithPass :: ParserTestCase Int Identity Int Property
-- teeWithPass producer consumer =
--     forAll (chooseInt (min_value, max_value)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             let
--                 prsr = P.fromFold $ FL.take n FL.toList
--             in
--                 case consumer (P.teeWith (,) prsr prsr) (producer ls) of
--                     Right (ls_1, ls_2) -> checkListEqual (Prelude.take n ls) ls_1 .&&. checkListEqual ls_1 ls_2
--                     Left _ -> property False

-- teeWithFailLeft :: ParserTestCase Int Identity Int Property
-- teeWithFailLeft producer consumer =
--     property (case consumer (P.teeWith (,) (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailRight :: ParserTestCase Int Identity Int Property
-- teeWithFailRight producer consumer =
--     property (case consumer (P.teeWith (,) (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailBoth :: ParserTestCase Int Identity Int Property
-- teeWithFailBoth producer consumer =
--     property (case consumer (P.teeWith (,) (P.die "die") (P.die "die")) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

{-
deintercalate :: ParserTestCase Int Identity Int Property
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

-- shortestPass :: ParserTestCase Int Identity Int Property
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

-- shortestPassLeft :: ParserTestCase Int Identity Int Property
-- shortestPassLeft producer consumer =
--     property (case consumer (P.shortest (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- shortestPassRight :: ParserTestCase Int Identity Int Property
-- shortestPassRight producer consumer =
--     property (case consumer (P.shortest (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailBoth :: ParserTestCase Int Identity Int Property
-- shortestFailBoth producer consumer =
--     property
--         (case consumer
--                   (P.shortest (P.die "die") (P.die "die"))
--                   (producer [1 :: Int]) of
--              Right _ -> False
--              Left _ -> True)
--
-- longestPass :: ParserTestCase Int Identity Int Property
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
-- longestPassLeft :: ParserTestCase Int Identity Int Property
-- longestPassLeft producer consumer =
--     property (case consumer (P.shortest (P.die "die") (P.fromPure (1 :: Int))) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- longestPassRight :: ParserTestCase Int Identity Int Property
-- longestPassRight producer consumer =
--     property (case consumer (P.shortest (P.fromPure (1 :: Int)) (P.die "die")) (producer [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)
--
-- longestFailBoth :: ParserTestCase Int Identity Int Property
-- longestFailBoth producer consumer =
--     property
--         (case consumer (P.shortest (P.die "die") (P.die "die")) (producer [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

many :: ParserTestCase Int Identity [Int] Property
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

many_empty :: ParserTestCase Int Identity [Int] Property
many_empty producer consumer =
    property (case runIdentity $ consumer (flip P.many FL.toList (P.die "die")) (producer [1 :: Int]) of
        Right res_list -> checkListEqual res_list ([] :: [Int])
        Left _ -> property False)

some :: ParserTestCase Int Identity [Int] Property
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

someFail :: ParserTestCase Int Identity [Int] Property
someFail producer consumer =
    property (case runIdentity $ consumer (P.some (P.die "die") FL.toList) (producer [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

applicative :: ParserTestCase Int Identity ([Int], [Int]) Property
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

sequence :: ParserTestCase Int IO [[Int]] Property
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

altEOF1 :: ParserTestCase Int (PropertyM IO) Int Property
altEOF1 producer consumer =
    monadicIO $ do
    s1 <- consumer
        (P.satisfy (> 0) <|> return 66)
        (producer ([]::[Int]))
    return $
        case s1 of
            Right x -> x == 66
            Left _ -> False

altEOF2 :: ParserTestCase Int (PropertyM IO) [Int] Property
altEOF2 producer consumer =
    monadicIO $ do
    s1 <- consumer
        ((P.takeEQ 2 FL.toList) <|> (P.takeEQ 1 FL.toList))
        (producer ([51]::[Int]))
    return $
        case s1 of
            Right x -> x == [51]
            Left _ -> False

{-# INLINE takeWhileFailD #-}
takeWhileFailD :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeWhileFailD predicate (Fold fstep finitial _ ffinal) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial s
            FL.Done b -> IDone b

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      FL.Partial s1 -> SContinue 1 s1
                      FL.Done b -> SDone 1 b
        else return $ Error "fail"

    extract s = fmap (FDone 0) (ffinal s)

{-# INLINE takeWhileFail #-}
takeWhileFail :: MonadIO m =>
    (a -> Bool) -> Fold m a b -> PK.ParserK a m b
takeWhileFail p f = PK.parserK (takeWhileFailD p f)

{-# INLINE takeWhileK #-}
takeWhileK :: MonadIO m => (a -> Bool) -> PK.ParserK a m [a]
takeWhileK p = PK.parserK $ P.takeWhile p FL.toList

{-# INLINE alt2 #-}
alt2 :: MonadIO m => K.StreamK m Int -> m (Either ParseError [Int])
alt2 =
    K.parse
        (   takeWhileFail (<= 5) FL.toList
        <|> takeWhileK (<= 7)
        )

{-# INLINE altD #-}
altD :: MonadIO m => S.Stream m Int -> m (Either P.ParseError [Int])
altD =
    S.parse
        (   takeWhileFailD (<= 5) FL.toList
        <|> P.takeWhile (<= 7) FL.toList
        )

monad :: ParserTestCase Int (PropertyM IO) ([Int], [Int]) Property
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

-------------------------------------------------------------------------------
-- Stream parsing
-------------------------------------------------------------------------------

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
            arrays <- S.toList $ A.chunksOf clen (S.fromList ls)
            let src = Producer.source (Just (Producer.OuterLoop arrays))
            let parser = P.fromFold (FL.take tlen FL.toList)
            let readSrc =
                    Producer.producer
                        $ Producer.concat Producer.fromList A.producer
            let streamParser =
                    Producer.simplify (Producer.parseManyD parser readSrc)
            xs <- run
                $ S.toList
                $ S.unfoldEach Unfold.fromList
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
    arr <- P.takeEQ 24 (A.createOf 24)
    let arr1 = A.unsafeCast arr :: A.Array Word64
        eid = A.unsafeGetIndex 0 arr1
        eflags = A.unsafeGetIndex 1 arr1
        pathLen = fromIntegral $ A.unsafeGetIndex 2 arr1
    -- XXX handle if pathLen is 0
    path <- P.takeEQ pathLen (A.createOf pathLen)
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


takeEndBy1 :: ParserTestCase Int Identity [Int] Property
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

takeEndByEsc :: ParserTestCase Int Identity [Int] Property
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
                Left err -> property (displayException err == msg)

takeFramedByEsc_ :: ParserTestCase Int Identity [Int] Property
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

takeFramedByEsc_Pass :: ParserTestCase Int Identity [Int] Property
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

takeFramedByEsc_Fail1 :: ParserTestCase Int Identity [Int] Property
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
            Left err -> property (displayException err == msg)

takeFramedByEsc_Fail2 :: ParserTestCase Int Identity [Int] Property
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
            Left err -> property (displayException err == msg)

takeFramedByEsc_Fail3 :: ParserTestCase Int Identity [Int] Property
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
            Left err -> property $ (displayException err == msg)

takeStartBy_ :: ParserTestCase Int Identity [Int] Property
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
                Left err -> property (displayException err == msg)
            where
                predicate = odd
                parser = P.takeBeginBy_ predicate FL.toList

quotedWordTest :: String -> [String] -> IO ()
quotedWordTest inp expected = do
    res <-
        S.fold FL.toList
            $ catRightsErr
            $ S.parseMany quotedWord $ S.fromList inp
    res `shouldBe` expected
    where
    catRightsErr = fmap (either (error . displayException) id)
    quotedWord =
        let toRQuote x =
                case x of
                    '"' -> Just x
                    '\'' -> Just x
                    _ -> Nothing
            -- Inside ",
            -- * \\ is translated to \
            -- * \" is translated to "
            trEsc '"' x =
                case x of
                    '\\' -> Just '\\'
                    '"' -> Just '"'
                    _ -> Nothing
            trEsc _ _ = Nothing
         in P.wordWithQuotes False trEsc '\\' toRQuote isSpace FL.toList

--------------------------------------------------------------------------------
-- Parser sanity tests
--------------------------------------------------------------------------------

{-
TODO:
Add sanity tests for
- Producer.parse
- Producer.parseMany
- Stream.parseMany
- Stream.parseIterate
-}

sanityParseBreak :: [Move] -> SpecWith ()
sanityParseBreak jumps = it (show jumps) $ do
    (val, rest) <- SI.parseBreak (jumpParser jumps) $ S.fromList tape
    lst <- S.toList rest
    (val, lst) `shouldBe` (expectedResult jumps tape)

sanityParseDBreak :: [Move] -> SpecWith ()
sanityParseDBreak jumps = it (show jumps) $ do
    (val, rest) <- K.parseDBreak (jumpParser jumps) $ K.fromList tape
    lst <- K.toList rest
    (val, lst) `shouldBe` (expectedResult jumps tape)

{-
sanityParseBreakChunksK :: [Move] -> SpecWith ()
sanityParseBreakChunksK jumps = it (show jumps) $ do
    (val, rest) <-
        A.parseBreakChunksK (jumpParser jumps)
            $ K.fromList $ Prelude.map A.fromList chunkedTape
    lst <- Prelude.map A.toList <$> K.toList rest
    (val, concat lst) `shouldBe` (expectedResult jumps tape)
-}

sanityParseMany :: [Move] -> SpecWith ()
sanityParseMany jumps = it (show jumps) $ do
    res <- S.toList $ SI.parseMany (jumpParser jumps) $ S.fromList tape
    res `shouldBe` (expectedResultMany jumps tape)

sanityParseIterate :: [Move] -> SpecWith ()
sanityParseIterate jumps = it (show jumps) $ do
    res <-
        S.toList
             $ SI.parseIterate (const (jumpParser jumps)) [] $ S.fromList tape
    res `shouldBe` (expectedResultMany jumps tape)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

data TestMode
    = TMParserStream
    | TMParserKStreamK
    | TMParserKStreamKChunks
    | TMParserKStreamKChunksGeneric
    deriving (Show)

runParserTC :: (Unbox a, Monad m) => TestMode -> ParserTestCase a m b c -> c
runParserTC tm runner =
    case tm of
        TMParserStream -> runner S.fromList S.parse
        TMParserKStreamK -> runner K.fromList (K.parse . PK.parserK)
        TMParserKStreamKChunks ->
            runner (producerChunks A.fromList) (A.parse . A.parserK)
        TMParserKStreamKChunksGeneric ->
            runner
                (producerChunks GA.fromList)
                (GA.parse . GA.parserK)

    where
    cSize = 50
    -- Not using A.createOf here because of the MonadIO constraint
    producerChunks fl =
        K.fromStream
             . S.groupsOf cSize (fl <$> FL.toList)
             . S.fromList

mainCommon :: TestMode -> Spec
mainCommon ptt = do
  describe (show ptt) $ do
    describe "Instances" $ do
        prop "applicative" $ runParserTC ptt applicative
        prop "Alternative: end of input 1" $ runParserTC ptt altEOF1
        prop "Alternative: end of input 2" $ runParserTC ptt altEOF2
        prop "monad" $ runParserTC ptt monad
        prop "sequence" $ runParserTC ptt sequence

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
        prop "eof pass on []" $ runParserTC ptt eofPass
        prop "eof fail on non-empty list" $ runParserTC ptt eofFail
        prop "first element exists and >= mid_value" $ runParserTC ptt satisfyPass
        prop "one pass on [Int]" $ runParserTC ptt onePass
        prop "one fail on []" $ runParserTC ptt one
        prop "check first element exists and satisfies predicate" $ runParserTC ptt satisfy
    describe "test for sequence parser" $ do
        prop "P.takeBetween = Prelude.take when len >= m and len <= n"
            $ runParserTC ptt takeBetweenPass
        prop ("P.takeBetween = Prelude.take when len >= m and len <= n and fail"
              ++ "otherwise fail") $ runParserTC ptt takeBetween
        prop "P.take = Prelude.take" $ runParserTC ptt Main.take
        prop "P.takeEQ = Prelude.take when len >= n" $ runParserTC ptt takeEQPass
        prop "P.takeEQ = Prelude.take when len >= n and fail otherwise"
            $ runParserTC ptt Main.takeEQ
        prop "P.takeGE n ls = ls when len >= n" $ runParserTC ptt takeGEPass
        prop "P.takeGE n ls = ls when len >= n and fail otherwise" $ runParserTC ptt Main.takeGE
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" $ runParserTC ptt lookAheadPass
        -- prop "Fail when stream length exceeded" lookAheadFail
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" $ runParserTC ptt lookAhead
        prop ("P.takeStartBy pred = head : Prelude.takeWhile (not . pred)"
                ++ " tail") $ runParserTC ptt takeStartBy
        prop "P.takeWhile = Prelude.takeWhile" $ runParserTC ptt Main.takeWhile
        prop ("P.takeWhile1 = Prelude.takeWhile if taken something,"
                ++ " else check why failed") $ runParserTC ptt takeWhile1
        prop "takeWhileP prd P.take = takeWhileMaxLen prd" $ runParserTC ptt takeWhileP
        prop ("P.takeP = Prelude.take") $ runParserTC ptt takeP
        prop "P.groupBy = Prelude.head . Prelude.groupBy" $ runParserTC ptt groupBy
        prop "groupByRolling" $ runParserTC ptt groupByRolling
        prop "many (P.wordBy ' ') = words'" $ runParserTC ptt wordBy
        -- prop "choice" choice
        prop "parse 0, then 1, else fail" $ runParserTC ptt splitWith
        prop "fail due to die as left parser" $ runParserTC ptt splitWithFailLeft
        prop "fail due to die as right parser" $ runParserTC ptt splitWithFailRight
        prop "fail due to die as both parsers" $ runParserTC ptt splitWithFailBoth
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
                ++ "Prelude.filter (== 0)") $ runParserTC ptt many
        prop "[] due to parser being die" $ runParserTC ptt many_empty
        prop ("P.some concatFold $ P.takeEndBy_ (== 1) FL.toList ="
                ++ "Prelude.filter (== 0)") $ runParserTC ptt some
        prop "fail due to parser being die" $ runParserTC ptt someFail
        prop "takeEndBy_" $ runParserTC ptt takeEndBy_
        prop "takeEndByOrMax_" $ runParserTC ptt takeEndByOrMax_
        prop "takeEndBy1" $ runParserTC ptt takeEndBy1
        prop "takeEndByEsc" $ runParserTC ptt takeEndByEsc
        prop "takeFramedByEsc_" $ runParserTC ptt takeFramedByEsc_
        prop "takeFramedByEsc_Pass" $ runParserTC ptt takeFramedByEsc_Pass
        prop "takeFramedByEsc_Fail1" $ runParserTC ptt takeFramedByEsc_Fail1
        prop "takeFramedByEsc_Fail2" $ runParserTC ptt takeFramedByEsc_Fail2
        prop "takeFramedByEsc_Fail3" $ runParserTC ptt takeFramedByEsc_Fail3
        prop "takeStartBy_" $ runParserTC ptt takeStartBy_

    runParserTC ptt takeProperties

main :: IO ()
main = do
  -- TODO: convert this test to the same format as other tests.
  r <- alt2 (K.fromList [1..20])
  case r of
    Right x | x == [1..7] -> putStrLn "K.Alt parse successful"
    Right x -> error $ "K.Alt parse got incorrect output " ++ show x
    _ -> error $ "K.Alt parse failed"

  r1 <- altD (S.fromList [1..20])
  case r1 of
    Right x | x == [1..7] -> putStrLn "Alt parse successful"
    Right x -> error $ "Alt parse got incorrect output " ++ show x
    _ -> error $ "Alt parse failed"

  hspec $
      H.parallel $
      modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        parserSanityTests "Stream.parseBreak" sanityParseBreak
        parserSanityTests "StreamK.parseDBreak" sanityParseDBreak
        -- parserSanityTests "A.sanityParseBreakChunksK" sanityParseBreakChunksK
        parserSanityTests "Stream.parseMany" sanityParseMany
        parserSanityTests "Stream.parseIterate" sanityParseIterate
        describe "Stream parsing" $ do
            prop "parseMany" parseMany
            prop "parseMany2Events" parseMany2Events
            prop "parseUnfold" parseUnfold
            prop "parserSequence" parserSequence

        describe "test for sequence parser" $ do
            parseManyWordQuotedBy
            prop "P.many == S.parseMany" manyEqParseMany
            prop "takeEndBy2" takeEndBy2

        describe "quotedWordTest" $ do
            it "Single quote test" $ do
               quotedWordTest "'hello\\\\\"world'" ["hello\\\\\"world"]
               quotedWordTest "'hello\\'" ["hello\\"]
            it "Double quote test" $ do
               quotedWordTest
                   "\"hello\\\"\\\\w\\'orld\""
                   ["hello\"\\w\\'orld"]

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
