{-# Language NoMonoLocalBinds #-}
-- | Common tests corresponding to the
-- @Streamly.Internal.Data.Parser.Type@ module (the parser type, its
-- instances and the primitive combinators defined alongside it).
module Streamly.Test.Data.Parser.CommonTypeTests (mainCommonType) where

import Control.Applicative ((<|>))
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Parser
       (ParseErrorPos(..), Parser(..), Step(..), Initial(..), Final(..))
import Streamly.Test.Common (chooseInt)
import Test.QuickCheck
       (forAll, Property, property, listOf, vectorOf, Gen)
import Test.QuickCheck.Monadic (monadicIO, run, PropertyM)

import Prelude hiding (sequence)

import qualified Control.Monad.Fail as Fail
import qualified Prelude
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P

import Test.Hspec
import Test.Hspec.QuickCheck

import Streamly.Test.Data.Parser.CommonTestDriver
    ( TestMode
    , ParserTestCase
    , ParserTestCase_Temp
    , runParserTC
    , runParserTC_temp
    , min_value
    , max_value
    )

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

-- | A 'takeWhile' that fails (instead of succeeding) when the predicate stops
-- holding. Used to exercise Alternative backtracking after a parser fails
-- mid-stream having already consumed input.
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
        else return $ SError "fail"

    extract s = fmap (FDone 0) (ffinal s)

-- Accumulator Tests

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

-- The left parser consumes 1..5 and then fails on 6 (mid-stream, with input
-- still available); the alternative backtracks and re-runs from the start,
-- consuming 1..7.
alt :: ParserTestCase_Temp Int (PropertyM IO) [Int] Property
alt producer consumer =
    monadicIO $ do
    s1 <- consumer
        (takeWhileFailD (<= 5) FL.toList <|> P.takeWhile (<= 7) FL.toList)
        (producer [1..20])
    return $
        case s1 of
            Right x -> x == [1..7]
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

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

{-# NOINLINE mainCommonType #-}
mainCommonType :: TestMode -> Spec
mainCommonType ptt = do
  -- This file has tests corresponding to Parser/Type.hs source file that are
  -- common to Parser and ParserK.
  describe (show ptt) $ do
    describe "Instances" $ do
        prop "applicative" $ runParserTC_temp ptt applicative
        prop "Alternative: end of input 1" $ runParserTC_temp ptt altEOF1
        prop "Alternative: end of input 2" $ runParserTC_temp ptt altEOF2
        prop "Alternative: backtrack after mid-stream failure" $ runParserTC_temp ptt alt
        prop "monad" $ runParserTC_temp ptt monad
        prop "sequence" $ runParserTC_temp ptt sequence

    describe "test for accumulator" $ do
        prop "fromPure value provided" $ runParserTC ptt fromPure
        prop "fromPure monadic value provided" $ runParserTC ptt fromEffect
        prop "fail err = Left (SomeException (ParseError err))" $ runParserTC ptt parserFail
        prop "always fail" $ runParserTC ptt die
        prop "always fail but monadic" $ runParserTC ptt dieM

    describe "test for sequence parser" $ do
        prop "parse 0, then 1, else fail" $ runParserTC_temp ptt splitWith
        prop "fail due to die as left parser" $ runParserTC_temp ptt splitWithFailLeft
        prop "fail due to die as right parser" $ runParserTC_temp ptt splitWithFailRight
        prop "fail due to die as both parsers" $ runParserTC_temp ptt splitWithFailBoth
