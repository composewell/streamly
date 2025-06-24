{-# Language NoMonoLocalBinds #-}
-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Word (Word8, Word32, Word64)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Parser (Parser(..), Step(..), Initial(..), Final(..))
import Streamly.Test.Common (listEquals, checkListEqual, chooseInt)
import Streamly.Internal.Data.Parser (ParseError(..))
import Test.QuickCheck (forAll, Property, property, listOf, vectorOf, Gen)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Prelude hiding (sequence)

import qualified Streamly.Test.Data.Parser.Common as Common
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A
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

#else

import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

#endif

maxTestCount :: Int
maxTestCount = 100

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

--------------------------------------------------------------------------------
-- Parser sequence tests
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Parser quoted word tests
--------------------------------------------------------------------------------

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
-- Instances
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

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

        Common.main
