{-# OPTIONS_GHC -Wno-deprecations #-}

module Streamly.Test.Unicode.Parser
    (main)
where

import Control.Monad.Identity (Identity(runIdentity))
import Debug.Trace (trace)
import Streamly.Internal.Data.Parser (ParseError (..), Parser)
import Streamly.Test.Common (chooseDouble)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
       (forAll, Property, property)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Scientific as Scientific
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream (parseBreak)
import qualified Streamly.Unicode.Parser as Unicode
import qualified Streamly.Internal.Unicode.Parser as Parser
import qualified Test.Hspec as H

-- Scientific notation
scientificExpFP :: Property
scientificExpFP =
    forAll (chooseDouble (-99.99e-12, 1234.4567e+234)) $ \ls ->
        case runIdentity $ Stream.parse parser (Stream.fromList (show ls)) of
                Right val -> val `H.shouldBe` read (show ls)
                Left _ -> error "Parsing failed."

        where

        toScientific (c, m) = Scientific.scientific c m
        parser = toScientific <$> Parser.number

-- Standard decimal notation.
scientificFixFP :: Property
scientificFixFP =
    forAll (chooseDouble (-0.00099, 123445.67998)) $ \ls ->
        case runIdentity $ Stream.parse parser (Stream.fromList (show ls)) of
                Right val -> val `H.shouldBe` read (show ls)
                Left _ -> error "Parsing failed."

        where

        toScientific (c, m) = Scientific.scientific c m
        parser = toScientific <$> Parser.number

doubleExpFP :: Property
doubleExpFP =
    forAll (chooseDouble (-99.99e-12, 1234.4567e+234)) $ \ls -> do
        let sls = show ls
        case runIdentity $ Stream.parse Parser.double (Stream.fromList sls) of
                Right val -> if val == ls
                             then property (val == ls)
                             else trace
                                    ("Read = "++ show (read sls :: Double) ++ " Expected = " ++ sls ++ " Got = " ++ show val)
                                    property (val == ls)
                Left _ -> property False

-- Standard decimal notation.
doubleFixFP :: Property
doubleFixFP =
    forAll (chooseDouble (-0.00099, 123445.67998)) $ \ls ->
        case runIdentity $ Stream.parse Parser.double (Stream.fromList (show ls)) of
                Right val -> if val == ls
                            then property (val == ls)
                            else trace
                                    ("Expected = " ++ show ls ++ " Got = " ++ show val)
                                    property (val == ls)
                Left _ -> property False

doubleParser :: String -> IO (Either ParseError Double)
doubleParser = Stream.parse Unicode.double . Stream.fromList

double :: String -> Double -> Property
double s d = monadicIO $ do
    x <- run $ doubleParser s
    case x of
        Right val -> if val == d
                     then assert (val == d)
                     else trace ("Expected = " ++ show d ++ " Got = "++ show val) (assert (val == d))
        Left (ParseError _) -> assert False

numberP :: Monad m => Parser Char m Double
numberP = uncurry Parser.mkDouble <$> Parser.number

numberParser :: String -> IO (Either ParseError Double)
numberParser = Stream.parse numberP . Stream.fromList

number :: String -> Double -> Property
number s d = monadicIO $ do
    x <- run $ numberParser s
    case x of
        Right val -> if val == d
                     then assert (val == d)
                     else trace ("Expected = " ++ show d ++ " Got = "++ show val) (assert (val == d))
        Left (ParseError _) -> assert False

doubleErr :: (String -> IO (Either ParseError Double)) -> String -> String -> Property
doubleErr f s msg = monadicIO $ do
    x <- run $ f s
    case x of
        Right _ -> assert False
        Left (ParseError err) -> if err == msg
                                then assert (err == msg)
                                else trace err (assert (err == msg))

remainingStreamDouble :: String -> [String]
remainingStreamDouble x =
    let f = Stream.parseBreak Unicode.double . Stream.fromList
        in concatMap (Stream.toList . snd) (f x)

remainingStreamNumber :: String -> [String]
remainingStreamNumber x =
    let f = Stream.parseBreak numberP . Stream.fromList
        in concatMap (Stream.toList . snd) (f x)

afterParse :: (String -> [String]) -> String -> String -> Property
afterParse f si so = monadicIO $ do
    let x = f si
        in assert (x == [so])

testParser :: String -> (String -> Double -> Property) -> H.SpecWith ()
testParser desc f = do
   -- XXX We can combine the parser tests and remaining stream tests in the
   -- same test.
    H.describe ("Unicode Parser " ++ desc) $ do
        prop "double \"00.00\"" $ f "00.00" 0.0
        prop "double \"00.00e00\"" $ f "00.00e00" 0.0
        prop "double \"4\" 4.0" $ f "4" 4.0
        prop "double \"00.3\" 0.3" $ f "00.3" 0.3
        prop "double \"0.003\" 3.0e-3" $ f "0.003" 3.0e-3
        prop "double \"44.56.67\" 44.56" $ f "44.56.67" 44.56
        prop "double \"0.0.00\" 0.0" $ f "0.0.00" 0.0
        prop "double \"44.56-78\" 44.56" $ f "44.56-78" 44.56
        prop "double \"44-\" 44.0" $ f "44-" 44.0
        prop "double \"44-56\" 44.0" $ f "44-56" 44.0
        prop "double \"44+\" 44.0" $ f "44+" 44.0
        prop "double \"44+56\" 44.0" $ f "44+56" 44.0
        prop "double \"44u\" 44.0" $ f "44u" 44.0
        prop "double \"+123.345\" 123.345" $ f "+123.345" 123.345
        prop "double \"-123.345\" (-123.345)" $ f "-123.345" (-123.345)
        prop "double \"1e\"" $ f "1e" 1.0
        prop "double \"1e+\"" $ f "1e+" 1.0
        prop "double \"1ex\"" $ f "1ex" 1.0
        prop "double \"1e+x\"" $ f "1e+x" 1.0
        prop "double \"1e+0\"" $ f "1e+0" 1.0
        prop "double \"1e-0\"" $ f "1e-0" 1.0
        prop "double \"1.0e-1\"" $ f "1.0e-1" 0.1
        prop "double \"1.0e+1\"" $ f "1.0e+1" 10.0
        prop "double \"1.0e1\"" $ f "1.0e1" 10.0
        prop "double \"1.0e+309\"" $ f "1.0e+309" 1.0e309
        prop "double \"1.0e-309\"" $ f "1.0e-309" 1.0e-309
        prop "double \"1.0e1.\"" $ f "1.0e1." 10.0
        prop "double \"1.0e1+\"" $ f "1.0e1+" 10.0
        prop "double \"1.0e1-\"" $ f "1.0e1-" 10.0
        prop "double \"1.0e1e\"" $ f "1.0e1e" 10.0
        prop "double \"1.0E+1\"" $ f "1.0E+1" 10.0
        prop "double \"9223372036854775806\"" $ f "9223372036854775806" 9.223372036854776e18
        -- maxBound :: Int
        prop "double \"9223372036854775807\"" $ f "9223372036854775807" 9.223372036854776e18
        prop "double \"9223372036854775808\"" $ f "9223372036854775808" 9.223372036854776e18

testParseErr :: String -> (String -> IO (Either ParseError Double)) -> H.SpecWith ()
testParseErr desc f = do
    H.describe ("Unicode Parser Error " ++ desc) $ do
        prop "double \"\" Error"
            $ doubleErr f "" "number: expecting sign or decimal digit, got end of input"
        prop "double \"a\" Error"
            $ doubleErr f "a" "number: expecting sign or decimal digit, got 'a'"
        prop "double \".4\" Error"
            $ doubleErr f ".4" "number: expecting sign or decimal digit, got '.'"
        prop "double \".\" Error"
            $ doubleErr f "." "number: expecting sign or decimal digit, got '.'"
        prop "double \"..\" Error"
            $ doubleErr f ".." "number: expecting sign or decimal digit, got '.'"
        prop "double \"-\" Error"
            $ doubleErr f "-" "number: expecting decimal digit, got end of input"
        prop "double \"+\" Error"
            $ doubleErr f "+" "number: expecting decimal digit, got end of input"
        prop "double \"++\" Error"
            $ doubleErr f "++" "number: expecting decimal digit, got '+'"

testLeftOver :: String -> (String -> [String]) -> H.SpecWith ()
testLeftOver desc f = do
    H.describe ("Unicode Parser leftover " ++ desc) $ do
        prop "afterParse \"4.\" \".\"" $ afterParse f "4." "."
        prop "afterParse \"4..\" \"..\"" $ afterParse f "4.." ".."
        prop "afterParse \"4-\" \"-\"" $ afterParse f "4-" "-"
        prop "afterParse \"4--\" \"--\"" $ afterParse f "4--" "--"
        prop "afterParse \"4.9abc\" \"abc\"" $ afterParse f "4.9abc" "abc"
        prop "afterParse \"4.9ex\"" $ afterParse f "4.9ex" "ex"
        prop "afterParse \"4.9e+x\"" $ afterParse f "4.9e+x" "e+x"
        prop "afterParse \"4.9\" \"\"" $ afterParse f "4.9" ""
        prop "afterParse \"+4.9\" \"\"" $ afterParse f "+4.9" ""
        prop "afterParse \"-4.9\" \"\"" $ afterParse f "-4.9" ""
        prop "afterParse \"4.9.\" \".\"" $ afterParse f "4.9." "."
        prop "afterParse \"4.9..\" \"..\"" $ afterParse f "4.9.." ".."
        prop "afterParse \"4.9...\" \"...\"" $ afterParse f "4.9..." "..."
        prop "afterParse \"4.9+\" \"+\"" $ afterParse f "4.9+" "+"
        prop "afterParse \"4.9++\" \"++\"" $ afterParse f "4.9++" "++"
        prop "afterParse \"4.9+++\" \"+++\"" $ afterParse f "4.9+++" "+++"
        prop "afterParse \"4.9-\" \"-\"" $ afterParse f "4.9-" "-"
        prop "afterParse \"4.9--\" \"--\"" $ afterParse f "4.9--" "--"
        prop "afterParse \"4.9---\" \"---\"" $ afterParse f "4.9---" "---"
        prop "afterParse \"\" \"\"" $ afterParse f "" ""
        prop "afterParse \".\" \".\"" $ afterParse f "." "."
        prop "afterParse \"..\" \"..\"" $ afterParse f ".." ".."
        prop "afterParse \"+\" \"+\"" $ afterParse f "+" "+"
        prop "afterParse \"++\" \"++\"" $ afterParse f "++" "++"

moduleName :: String
moduleName = "Unicode.Parser"

main :: IO ()
main = do
    H.hspec
  $ H.describe moduleName $ do
    testParser "double" double
    testParser "number" number
    testParseErr "double" doubleParser
    testParseErr "number" numberParser
    testLeftOver "double" remainingStreamDouble
    testLeftOver "number" remainingStreamNumber
    H.describe "Scientific parser property test" $ do
        prop "Exponent format" scientificExpFP
        prop "Decimal format" scientificFixFP
    H.describe "double parser property test" $ do
        prop "Exponent format" doubleExpFP
        prop "Decimal format" doubleFixFP
