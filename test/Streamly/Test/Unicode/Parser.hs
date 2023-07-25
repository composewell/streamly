{-# OPTIONS_GHC -Wno-deprecations #-}

module Streamly.Test.Unicode.Parser
    (main)
where

import Control.Monad.Identity (Identity(runIdentity))
import Debug.Trace (trace)
import Streamly.Internal.Data.Parser (ParseError (..))
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
                Right val -> if val == show ls
                             then property (val == show ls)
                             else trace
                                    ("Expected = " ++ show ls ++ " Got = " ++ val)
                                    property (val == show ls)
                Left _ -> property False

        where

        formatter = Scientific.formatScientific Scientific.Exponent Nothing
        toScientific (c, m) = Scientific.scientific c m
        parser = formatter . toScientific <$> Parser.number

-- Standard decimal notation.
scientificFixFP :: Property
scientificFixFP =
    forAll (chooseDouble (-0.00099, 123445.67998)) $ \ls ->
        case runIdentity $ Stream.parse parser (Stream.fromList (show ls)) of
                Right val -> if val == show ls
                             then property (val == show ls)
                             else trace
                                    ("Expected = " ++ show ls ++ " Got = " ++ val)
                                    property (val == show ls)
                Left _ -> property False

        where

        formatter = Scientific.formatScientific Scientific.Fixed Nothing
        toScientific (c, m) = Scientific.scientific c m
        parser = formatter . toScientific <$> Parser.number

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
                     else trace ("Expectedc = " ++ show d ++ " Got = "++ show val) (assert (val == d))
        Left (ParseError _) -> assert False

doubleErr :: String -> String -> Property
doubleErr s msg = monadicIO $ do
    x <- run $ doubleParser s
    case x of
        Right _ -> assert False
        Left (ParseError err) -> if err == msg
                                then assert (err == msg)
                                else trace err (assert (err == msg))

remainingStream :: String -> [String]
remainingStream x =
    let f = Stream.parseBreak Unicode.double . Stream.fromList
        in concatMap (Stream.toList . snd) (f x)

afterParse :: String -> String -> Property
afterParse si so = monadicIO $ do
    let x = remainingStream si
        in assert (x == [so])

moduleName :: String
moduleName = "Unicode.Parser"

main :: IO ()
main = do
    H.hspec
  $ H.describe moduleName $ do
    H.describe "Unicode Parser" $ do
        prop "double \"4\" 4.0" $ double "4" 4.0
        prop "double \"00.3\" 0.3" $ double "00.3" 0.3
        prop "double \"0.003\" 3.0e-3" $ double "0.003" 3.0e-3
        prop "double \"44.56.67\" 44.56" $ double "44.56.67" 44.56
        prop "double \"0.0.00\" 0.0" $ double "0.0.00" 0.0
        prop "double \"44.56-78\" 44.56" $ double "44.56-78" 44.56
        prop "double \"44-\" 44.0" $ double "44-" 44.0
        prop "double \"44-56\" 44.0" $ double "44-56" 44.0
        prop "double \"44+\" 44.0" $ double "44+" 44.0
        prop "double \"44+56\" 44.0" $ double "44+56" 44.0
        prop "double \"44u\" 44.0" $ double "44u" 44.0
        prop "double \"+123.345\" 123.345" $ double "+123.345" 123.345
        prop "double \"-123.345\" (-123.345)" $ double "-123.345" (-123.345)
        prop "double \"1e\"" $ double "1e" 1.0
        prop "double \"1e+\"" $ double "1e+" 1.0
        prop "double \"1e+0\"" $ double "1e+0" 1.0
        prop "double \"1e-0\"" $ double "1e-0" 1.0
        prop "double \"1.0e-1\"" $ double "1.0e-1" 0.1
        prop "double \"1.0e+1\"" $ double "1.0e+1" 10.0
        prop "double \"1.0e1\"" $ double "1.0e1" 10.0
        prop "double \"1.0e+309\"" $ double "1.0e+309" 1.0e309
        prop "double \"1.0e-309\"" $ double "1.0e-309" 1.0e-309
        prop "double \"1.0e1.\"" $ double "1.0e1." 10.0
        prop "double \"1.0e1+\"" $ double "1.0e1+" 10.0
        prop "double \"1.0e1-\"" $ double "1.0e1-" 10.0
        prop "double \"1.0e1e\"" $ double "1.0e1e" 10.0
        prop "double \"1.0E+1\"" $ double "1.0E+1" 10.0

        prop "double \"\" Error"
            $ doubleErr "" "number: expecting sign or decimal digit, got end of input"
        prop "double \"a\" Error"
            $ doubleErr "a" "number: expecting sign or decimal digit, got 'a'"
        prop "double \".4\" Error"
            $ doubleErr ".4" "number: expecting sign or decimal digit, got '.'"
        prop "double \".\" Error"
            $ doubleErr "." "number: expecting sign or decimal digit, got '.'"
        prop "double \"..\" Error"
            $ doubleErr ".." "number: expecting sign or decimal digit, got '.'"
        prop "double \"-\" Error"
            $ doubleErr "-" "number: expecting decimal digit, got end of input"
        prop "double \"+\" Error"
            $ doubleErr "+" "number: expecting decimal digit, got end of input"
        prop "double \"++\" Error"
            $ doubleErr "++" "number: expecting decimal digit, got '+'"

        prop "afterParse \"4.\" \".\"" $ afterParse "4." "."
        prop "afterParse \"4..\" \"..\"" $ afterParse "4.." ".."
        prop "afterParse \"4-\" \"-\"" $ afterParse "4-" "-"
        prop "afterParse \"4--\" \"--\"" $ afterParse "4--" "--"
        prop "afterParse \"4.9abc\" \"abc\"" $ afterParse "4.9abc" "abc"
        prop "afterParse \"4.9\" \"\"" $ afterParse "4.9" ""
        prop "afterParse \"+4.9\" \"\"" $ afterParse "+4.9" ""
        prop "afterParse \"-4.9\" \"\"" $ afterParse "-4.9" ""
        prop "afterParse \"4.9.\" \".\"" $ afterParse "4.9." "."
        prop "afterParse \"4.9..\" \"..\"" $ afterParse "4.9.." ".."
        prop "afterParse \"4.9...\" \"...\"" $ afterParse "4.9..." "..."
        prop "afterParse \"4.9+\" \"+\"" $ afterParse "4.9+" "+"
        prop "afterParse \"4.9++\" \"++\"" $ afterParse "4.9++" "++"
        prop "afterParse \"4.9+++\" \"+++\"" $ afterParse "4.9+++" "+++"
        prop "afterParse \"4.9-\" \"-\"" $ afterParse "4.9-" "-"
        prop "afterParse \"4.9--\" \"--\"" $ afterParse "4.9--" "--"
        prop "afterParse \"4.9---\" \"---\"" $ afterParse "4.9---" "---"
        prop "afterParse \"\" \"\"" $ afterParse "" ""
        prop "afterParse \".\" \".\"" $ afterParse "." "."
        prop "afterParse \"..\" \"..\"" $ afterParse ".." ".."
        prop "afterParse \"+\" \"+\"" $ afterParse "+" "+"
        prop "afterParse \"++\" \"++\"" $ afterParse "++" "++"
    H.describe "Scientific parser property test" $ do
        prop "Exponent format" scientificExpFP
        prop "Decimal format" scientificFixFP
    H.describe "double parser property test" $ do
        prop "Exponent format" doubleExpFP
        prop "Decimal format" doubleFixFP
