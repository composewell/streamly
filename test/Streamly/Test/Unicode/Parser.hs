{-# OPTIONS_GHC -Wno-deprecations #-}

module Streamly.Test.Unicode.Parser
    (main)
where

import Streamly.Internal.Data.Parser (ParseError (..))
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (run, monadicIO, assert)

import qualified Streamly.Data.Stream as Stream
    ( fromList, parse )
import qualified Streamly.Internal.Data.Stream as Stream (parseBreak, toList)
import qualified Streamly.Unicode.Parser as Unicode
import qualified Test.Hspec as H
import Debug.Trace (trace)

doubleParser :: String -> IO (Either ParseError Double)
doubleParser = Stream.parse Unicode.double . Stream.fromList

double :: String -> Double -> Property
double s d = monadicIO $ do
    x <- run $ doubleParser s
    case x of
        Right val -> trace ("parsed " ++ show val) (assert (val == d))
        Left (ParseError _) -> assert (False)

doubleErr :: String -> String -> Property
doubleErr s msg = monadicIO $ do
    x <- run $ doubleParser s
    case x of
        Right _ -> assert (False)
        Left (ParseError err) -> trace err (assert (err == msg))

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
