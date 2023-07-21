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

doubleParser :: String -> IO (Either ParseError Double)
doubleParser = Stream.parse Unicode.double . Stream.fromList

double :: String -> Double -> Property
double s d = monadicIO $ do
    x <- run $ doubleParser s
    case x of
        Right val -> assert (val == d)
        Left (ParseError err) -> assert (err == "Not enough input")

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
        prop "double \"44.56-78\" 44.56" $ double "44.56-78" 44.56
        prop "double \"44-56\" 44.0" $ double "44-56" 44.0
        prop "double \"44+56\" 44.0" $ double "44+56" 44.0
        prop "double \"44-\" 44.0" $ double "44-" 44.0
        prop "double \"44+\" 44.0" $ double "44+" 44.0
        prop "double \"44u\" 44.0" $ double "44u" 44.0
        prop "double \"+123.345\" 123.345" $ double "+123.345" 123.345
        prop "double \"-123.345\" (-123.345)" $ double "-123.345" (-123.345)
        prop "double \"0.0.00\" 0.0" $ double "0.0.00" 0.0

        prop "double \".\" Error" $ double "." 2.0
        prop "double \"\" Error" $ double "." 2.0
        prop "double \"-\" Error" $ double "." 2.0
        prop "double \"+\" Error" $ double "." 2.0
        prop "double \"..\" Error" $ double "." 2.0
        prop "double \"++\" Error" $ double "." 2.0
        prop "double \"a\" Error" $ double "." 2.0

        prop "afterParse \"4.9abc\" \"abc\"" $ afterParse "4.9abc" "abc"
        prop "afterParse \"4.9\" \"\"" $ afterParse "4.9" ""
        prop "afterParse \"+4.9\" \"\"" $ afterParse "+4.9" ""
        prop "afterParse \"-4.9\" \"\"" $ afterParse "-4.9" ""
        prop "afterParse \"4.9.\" \"\"" $ afterParse "4.9." "."
        prop "afterParse \"4.9..\" \"\"" $ afterParse "4.9.." ".."
        prop "afterParse \"4.9...\" \"\"" $ afterParse "4.9..." "..."
        prop "afterParse \"4.9+\" \"\"" $ afterParse "4.9+" "+"
        prop "afterParse \"4.9++\" \"\"" $ afterParse "4.9++" "++"
        prop "afterParse \"4.9+++\" \"\"" $ afterParse "4.9+++" "+++"
        prop "afterParse \"4.9-\" \"\"" $ afterParse "4.9-" "-"
        prop "afterParse \"4.9--\" \"\"" $ afterParse "4.9--" "--"
        prop "afterParse \"4.9---\" \"\"" $ afterParse "4.9---" "---"
        prop "afterParse \"\" \"\"" $ afterParse "" ""
        -- XX depened on PR https://github.com/composewell/streamly/pull/2453
        {-
        prop "afterParse \".\" \".\"" $ afterParse "." "."
        prop "afterParse \"..\" \"..\"" $ afterParse ".." ".."
        prop "afterParse \"+\" \"+\"" $ afterParse "+" "+"
        prop "afterParse \"++\" \"++\"" $ afterParse "++" "++"
        -}
