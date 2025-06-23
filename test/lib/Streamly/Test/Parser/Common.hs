module Streamly.Test.Parser.Common
    ( Move(..)
    , jumpParser
    , chunkedTape
    , tape
    , tapeLen
    , expectedResult
    , expectedResultMany
    , parserSanityTests
    )
where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Parser (ParseError(..))
import qualified Streamly.Internal.Data.Parser as P
import Test.Hspec

--------------------------------------------------------------------------------
-- Parser driver tests
--------------------------------------------------------------------------------

data Move
    = Consume Int
    | Custom (P.Step () ())
    deriving (Show)

jumpParser :: Monad m => [Move] -> P.Parser Int m [Int]
jumpParser jumps = P.Parser step initial done
    where
    initial = pure $ P.IPartial (jumps, [])

    step ([], buf) _ = pure $ P.SDone 0 (reverse buf)
    step (action:xs, buf) a =
        case action of
            Consume n
                | n == 1 -> pure $ P.SContinue 1 (xs, a:buf)
                | n > 0 -> pure $ P.SContinue 1 (Consume (n - 1) : xs, a:buf)
                | otherwise -> error "Cannot consume <= 0"
            Custom (P.SPartial i ()) -> pure $ P.SPartial i (xs, buf)
            Custom (P.SContinue i ()) -> pure $ P.SContinue i (xs, buf)
            Custom (P.SDone i ()) -> pure $ P.SDone i (reverse buf)
            Custom (P.SError err) -> pure $ P.SError err

    done ([], buf) = pure $ P.FDone 0 (reverse buf)
    done (action:xs, buf) =
        case action of
            Consume _ -> pure $ P.FError "INCOMPLETE"
            Custom (P.SPartial i ()) -> pure $ P.FContinue i (xs, buf)
            Custom (P.SContinue i ()) -> pure $ P.FContinue i (xs, buf)
            Custom (P.SDone i ()) -> pure $ P.FDone i (reverse buf)
            Custom (P.SError err) -> pure $ P.FError err

chunkedTape :: [[Int]]
chunkedTape = Prelude.map (\x -> [x..(x+9)]) [1, 11 .. 91]

tape :: [Int]
tape = concat chunkedTape

tapeLen :: Int
tapeLen = length tape

expectedResult :: [Move] -> [Int] -> (Either ParseError [Int], [Int])
expectedResult moves inp = go 0 0 [] moves
    where
    inpLen = length inp

    slice off len = Prelude.take len . Prelude.drop off
    slice_ = Prelude.drop

    -- i = Index of inp head
    -- j = Minimum index of inp head
    go i j ys [] = (Right ys, slice_ (max i j) inp)
    go i j ys ((Consume n):xs)
        | i + n > inpLen = (Left (ParseError "INCOMPLETE"), drop j inp)
        | otherwise =
            go (i + n) j (ys ++ slice i n inp) xs
    go i j ys ((Custom step):xs)
        | i > inpLen = error "i > inpLen"
        | otherwise =
              case step of
                  P.SPartial n () -> go (i + n) (max j (i + n)) ys xs
                  P.SContinue n () -> go (i + n) j ys xs
                  P.SDone n () -> (Right ys, slice_ (max (i + n) j) inp)
                  P.SError err -> (Left (ParseError err), slice_ j inp)

expectedResultMany :: [Move] -> [Int] -> [Either ParseError [Int]]
expectedResultMany _ [] = []
expectedResultMany moves inp =
    let (res, rest) = expectedResult moves inp
     in
       case res of
           Left err -> [Left err]
           Right val -> Right val : expectedResultMany moves rest

createPaths :: [a] -> [[a]]
createPaths xs =
    Prelude.map (`Prelude.take` xs) [1..length xs]

parserSanityTests :: String -> ([Move] -> SpecWith ()) -> SpecWith ()
parserSanityTests desc testRunner =
    describe desc $ do
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume (tapeLen + 1)
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Custom (P.SError "Message0")
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 10
                , Custom (P.SPartial 1 ())
                , Consume 10
                , Custom (P.SPartial 0 ())
                , Consume 10
                , Custom (P.SPartial (-10) ())
                , Consume 10
                , Custom (P.SContinue 1 ())
                , Consume 10
                , Custom (P.SContinue 0 ())
                , Consume 10
                , Custom (P.SContinue (-10) ())
                , Custom (P.SError "Message1")
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 10
                , Custom (P.SContinue 1 ())
                , Consume 10
                , Custom (P.SContinue 0 ())
                , Consume 10
                , Custom (P.SContinue (-10) ())
                , Consume 10
                , Custom (P.SDone 1 ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 20
                , Custom (P.SContinue 1 ())
                , Custom (P.SContinue (-10) ())
                , Custom (P.SDone 0 ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 20
                , Custom (P.SContinue 1 ())
                , Custom (P.SContinue (-10) ())
                , Custom (P.SError "Message2")
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 20
                , Custom (P.SContinue 1 ())
                , Custom (P.SContinue (-10) ())
                , Custom (P.SDone (-4) ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume (tapeLen - 1)
                , Custom (P.SContinue 1 ())
                , Custom (P.SContinue (-9) ())
                , Custom (P.SDone (-4) ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume (tapeLen - 1)
                , Custom (P.SContinue 1 ())
                , Custom (P.SContinue (-9) ())
                , Custom (P.SError "Message3")
                ]
