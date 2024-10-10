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

    step ([], buf) _ = pure $ P.Done 1 (reverse buf)
    step (action:xs, buf) a =
        case action of
            Consume n
                | n == 1 -> pure $ P.Continue 0 (xs, a:buf)
                | n > 0 -> pure $ P.Continue 0 (Consume (n - 1) : xs, a:buf)
                | otherwise -> error "Cannot consume <= 0"
            Custom (P.Partial i ()) -> pure $ P.Partial i (xs, buf)
            Custom (P.Continue i ()) -> pure $ P.Continue i (xs, buf)
            Custom (P.Done i ()) -> pure $ P.Done i (reverse buf)
            Custom (P.Error err) -> pure $ P.Error err

    done ([], buf) = pure $ P.Done 0 (reverse buf)
    done (action:xs, buf) =
        case action of
            Consume _ -> pure $ P.Error "INCOMPLETE"
            Custom (P.Partial i ()) -> pure $ P.Partial i (xs, buf)
            Custom (P.Continue i ()) -> pure $ P.Continue i (xs, buf)
            Custom (P.Done i ()) -> pure $ P.Done i (reverse buf)
            Custom (P.Error err) -> pure $ P.Error err

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
    slice_ off = Prelude.drop off

    -- i = Index of inp head
    -- j = Minimum index of inp head
    go i j ys [] = (Right ys, slice_ (max i j) inp)
    go i j ys ((Consume n):xs)
        | i + n > inpLen = (Left (ParseError "INCOMPLETE"), drop j inp)
        | otherwise =
            go (i + n) j (ys ++ slice i n inp) xs
    go i j ys ((Custom step):xs)
        | i > inpLen = error "i > inpLen"
        | i == inpLen =
              -- Where there is no input we do not move forward by default.
              -- Hence it is (i - n) and not (i + 1 - n)
              case step of
                  P.Partial n () -> go (i - n) (max j (i - n)) ys xs
                  P.Continue n () -> go (i - n) j ys xs
                  P.Done n () -> (Right ys, slice_ (max (i - n) j) inp)
                  P.Error err -> (Left (ParseError err), slice_ j inp)
        | otherwise =
              case step of
                  P.Partial n () -> go (i + 1 - n) (max j (i + 1 - n)) ys xs
                  P.Continue n () -> go (i + 1 - n) j ys xs
                  P.Done n () -> (Right ys, slice_ (max (i - n + 1) j) inp)
                  P.Error err -> (Left (ParseError err), slice_ j inp)

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
    Prelude.map (flip Prelude.take xs) [1..length xs]

parserSanityTests :: String -> ([Move] -> SpecWith ()) -> SpecWith ()
parserSanityTests desc testRunner =
    describe desc $ do
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume (tapeLen + 1)
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Custom (P.Error "Message0")
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 10
                , Custom (P.Partial 0 ())
                , Consume 10
                , Custom (P.Partial 1 ())
                , Consume 10
                , Custom (P.Partial 11 ())
                , Consume 10
                , Custom (P.Continue 0 ())
                , Consume 10
                , Custom (P.Continue 1 ())
                , Consume 10
                , Custom (P.Continue 11 ())
                , Custom (P.Error "Message1")
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 10
                , Custom (P.Continue 0 ())
                , Consume 10
                , Custom (P.Continue 1 ())
                , Consume 10
                , Custom (P.Continue 11 ())
                , Consume 10
                , Custom (P.Done 0 ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 20
                , Custom (P.Continue 0 ())
                , Custom (P.Continue 11 ())
                , Custom (P.Done 1 ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 20
                , Custom (P.Continue 0 ())
                , Custom (P.Continue 11 ())
                , Custom (P.Error "Message2")
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume 20
                , Custom (P.Continue 0 ())
                , Custom (P.Continue 11 ())
                , Custom (P.Done 5 ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume tapeLen
                , Custom (P.Continue 0 ())
                , Custom (P.Continue 10 ())
                , Custom (P.Done 5 ())
                ]
        Prelude.mapM_ testRunner $
            createPaths
                [ Consume tapeLen
                , Custom (P.Continue 0 ())
                , Custom (P.Continue 10 ())
                , Custom (P.Error "Message3")
                ]
