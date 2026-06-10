#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Data.Parser.Alternative
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Alternative
  (
    benchmarks
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..))
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Parser
    (ParseError(..), Parser(..), Initial(..), Step(..), Final(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as S
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1,1) >>= f . streamUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE splitManyWordByEven #-}
splitManyWordByEven :: Int -> IO (Either ParseError ())
splitManyWordByEven value =
    withStream value $ Stream.parse (PR.splitMany (PR.wordBy even Fold.drain) Fold.drain)

#ifdef INSPECTION
inspect $ 'splitManyWordByEven `hasNoType` ''S.Step
inspect $ 'splitManyWordByEven `hasNoType` ''PR.Step
inspect $ 'splitManyWordByEven `hasNoType` ''PR.Initial
inspect $ 'splitManyWordByEven `hasNoType` ''FL.Step
inspect $ 'splitManyWordByEven `hasNoType` ''SPEC
inspect $ 'splitManyWordByEven `hasNoType` ''PR.Fused3
#endif

{-# INLINE splitMany #-}
splitMany :: Int -> IO (Either ParseError Int)
splitMany value = withStream value $ Stream.parse (PR.splitMany (PR.satisfy (> 0)) Fold.length)

#ifdef INSPECTION
inspect $ 'splitMany `hasNoType` ''S.Step
inspect $ 'splitMany `hasNoType` ''PR.Step
inspect $ 'splitMany `hasNoType` ''PR.Initial
inspect $ 'splitMany `hasNoType` ''FL.Step
inspect $ 'splitMany `hasNoType` ''SPEC
inspect $ 'splitMany `hasNoType` ''PR.Fused3
#endif

{-# INLINE splitSome #-}
splitSome :: Int -> IO (Either ParseError Int)
splitSome value = withStream value $ Stream.parse (PR.splitSome (PR.satisfy (> 0)) Fold.length)

#ifdef INSPECTION
inspect $ 'splitSome `hasNoType` ''S.Step
inspect $ 'splitSome `hasNoType` ''PR.Step
inspect $ 'splitSome `hasNoType` ''PR.Initial
inspect $ 'splitSome `hasNoType` ''FL.Step
inspect $ 'splitSome `hasNoType` ''SPEC
inspect $ 'splitSome `hasNoType` ''PR.Fused3
#endif

{-# INLINE manyAlt #-}
manyAlt :: Int -> IO Int
manyAlt value = do
    x <- withStream value $ Stream.parse (AP.many (PR.satisfy (> 0)))
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: Int -> IO Int
someAlt value = do
    x <- withStream value $ Stream.parse (AP.some (PR.satisfy (> 0)))
    return $ Prelude.length x

-- XXX dropWhile with applicative does not fuse
-- PR.dropWhile (<= (value * 1 `div` 4)) *> PR.die "alt"
{-# INLINE takeWhileFail #-}
takeWhileFail :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeWhileFail predicate (Fold fstep finitial _ ffinal) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            Fold.Partial s -> IPartial s
            Fold.Done b -> IDone b

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      Fold.Partial s1 -> SPartial 1 s1
                      Fold.Done b -> SDone 1 b
        else return $ SError "fail"

    extract s = fmap (FDone 0) (ffinal s)

{-# INLINE alt2 #-}
alt2 :: Int -> IO (Either ParseError ())
alt2 value =
    withStream value $
        Stream.parse
            (PR.alt
                (takeWhileFail (<= (value `div` 2)) Fold.drain)
                (PR.dropWhile (<= value))
            )

#ifdef INSPECTION
inspect $ 'alt2 `hasNoType` ''S.Step
inspect $ 'alt2 `hasNoType` ''PR.Step
inspect $ 'alt2 `hasNoType` ''PR.Initial
inspect $ 'alt2 `hasNoType` ''FL.Step
-- inspect $ 'alt2 `hasNoType` ''SPEC
-- inspect $ 'alt2 `hasNoType` ''PR.AltParseState
#endif

{- HLINT ignore "Evaluate"-}
{-# INLINE alt4 #-}
alt4 :: Int -> IO (Either ParseError ())
alt4 value =
    withStream value $
        Stream.parse
            (   takeWhileFail (<= (value * 1 `div` 4)) Fold.drain
            <|> takeWhileFail (<= (value * 2 `div` 4)) Fold.drain
            <|> takeWhileFail (<= (value * 3 `div` 4)) Fold.drain
            <|> PR.dropWhile (<= value)
            )

{-# INLINE alt8 #-}
alt8 :: Int -> IO (Either ParseError ())
alt8 value =
    withStream value $
        Stream.parse
            (   takeWhileFail (<= (value * 1 `div` 8)) Fold.drain
            <|> takeWhileFail (<= (value * 2 `div` 8)) Fold.drain
            <|> takeWhileFail (<= (value * 3 `div` 8)) Fold.drain
            <|> takeWhileFail (<= (value * 4 `div` 8)) Fold.drain
            <|> takeWhileFail (<= (value * 5 `div` 8)) Fold.drain
            <|> takeWhileFail (<= (value * 6 `div` 8)) Fold.drain
            <|> takeWhileFail (<= (value * 7 `div` 8)) Fold.drain
            <|> PR.dropWhile (<= value)
            )

{-# INLINE alt16 #-}
alt16 :: Int -> IO (Either ParseError ())
alt16 value =
    withStream value $
        Stream.parse
            (   takeWhileFail (<= (value * 1 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 2 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 3 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 4 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 5 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 6 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 8 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 9 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 10 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 11 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 12 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 13 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 14 `div` 16)) Fold.drain
            <|> takeWhileFail (<= (value * 15 `div` 16)) Fold.drain
            <|> PR.dropWhile (<= value)
            )

{-# INLINE altSmall #-}
altSmall :: Int -> IO ()
altSmall value =
    withStream value $
        Stream.fold Fold.drain .
            Stream.parseMany
                (PR.alt
                    (PR.satisfy (>= value) *> PR.die "alt")
                    (PR.satisfy (<= value))
                )

{-
{-# INLINE teeAllAny #-}
teeAllAny :: Monad m
    => Int -> Stream m Int -> m ((), ())
teeAllAny value =
    Stream.parse
        (PR.teeWith (,)
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: Monad m
    => Int -> Stream m Int -> m ((), ())
teeFstAllAny value =
    Stream.parse
        (PR.teeWithFst (,)
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
shortestAllAny value =
    Stream.parse
        (PR.shortest
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
longestAllAny value =
    Stream.parse
        (PR.longest
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )
-}

-------------------------------------------------------------------------------
-- Choice
-------------------------------------------------------------------------------

-- choice using the "Alternative" instance with direct style parser type has
-- quadratic performance complexity.
--
{-# INLINE choiceAsum #-}
choiceAsum :: Int -> IO (Either ParseError Int)
choiceAsum value =
    withStream value $
        Stream.parse
            (F.asum (replicate value (PR.satisfy (< 0)))
                AP.<|> PR.satisfy (> 0))

{-
{-# INLINE choice #-}
choice :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
choice value =
    Stream.parse
        (PR.choice (replicate value (PR.satisfy (< 0))) AP.<|> PR.satisfy (> 0))
-}

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Alternative
      (SpaceO_1, benchIO "alt2parseMany" $ altSmall value)
    , (SpaceO_1, benchIO "alt2" $ alt2 value)
    , (SpaceO_1, benchIO "alt4" $ alt4 value)
    , (SpaceO_1, benchIO "alt8" $ alt8 value)
    , (SpaceO_1, benchIO "alt16" $ alt16 value)

    -- O_n as they accumulate the results in a list.
    , (HeapO_n, benchIO "manyAlt" $ manyAlt value)
    , (HeapO_n, benchIO "someAlt" $ someAlt value)
    , (SpaceO_n, benchIO "choice (asum)/100" $ choiceAsum (value `div` 100))
    -- , benchIO "choice/100" $ choice (value `div` 100)

    -- Sequential Repetition
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "splitMany" $ splitMany value)
    , (SpaceO_1, benchIO "splitMany (wordBy even)" $ splitManyWordByEven value)
    , (SpaceO_1, benchIO "splitSome" $ splitSome value)

    {-
    , benchIO "tee" $ teeAllAny value
    , benchIO "teeFst" $ teeFstAllAny value
    , benchIO "shortest" $ shortestAllAny value
    , benchIO "longest" $ longestAllAny value
    -}
    ]
