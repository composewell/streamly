#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
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
import Test.Tasty.Bench (Benchmark)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.Parser.Common

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE manyWordByEven #-}
manyWordByEven :: Monad m => Stream m Int -> m (Either ParseError ())
manyWordByEven = Stream.parse (PR.many (PR.wordBy even Fold.drain) Fold.drain)

{-# INLINE many #-}
many :: Monad m => Stream m Int -> m (Either ParseError Int)
many = Stream.parse (PR.many (PR.satisfy (> 0)) Fold.length)

{-# INLINE some #-}
some :: Monad m => Stream m Int -> m (Either ParseError Int)
some = Stream.parse (PR.some (PR.satisfy (> 0)) Fold.length)

{-# INLINE manyAlt #-}
manyAlt :: Monad m => Stream m Int -> m Int
manyAlt xs = do
    x <- Stream.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: Monad m => Stream m Int -> m Int
someAlt xs = do
    x <- Stream.parse (AP.some (PR.satisfy (> 0))) xs
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
alt2 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
alt2 value =
    Stream.parse
        (PR.alt
            (takeWhileFail (<= (value `div` 2)) Fold.drain)
            (PR.dropWhile (<= value))
        )

{-# INLINE alt4 #-}
alt4 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
alt4 value =
    Stream.parse
        (   takeWhileFail (<= (value * 1 `div` 4)) Fold.drain
        <|> takeWhileFail (<= (value * 2 `div` 4)) Fold.drain
        <|> takeWhileFail (<= (value * 3 `div` 4)) Fold.drain
        <|> PR.dropWhile (<= value)
        )

{-# INLINE alt8 #-}
alt8 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
alt8 value =
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
alt16 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
alt16 value =
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
altSmall :: Monad m
    => Int -> Stream m Int -> m ()
altSmall value =
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
choiceAsum :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
choiceAsum value =
    Stream.parse (F.asum (replicate value (PR.satisfy (< 0)))
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
      (SpaceO_1, benchIOSink value "alt2parseMany" $ altSmall value)
    , (SpaceO_1, benchIOSink value "alt2" $ alt2 value)
    , (SpaceO_1, benchIOSink value "alt4" $ alt4 value)
    , (SpaceO_1, benchIOSink value "alt8" $ alt8 value)
    , (SpaceO_1, benchIOSink value "alt16" $ alt16 value)

    -- O_n as they accumulate the results in a list.
    , (HeapO_n, benchIOSink value "manyAlt" manyAlt)
    , (HeapO_n, benchIOSink value "someAlt" someAlt)
    , (SpaceO_n, benchIOSink value "choice (asum)/100" $ choiceAsum (value `div` 100))
    -- , benchIOSink value "choice/100" $ choice (value `div` 100)

    -- Sequential Repetition
    , (SpaceO_1, benchIOSink value "many" many)
    , (SpaceO_1, benchIOSink value "many (wordBy even)" $ manyWordByEven)
    , (SpaceO_1, benchIOSink value "some" some)

    {-
    , benchIOSink value "tee" $ teeAllAny value
    , benchIOSink value "teeFst" $ teeFstAllAny value
    , benchIOSink value "shortest" $ shortestAllAny value
    , benchIOSink value "longest" $ longestAllAny value
    -}
    ]
