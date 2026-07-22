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
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Types (SPEC(..))
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
import Fusion.Plugin.Types

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as S
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . streamUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# ANN splitMany_WordBy (PermitPatternMatches [''(), ''Int]) #-}
{-# ANN splitMany_WordBy (PermitConstructions [''(), ''Either]) #-}
{-# ANN splitMany_WordBy (PermitTypeClasses []) #-}
{-# NOINLINE splitMany_WordBy #-}
splitMany_WordBy :: Int -> Int -> IO (Either ParseError ())
splitMany_WordBy value =
    withStream value
        $ Stream.parse
            (PR.splitMany (PR.wordBy even Fold.drain) Fold.drain)

#ifdef INSPECTION
inspect $ 'splitMany_WordBy `hasNoType` ''S.Step
inspect $ 'splitMany_WordBy `hasNoType` ''PR.Step
inspect $ 'splitMany_WordBy `hasNoType` ''PR.Initial
inspect $ 'splitMany_WordBy `hasNoType` ''FL.Step
inspect $ 'splitMany_WordBy `hasNoType` ''SPEC
inspect $ 'splitMany_WordBy `hasNoType` ''PR.Fused3
#endif

{-# ANN splitMany_Satisfy (PermitPatternMatches [''Int]) #-}
{-# ANN splitMany_Satisfy (PermitConstructions [''Int, ''Either]) #-}
{-# ANN splitMany_Satisfy (PermitTypeClasses []) #-}
{-# NOINLINE splitMany_Satisfy #-}
splitMany_Satisfy :: Int -> Int -> IO (Either ParseError Int)
splitMany_Satisfy value =
    withStream value
        $ Stream.parse (PR.splitMany (PR.satisfy (> 0)) Fold.length)

#ifdef INSPECTION
inspect $ 'splitMany_Satisfy `hasNoType` ''S.Step
inspect $ 'splitMany_Satisfy `hasNoType` ''PR.Step
inspect $ 'splitMany_Satisfy `hasNoType` ''PR.Initial
inspect $ 'splitMany_Satisfy `hasNoType` ''FL.Step
inspect $ 'splitMany_Satisfy `hasNoType` ''SPEC
inspect $ 'splitMany_Satisfy `hasNoType` ''PR.Fused3
#endif

{-# ANN splitSome (PermitPatternMatches [''Int]) #-}
{-# ANN splitSome (PermitConstructions [''Either, ''Int]) #-}
{-# ANN splitSome (PermitTypeClasses []) #-}
{-# NOINLINE splitSome #-}
splitSome :: Int -> Int -> IO (Either ParseError Int)
splitSome value =
    withStream value
        $ Stream.parse (PR.splitSome (PR.satisfy (> 0)) Fold.length)

#ifdef INSPECTION
inspect $ 'splitSome `hasNoType` ''S.Step
inspect $ 'splitSome `hasNoType` ''PR.Step
inspect $ 'splitSome `hasNoType` ''PR.Initial
inspect $ 'splitSome `hasNoType` ''FL.Step
inspect $ 'splitSome `hasNoType` ''SPEC
inspect $ 'splitSome `hasNoType` ''PR.Fused3
#endif

{-# ANN many_AlternativeInstance (PermitPatternMatches [''[], ''Int]) #-}
{-# ANN many_AlternativeInstance (PermitConstructions [''[], ''Int]) #-}
{-# ANN many_AlternativeInstance (PermitTypeClasses []) #-}
{-# NOINLINE many_AlternativeInstance #-}
many_AlternativeInstance :: Int -> Int -> IO Int
many_AlternativeInstance value start = do
    x <- withStream value (Stream.parse (AP.many (PR.satisfy (> 0)))) start
    return $ Prelude.length x

{-# ANN some_AlternativeInstance (PermitPatternMatches [''Int, ''[]]) #-}
{-# ANN some_AlternativeInstance (PermitConstructions [''[], ''Int]) #-}
{-# ANN some_AlternativeInstance (PermitTypeClasses []) #-}
{-# NOINLINE some_AlternativeInstance #-}
some_AlternativeInstance :: Int -> Int -> IO Int
some_AlternativeInstance value start = do
    x <- withStream value (Stream.parse (AP.some (PR.satisfy (> 0)))) start
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

{-# ANN alt_x2 (PermitPatternMatches [''[], ''Int, ''(,), ''SPEC, ''PR.AltParseState]) #-}
{-# ANN alt_x2 (PermitConstructions
    [''[], ''Int, ''SrcLoc, ''CallStack, ''(), ''(,), ''PR.AltParseState, ''Either]) #-}
{-# ANN alt_x2 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_x2 #-}
alt_x2 :: Int -> Int -> IO (Either ParseError ())
alt_x2 value =
    withStream value $
        Stream.parse
            (PR.alt
                (takeWhileFail (<= (value `div` 2)) Fold.drain)
                (PR.dropWhile (<= value))
            )

#ifdef INSPECTION
inspect $ 'alt_x2 `hasNoType` ''S.Step
inspect $ 'alt_x2 `hasNoType` ''PR.Step
inspect $ 'alt_x2 `hasNoType` ''PR.Initial
inspect $ 'alt_x2 `hasNoType` ''FL.Step
-- inspect $ 'alt_x2 `hasNoType` ''SPEC
-- inspect $ 'alt_x2 `hasNoType` ''PR.AltParseState
#endif

{- HLINT ignore "Evaluate"-}
{-# ANN alt_AlternativeInstance_x4 (PermitPatternMatches
    [''[], ''Int, ''(,), ''SPEC, ''PR.AltParseState]) #-}
{-# ANN alt_AlternativeInstance_x4 (PermitConstructions
    [''[], ''Int, ''SrcLoc, ''CallStack, ''PR.AltParseState, ''(), ''(,)]) #-}
{-# ANN alt_AlternativeInstance_x4 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_AlternativeInstance_x4 #-}
alt_AlternativeInstance_x4 :: Int -> Int -> IO (Either ParseError ())
alt_AlternativeInstance_x4 value =
    withStream value $
        Stream.parse
            (   takeWhileFail (<= (value * 1 `div` 4)) Fold.drain
            <|> takeWhileFail (<= (value * 2 `div` 4)) Fold.drain
            <|> takeWhileFail (<= (value * 3 `div` 4)) Fold.drain
            <|> PR.dropWhile (<= value)
            )

{-# ANN alt_AlternativeInstance_x8 (PermitPatternMatches
    [''[], ''Int, ''(,), ''SPEC, ''PR.AltParseState]) #-}
{-# ANN alt_AlternativeInstance_x8 (PermitConstructions
    [''[], ''Int, ''SrcLoc, ''CallStack, ''PR.AltParseState, ''(), ''(,)]) #-}
{-# ANN alt_AlternativeInstance_x8 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_AlternativeInstance_x8 #-}
alt_AlternativeInstance_x8 :: Int -> Int -> IO (Either ParseError ())
alt_AlternativeInstance_x8 value =
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

{-# ANN alt_AlternativeInstance_x16 (PermitPatternMatches
    [''[], ''Int, ''(,), ''SPEC, ''PR.AltParseState]) #-}
{-# ANN alt_AlternativeInstance_x16 (PermitConstructions
    [''[], ''Int, ''SrcLoc, ''CallStack, ''PR.AltParseState, ''(), ''(,)]) #-}
{-# ANN alt_AlternativeInstance_x16 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_AlternativeInstance_x16 #-}
alt_AlternativeInstance_x16 :: Int -> Int -> IO (Either ParseError ())
alt_AlternativeInstance_x16 value =
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

{-# ANN alt_ParseMany_x2 (PermitPatternMatches
    [''[], ''(,), ''PR.AltParseState, ''PR.SeqAState, ''Int]) #-}
{-# ANN alt_ParseMany_x2 (PermitConstructions
    [''[], ''Int, ''SrcLoc, ''CallStack, ''PR.AltParseState, ''(), ''(,)]) #-}
{-# ANN alt_ParseMany_x2 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_ParseMany_x2 #-}
alt_ParseMany_x2 :: Int -> Int -> IO ()
alt_ParseMany_x2 value =
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
{-# ANN asum (PermitPatternMatches
    [''PR.AltParseState, ''Int, ''Parser, ''Initial, ''Step, ''Final, ''[], ''IO, ''(,)]) #-}
{-# ANN asum (PermitConstructions
    [ ''Final, ''PR.AltParseState, ''(), ''Initial, ''Step, ''Parser, ''[]
    , ''Int, ''SrcLoc, ''CallStack, ''Char, ''(,), ''Either
    ]) #-}
{-# ANN asum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE asum #-}
asum :: Int -> Int -> IO (Either ParseError Int)
asum value =
    withStream value $
        Stream.parse
            (F.asum (replicate value (PR.satisfy (< 0)))
                AP.<|> PR.satisfy (> 0))

{-
{-# INLINE choice #-}
choice :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
choice value =
    Stream.parse
        (PR.choice (replicate value (PR.satisfy (< 0)))
            AP.<|> PR.satisfy (> 0))
-}

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Alternative
      (SpaceO_1, benchIO "alt_ParseMany_x2 (small parses)"
          $ alt_ParseMany_x2 value)
    , (SpaceO_1, benchIO "alt_x2" $ alt_x2 value)
    , (SpaceO_1, benchIO "alt_AlternativeInstance_x4 (<|>)"
          $ alt_AlternativeInstance_x4 value)
    , (SpaceO_1, benchIO "alt_AlternativeInstance_x8 (<|>)"
          $ alt_AlternativeInstance_x8 value)
    , (SpaceO_1, benchIO "alt_AlternativeInstance_x16 (<|>)"
          $ alt_AlternativeInstance_x16 value)

    -- O_n as they accumulate the results in a list.
    , (HeapO_n, benchIO "many_AlternativeInstance"
          $ many_AlternativeInstance value)
    , (HeapO_n, benchIO "some_AlternativeInstance"
          $ some_AlternativeInstance value)
    , (SpaceO_n, benchIO "asum (value div 100)" $ asum (value `div` 100))
    -- , benchIO "choice (value div 100)" $ choice (value `div` 100)

    -- Sequential Repetition
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "splitMany_Satisfy" $ splitMany_Satisfy value)
    , (SpaceO_1, benchIO "splitMany_WordBy (wordBy even)"
          $ splitMany_WordBy value)
    , (SpaceO_1, benchIO "splitSome" $ splitSome value)

    {-
    , benchIO "tee" $ teeAllAny value
    , benchIO "teeFst" $ teeFstAllAny value
    , benchIO "shortest" $ shortestAllAny value
    , benchIO "longest" $ longestAllAny value
    -}
    ]
