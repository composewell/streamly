
#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser
  (
    benchmarks
  , sourceUnfoldrM
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..))
import Data.Function ((&))
import Data.Functor (($>))
import Data.Monoid (Sum(..))
import GHC.Magic (inline)
import GHC.Magic (noinline)
import System.IO (Handle)
import System.Random (randomRIO)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Parser
    (ParseError(..), Parser(..), Initial(..), Step(..), Final(..))
import Streamly.Internal.Data.Stream (Stream)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile, dropWhile, span)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Generates something like this: { { \{ \{ } }.  The stream consists of
-- three parts, the first part is contains a sequence of `{`. The second part
-- contains a sequence pf escaped values `\{`. The third part contains a
-- sequence of `}`.
{-# INLINE sourceEscapedFrames #-}
sourceEscapedFrames ::
    Monad m
    => Int
    -> Int
    -> Stream m Char
sourceEscapedFrames value n = Stream.unfoldrM step n
    where

    bs = '\\'
    cbOpen = '{'
    cbClose = '}'
    value1 = value `div` 4

    step cnt
        | cnt > 4 * value1 = return Nothing
        | cnt <= value1 = return $ Just (cbOpen, cnt + 1)
        | cnt > 3 * value1 = return $ Just (cbClose, cnt + 1)
        | otherwise =
            return
                $ Just
                $ if (cnt - value1) `mod` 2 == 1
                  then (bs, cnt + 1)
                  else (cbOpen, cnt + 1)

{-# INLINE benchIOSrc #-}
benchIOSrc
    :: NFData b
    => (Int -> Int -> Stream IO a)
    -> Int
    -> String
    -> (Stream IO a -> IO b)
    -> Benchmark
benchIOSrc src value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . src value

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE takeBetween #-}
takeBetween :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeBetween value = Stream.parse (PR.takeBetween 0 value Fold.drain)

{-# INLINE takeEQ #-}
takeEQ :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeEQ value = Stream.parse (PR.takeEQ value Fold.drain)

{-# INLINE takeGE #-}
takeGE :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeGE value = Stream.parse (PR.takeGE value Fold.drain)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
dropWhile value = Stream.parse (PR.dropWhile (<= value))

{-# INLINE takeBeginBy #-}
takeBeginBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeBeginBy value stream = do
    let stream2 = value `Stream.cons` stream
    Stream.parse (PR.takeBeginBy (== value) Fold.drain) stream2

takeFramedByEsc_ :: Monad m => Int -> Stream m Char -> m (Either ParseError ())
takeFramedByEsc_ _ = Stream.parse parser

    where

    isEsc = (== '\\')
    isBegin = (== '{')
    isEnd = (== '}')

    parser = PR.takeFramedByEsc_ isEsc isBegin isEnd Fold.drain

{-# INLINE listEqBy #-}
listEqBy :: Int -> Stream IO Int -> IO (Either ParseError [Int])
listEqBy len = Stream.parse (PR.listEqBy (==) [1 .. len])

{-# INLINE streamEqBy #-}
streamEqBy :: Int -> Stream IO Int -> IO (Either ParseError ())
streamEqBy len = Stream.parse (PR.streamEqBy (==) (Stream.enumerateFromTo 1 len))

{-# INLINE takeWhile #-}
takeWhile :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeWhile value = Stream.parse (PR.takeWhile (<= value) Fold.drain)

takeWhileP :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeWhileP value =
    Stream.parse (PR.takeWhileP (<= value) (PR.takeWhile (<= value - 1) Fold.drain))

{-# INLINE takeP #-}
takeP :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeP value = Stream.parse (PR.takeP value (PR.fromFold Fold.drain))

{-# INLINE groupBy #-}
groupBy :: Monad m => Stream m Int -> m (Either ParseError ())
groupBy = Stream.parse (PR.groupBy (<=) Fold.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => Stream m Int -> m (Either ParseError ())
groupByRolling = Stream.parse (PR.groupByRolling (<=) Fold.drain)

{-# INLINE wordBy #-}
wordBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
wordBy value = Stream.parse (PR.wordBy (>= value) Fold.drain)

{-# INLINE sepByWords #-}
sepByWords :: Monad m => Stream m Int -> m (Either ParseError ())
sepByWords = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

{-# INLINE sepByAllWords #-}
sepByAllWords :: Monad m => Stream m Int -> m (Either ParseError ())
sepByAllWords = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepByAll (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

-- Returning a list to compare with the sepBy1 in ParserK
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m => Stream m Int -> m (Either ParseError [Int])
sepBy1 xs = do
    Stream.parse (PR.sepBy1 (PR.satisfy odd) (PR.satisfy even) Fold.toList) xs

{-# INLINE sepByWords1 #-}
sepByWords1 :: Monad m => Stream m Int -> m (Either ParseError ())
sepByWords1 = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy1 (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

{-# INLINE deintercalate #-}
deintercalate :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalate _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE deintercalate1 #-}
deintercalate1 :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalate1 _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate1
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE deintercalateAll #-}
deintercalateAll :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalateAll _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalateAll
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE manyWordByEven #-}
manyWordByEven :: Monad m => Stream m Int -> m (Either ParseError ())
manyWordByEven = Stream.parse (PR.many (PR.wordBy even Fold.drain) Fold.drain)

{-# INLINE many #-}
many :: Monad m => Stream m Int -> m (Either ParseError Int)
many = Stream.parse (PR.many (PR.satisfy (> 0)) Fold.length)

{-# INLINE manyAlt #-}
manyAlt :: Monad m => Stream m Int -> m Int
manyAlt xs = do
    x <- Stream.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: Monad m => Stream m Int -> m (Either ParseError Int)
some = Stream.parse (PR.some (PR.satisfy (> 0)) Fold.length)

{-# INLINE someAlt #-}
someAlt :: Monad m => Stream m Int -> m Int
someAlt xs = do
    x <- Stream.parse (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE manyTill #-}
manyTill :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
manyTill value =
    Stream.parse (PR.manyTill (PR.satisfy (> 0)) (PR.satisfy (== value)) Fold.length)

{-# INLINE splitAp2 #-}
splitAp2 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ((), ()))
splitAp2 value =
    Stream.parse
        ((,)
            <$> PR.dropWhile (<= (value `div` 2))
            <*> PR.dropWhile (<= value)
        )

{-# INLINE splitAp4 #-}
splitAp4 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitAp4 value =
    Stream.parse
        (      (\() () () () -> ())
            <$> PR.dropWhile (<= (value * 1 `div` 4))
            <*> PR.dropWhile (<= (value * 2 `div` 4))
            <*> PR.dropWhile (<= (value * 3 `div` 4))
            <*> PR.dropWhile (<= value)
        )

{-# INLINE splitAp8 #-}
splitAp8 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitAp8 value =
    Stream.parse
        (      (\() () () () () () () () -> ())
            <$> PR.dropWhile (<= (value * 1 `div` 8))
            <*> PR.dropWhile (<= (value * 2 `div` 8))
            <*> PR.dropWhile (<= (value * 3 `div` 8))
            <*> PR.dropWhile (<= (value * 4 `div` 8))
            <*> PR.dropWhile (<= (value * 5 `div` 8))
            <*> PR.dropWhile (<= (value * 6 `div` 8))
            <*> PR.dropWhile (<= (value * 7 `div` 8))
            <*> PR.dropWhile (<= value)
        )

{-# INLINE splitApBefore #-}
splitApBefore :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitApBefore value =
    Stream.parse
        (  PR.dropWhile (<= (value `div` 2))
        *> PR.dropWhile (<= value)
        )

{-# INLINE splitApAfter #-}
splitApAfter :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitApAfter value =
    Stream.parse
        (  PR.dropWhile (<= (value `div` 2))
        <* PR.dropWhile (<= value)
        )

{-# INLINE splitWith2 #-}
splitWith2 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ((), ()))
splitWith2 value =
    Stream.parse
        (PR.splitWith (,)
            (PR.dropWhile (<= (value `div` 2)))
            (PR.dropWhile (<= value))
        )

{-# INLINE split_ #-}
split_ :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
split_ value =
    Stream.parse
        (PR.split_
            (PR.dropWhile (<= (value `div` 2)))
            (PR.dropWhile (<= value))
        )

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

{-# INLINE monad #-}
monad :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad value =
    Stream.parse
        $ do
            PR.dropWhile (<= (value `div` 2))
            PR.dropWhile (<= value)

{-# INLINE monad4 #-}
monad4 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad4 value =
    Stream.parse $ do
        PR.dropWhile (<= (value `div` 4))
        PR.dropWhile (<= (value `div` 2))
        PR.dropWhile (<= (value * 3 `div` 4))
        PR.dropWhile (<= value)

{-# INLINE monad8 #-}
monad8 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad8 value =
    Stream.parse $ do
        PR.dropWhile (<= (value * 1 `div` 8))
        PR.dropWhile (<= (value * 2 `div` 8))
        PR.dropWhile (<= (value * 3 `div` 8))
        PR.dropWhile (<= (value * 4 `div` 8))
        PR.dropWhile (<= (value * 5 `div` 8))
        PR.dropWhile (<= (value * 6 `div` 8))
        PR.dropWhile (<= (value * 7 `div` 8))
        PR.dropWhile (<= value)

{-# INLINE monad16 #-}
monad16 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad16 value =
    Stream.parse $ do
        PR.dropWhile (<= (value * 1 `div` 16))
        PR.dropWhile (<= (value * 2 `div` 16))
        PR.dropWhile (<= (value * 3 `div` 16))
        PR.dropWhile (<= (value * 4 `div` 16))
        PR.dropWhile (<= (value * 5 `div` 16))
        PR.dropWhile (<= (value * 6 `div` 16))
        PR.dropWhile (<= (value * 7 `div` 16))
        PR.dropWhile (<= (value * 8 `div` 16))
        PR.dropWhile (<= (value * 9 `div` 16))
        PR.dropWhile (<= (value * 10 `div` 16))
        PR.dropWhile (<= (value * 11 `div` 16))
        PR.dropWhile (<= (value * 12 `div` 16))
        PR.dropWhile (<= (value * 13 `div` 16))
        PR.dropWhile (<= (value * 14 `div` 16))
        PR.dropWhile (<= (value * 15 `div` 16))
        PR.dropWhile (<= value)

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
takeEndBy_ value = Stream.parse (PR.takeEndBy_ (>= value) (PR.fromFold Fold.drain))

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
-- Spanning
-------------------------------------------------------------------------------

{-# INLINE span #-}
span :: Monad m => Int -> Stream m Int -> m (Either ParseError ((), ()))
span value = Stream.parse (PR.span (<= (value `div` 2)) Fold.drain Fold.drain)

{-# INLINE spanBy #-}
spanBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ((), ()))
spanBy value =
    Stream.parse (PR.spanBy (\_ i -> i <= (value `div` 2)) Fold.drain Fold.drain)

{-# INLINE spanByRolling #-}
spanByRolling :: Monad m => Int -> Stream m Int -> m (Either ParseError ((), ()))
spanByRolling value =
    Stream.parse (PR.spanByRolling (\_ i -> i <= value `div` 2) Fold.drain Fold.drain)

parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    Stream.fold Fold.length
        $ Stream.parseMany
              (PR.fromFold $ Fold.take n Fold.sum)
              (Stream.unfold Handle.reader inh)

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
lookAhead value =
    Stream.parse (PR.lookAhead (PR.takeWhile (<= value) Fold.drain) $> ())

-- XXX The timing of this increased 3x after the stepify extract changes.
{-# INLINE sequenceA_ #-}
sequenceA_ :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
sequenceA_ value =
    Stream.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: Monad m => Int -> Stream m Int -> m Int
sequenceA value xs = do
    x <- Stream.parse (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: Monad m => Int -> Stream m Int -> m Int
sequence value xs = do
    x <- Stream.parse (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
sequence_ value xs =
    Stream.parse (foldr f (return ()) (replicate value (PR.takeBetween 0 1 Fold.drain))) xs

    where

    {-# INLINE f #-}
    f m k = m >>= (\_ -> k)

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
-- Parsing with unfolds
-------------------------------------------------------------------------------

{-# INLINE parseManyUnfoldArrays #-}
parseManyUnfoldArrays :: Int -> [Array.Array Int] -> IO ()
parseManyUnfoldArrays count arrays = do
    let src = Producer.source (Just (Producer.OuterLoop arrays))
    let parser = PR.fromFold (Fold.take count Fold.drain)
    let readSrc =
            Producer.producer
                $ Producer.concat Producer.fromList Array.producer
    let streamParser =
            Producer.simplify (Producer.parseMany parser readSrc)
    Stream.fold Fold.drain $ Stream.unfold streamParser src

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: Monad m => Int -> Stream m Int -> m ()
parseMany n =
      Stream.fold Fold.drain
    . fmap getSum
    . Stream.catRights . Stream.parseMany (PR.fromFold $ Fold.take n Fold.mconcat)
    . fmap Sum

{-# INLINE parseManyGroupBy #-}
parseManyGroupBy :: Monad m => (Int -> Int -> Bool) -> Stream m Int -> m ()
parseManyGroupBy cmp =
    Stream.fold Fold.drain . Stream.parseMany (PR.groupBy cmp Fold.drain)

{-# INLINE parseManyGroupsRolling #-}
parseManyGroupsRolling :: Monad m => Bool -> Stream m Int -> m ()
parseManyGroupsRolling b =
      Stream.fold Fold.drain
    . Stream.parseMany (PR.groupByRolling (\_ _ -> b) Fold.drain)

{-# INLINE parseManyGroupsRollingEither #-}
parseManyGroupsRollingEither :: Monad m =>
    (Int -> Int -> Bool) -> Int -> m ()
parseManyGroupsRollingEither cmp value = do
    sourceUnfoldrM value 1
        & Stream.parseMany (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        & Stream.fold Fold.drain

{-# INLINE parseManyGroupsRollingEitherAlt #-}
parseManyGroupsRollingEitherAlt :: Monad m =>
    (Int -> Int -> Bool) -> Int -> m ()
parseManyGroupsRollingEitherAlt cmp value = do
    sourceUnfoldrM value 1
        -- Make the input unsorted.
        & fmap (\x -> if even x then x + 2 else x)
        & Stream.parseMany (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        & Stream.fold Fold.drain

{-# INLINE parseIterate #-}
parseIterate :: Monad m => Int -> Stream m Int -> m ()
parseIterate n =
      Stream.fold Fold.drain
    . fmap getSum
    . Stream.catRights
    . Stream.parseIterate
        (PR.fromFold . Fold.take n . Fold.sconcat)
        (Sum 0)
    . fmap Sum

{-# INLINE concatSequence #-}
concatSequence :: Monad m => Stream m Int -> m (Either ParseError ())
concatSequence =
    Stream.parse $ PR.sequence (Stream.repeat PR.one) Fold.drain

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

benchmarks :: Int -> BenchEnv -> [Array.Array Int] -> [(SpaceComplexity, Benchmark)]
benchmarks value env arrays =
    [ (SpaceO_1, benchIOSink value "takeBetween" $ takeBetween value)
    , (SpaceO_1, benchIOSink value "takeWhile" $ takeWhile value)
    , (SpaceO_1, benchIOSink value "takeWhileP" $ takeWhileP value)
    , (SpaceO_1, benchIOSink value "takeP" $ takeP value)
    , (SpaceO_1, benchIOSink value "dropWhile" $ dropWhile value)
    , (SpaceO_1, benchIOSink value "takeBeginBy" $ takeBeginBy value)
    , (SpaceO_1, benchIOSink value "takeEndBy_" $ takeEndBy_ value)
    , (SpaceO_1, benchIOSink value "groupBy" $ groupBy)
    , (SpaceO_1, benchIOSink value "groupByRolling" $ groupByRolling)
    , (SpaceO_1, benchIOSink value "wordBy" $ wordBy value)
    , (SpaceO_1, benchIOSink value "sepBy (words)" sepByWords)
    , (SpaceO_1, benchIOSink value "sepByAll (words)" sepByAllWords)
    , (SpaceO_1, benchIOSink value "sepBy1 (words)" sepByWords1)
    , (SpaceO_1, benchIOSink value "deintercalate" $ deintercalate value)
    , (SpaceO_1, benchIOSink value "deintercalate1" $ deintercalate1 value)
    , (SpaceO_1, benchIOSink value "deintercalateAll" $ deintercalateAll value)
    -- Applicative and Monad
    , (SpaceO_1, benchIOSink value "splitAp2" $ splitAp2 value)
    , (SpaceO_1, benchIOSink value "splitAp4" $ splitAp4 value)
    , (SpaceO_1, benchIOSink value "splitAp8" $ splitAp8 value)
    , (SpaceO_1, benchIOSink value "splitApBefore" $ splitApBefore value)
    , (SpaceO_1, benchIOSink value "splitApAfter" $ splitApAfter value)
    , (SpaceO_1, benchIOSink value "splitWith2" $ splitWith2 value)
    , (SpaceO_1, benchIOSink value "span" $ span value)
    , (SpaceO_1, benchIOSink value "spanBy" $ spanBy value)
    , (SpaceO_1, benchIOSink value "spanByRolling" $ spanByRolling value)
    , (SpaceO_1, benchIOSink value "monad2" $ monad value)
    , (SpaceO_1, benchIOSink value "monad4" $ monad4 value)
    , (SpaceO_1, benchIOSink value "monad8" $ monad8 value)
    -- Alternative
    , (SpaceO_1, benchIOSink value "alt2parseMany" $ altSmall value)
    , (SpaceO_1, benchIOSink value "alt2" $ alt2 value)
    , (SpaceO_1, benchIOSink value "alt4" $ alt4 value)
    , (SpaceO_1, benchIOSink value "alt8" $ alt8 value)
    , (SpaceO_1, benchIOSink value "alt16" $ alt16 value)
    , (SpaceO_1, benchIOSink value "many" many)
    , (SpaceO_1, benchIOSink value "many (wordBy even)" $ manyWordByEven)
    , (SpaceO_1, benchIOSink value "some" some)
    , (SpaceO_1, benchIOSink value "manyTill" $ manyTill value)
    , (SpaceO_1, benchIOSink value "parseMany" $ parseMany value)
    , (SpaceO_1, benchIOSink value "parseMany (take 1)" (parseMany 1))
    , (SpaceO_1, benchIOSink value "parseMany (take all)" (parseMany value))
    , (SpaceO_1, benchIOSink value "parseMany (groupBy (<))" (parseManyGroupBy (<)))
    , (SpaceO_1, benchIOSink value "parseMany (groupBy (==))" (parseManyGroupBy (==)))
    , (SpaceO_1, benchIOSink value "parseMany groupRollingBy (bound groups)"
          $ parseManyGroupsRolling False)
    , (SpaceO_1, benchIOSink value "parseMany groupRollingBy (1 group)"
          $ parseManyGroupsRolling True)
    , (SpaceO_1, bench "parseMany groupRollingByEither (Left)"
        $ nfIO $ parseManyGroupsRollingEitherLeft)
    , (SpaceO_1, bench "parseMany groupRollingByEither (Right)"
        $ nfIO $ parseManyGroupsRollingEitherRight)
    , (SpaceO_1, bench "parseMany groupRollingByEither (Alternating)"
        $ nfIO $ parseManyGroupsRollingEitherAlt1)
    , (SpaceO_1, benchIOSink value "parseIterate (take 1)" (parseIterate 1))
    , (SpaceO_1, benchIOSink value "parseIterate (take all)" (parseIterate value))
    , (SpaceO_1, benchIOSink value "concatSequence" concatSequence)
    {-
    , benchIOSink value "tee" $ teeAllAny value
    , benchIOSink value "teeFst" $ teeFstAllAny value
    , benchIOSink value "shortest" $ shortestAllAny value
    , benchIOSink value "longest" $ longestAllAny value
    -}
    , (SpaceO_1, benchIOSink value "streamEqBy" (streamEqBy value))
    , (SpaceO_1, mkBench ("parseMany (Fold.take " ++ show (bigSize env) ++ " Fold.sum)") env
          $ \inh _ -> noinline parseManyChunksOfSum (bigSize env) inh)
    , (SpaceO_1, mkBench "parseMany (Fold.take 1 Fold.sum)" env
          $ \inh _ -> inline parseManyChunksOfSum 1 inh)
    , (SpaceO_1, bench "parseMany/Unfold/1000 arrays/take all"
        $ nfIO $ parseManyUnfoldArrays value arrays)
    , (SpaceO_1, bench "parseMany/Unfold/1000 arrays/take 1"
        $ nfIO $ parseManyUnfoldArrays 1 arrays)
    , (HeapO_n, benchIOSink value "takeEQ" $ takeEQ value)
    , (HeapO_n, benchIOSink value "takeGE" $ takeGE value)

    -- lookahead benchmark holds the entire input till end
    , (HeapO_n, benchIOSink value "lookAhead" $ lookAhead value)

    -- o-n-heap because of backtracking
    , (HeapO_n, benchIOSrc sourceEscapedFrames value "takeFramedByEsc_"
        $ takeFramedByEsc_ value)

    -- non-linear time complexity (parserD)
    , (HeapO_n, benchIOSink value "split_" $ split_ value)
    -- XXX Takes lot of space when run on a long stream, why?
    , (HeapO_n, benchIOSink value "monad16" $ monad16 value)

    -- These show non-linear time complexity.
    -- They accumulate the results in a list.
    , (HeapO_n, benchIOSink value "sepBy1" sepBy1)
    , (HeapO_n, benchIOSink value "manyAlt" manyAlt)
    , (HeapO_n, benchIOSink value "someAlt" someAlt)
    , (HeapO_n, benchIOSink value "listEqBy" (listEqBy value))
    , (SpaceO_n, benchIOSink value "sequenceA/100" $ sequenceA (value `div` 100))
    , (SpaceO_n, benchIOSink value "sequenceA_/100" $ sequenceA_ (value `div` 100))
    , (SpaceO_n, benchIOSink value "sequence/100" $ sequence (value `div` 100))
    , (SpaceO_n, benchIOSink value "sequence_/100" $ sequence_ (value `div` 100))
    , (SpaceO_n, benchIOSink value "choice (asum)/100" $ choiceAsum (value `div` 100))
    -- , benchIOSink value "choice/100" $ choice (value `div` 100)
    ]
    where

    {-# NOINLINE parseManyGroupsRollingEitherLeft #-}
    parseManyGroupsRollingEitherLeft = parseManyGroupsRollingEither (<) value

    {-# NOINLINE parseManyGroupsRollingEitherRight #-}
    parseManyGroupsRollingEitherRight = parseManyGroupsRollingEither (>) value

    {-# NOINLINE parseManyGroupsRollingEitherAlt1 #-}
    parseManyGroupsRollingEitherAlt1 =
        parseManyGroupsRollingEitherAlt (>) value
