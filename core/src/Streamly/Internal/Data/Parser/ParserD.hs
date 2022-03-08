#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style parser implementation with stream fusion.

module Streamly.Internal.Data.Parser.ParserD
    (
      Parser (..)
    , ParseError (..)
    , Step (..)
    , Initial (..)
    , rmapM

    -- * Conversion to/from ParserK
    , fromParserK
    , toParserK

    -- * Downgrade to Fold
    , toFold

    -- First order parsers
    -- * Accumulators
    , fromFold
    , fromPure
    , fromEffect
    , die
    , dieM

    -- * Element parsers
    , peek
    , eof
    , satisfy
    , next
    , maybe
    , either

    -- * Sequence parsers
    --
    -- Parsers chained in series, if one parser terminates the composition
    -- terminates. Currently we are using folds to collect the output of the
    -- parsers but we can use Parsers instead of folds to make the composition
    -- more powerful. For example, we can do:
    --
    -- sliceSepByMax cond n p = sliceBy cond (take n p)
    -- sliceSepByBetween cond m n p = sliceBy cond (takeBetween m n p)
    -- takeWhileBetween cond m n p = takeWhile cond (takeBetween m n p)
    --
    -- Grab a sequence of input elements without inspecting them
    , takeBetween
    -- , take -- take   -- takeBetween 0 n
    -- , takeLE1 -- take1 -- takeBetween 1 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , takeP
    , lookAhead
    , takeWhile
    , takeWhile1
    , sliceSepByP
    -- , sliceSepByBetween
    , sliceBeginWith
    -- , sliceSepWith
    --
    -- , frameSepBy -- parse frames escaped by an escape char/sequence
    -- , frameEndWith
    --
    , wordBy
    , groupBy
    , groupByRolling
    , groupByRollingEither
    , eqBy
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string

    -- ** Spanning
    , span
    , spanBy
    , spanByRolling

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- ** Sequential Applicative
    , serialWith
    , split_

    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate

    -- ** Sequential Alternative
    , alt

    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest

    -- * N-ary Combinators
    -- ** Sequential Collection
    , sequence
    , concatMap

    -- ** Sequential Repetition
    , count
    , countBetween
    -- , countBetweenTill

    , many
    , some
    , manyTill

    -- -- ** Special cases
    -- XXX traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by "\r\n". The main parser
    -- will have to detect and exclude the sequence "\r\n" anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- , endBy
    -- , sepBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy

    -- -- * Distribution
    --
    -- A simple and stupid impl would be to just convert the stream to an array
    -- and give the array reference to all consumers. The array can be grown on
    -- demand by any consumer and truncated when nonbody needs it.
    --
    -- -- ** Distribute to collection
    -- -- ** Distribute to repetition

    -- -- ** Interleaved collection
    -- Round robin
    -- Priority based
    -- -- ** Interleaved repetition
    -- repeat one parser and when it fails run an error recovery parser
    -- e.g. to find a key frame in the stream after an error

    -- ** Collection of Alternatives
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    , choice   -- first successful in position

    -- -- ** Repeated Alternatives
    -- , retryMax    -- try N times
    -- , retryUntil  -- try until successful
    -- , retryUntilN -- try until successful n times
    )
where

import Control.Exception (assert, Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Fold.Type as FL

import Prelude hiding
       (any, all, take, takeWhile, sequence, concatMap, maybe, either, span)
import Streamly.Internal.Data.Parser.ParserD.Tee
import Streamly.Internal.Data.Parser.ParserD.Type

--
-- $setup
-- >>> :m
-- >>> :set -package streamly
-- >>> import Prelude hiding ()
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser

-------------------------------------------------------------------------------
-- Downgrade a parser to a Fold
-------------------------------------------------------------------------------

data ParserToFoldError =
      InitialError String
    | PartialError Int
    | ContinueError Int
    | DoneError Int
    | ErrorError String
    deriving Show

instance Exception ParserToFoldError

-- | See 'Streamly.Internal.Data.Parser.toFold'.
--
-- /Internal/
--
{-# INLINE toFold #-}
toFold :: MonadThrow m => Parser m a b -> Fold m a b
toFold (Parser pstep pinitial pextract) = Fold step initial pextract

    where

    initial = do
        r <- pinitial
        case r of
            IPartial s -> return $ FL.Partial s
            IDone b -> return $ FL.Done b
            IError err -> throwM $ InitialError err

    step st a = do
        r <- pstep st a
        case r of
            Partial 0 s -> return $ FL.Partial s
            Continue 0 s -> return $ FL.Partial s
            Done 0 b -> return $ FL.Done b
            Partial n _ -> throwM $ PartialError n
            Continue n _ -> throwM $ ContinueError n
            Done n _ -> throwM $ DoneError n
            Error err -> throwM $ ErrorError err

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.fromFold'.
--
-- /Pre-release/
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser m a b
fromFold (Fold fstep finitial fextract) = Parser step initial fextract

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s1 -> IPartial s1
                  FL.Done b -> IDone b

    step s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 s1
                  FL.Done b -> Done 0 b

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.peek'.
--
-- /Pre-release/
--
{-# INLINE peek #-}
peek :: MonadThrow m => Parser m a a
peek = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $ Done 1 a

    extract () = throwM $ ParseError "peek: end of input"

-- | See 'Streamly.Internal.Data.Parser.eof'.
--
-- /Pre-release/
--
{-# INLINE eof #-}
eof :: Monad m => Parser m a ()
eof = Parser step initial return

    where

    initial = return $ IPartial ()

    step () _ = return $ Error "eof: not at end of input"

-- | See 'Streamly.Internal.Data.Parser.satisfy'.
--
-- /Pre-release/
--
{-# INLINE satisfy #-}
satisfy :: MonadThrow m => (a -> Bool) -> Parser m a a
satisfy predicate = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        if predicate a
        then Done 0 a
        else Error "satisfy: predicate failed"

    extract _ = throwM $ ParseError "satisfy: end of input"

-- | See 'Streamly.Internal.Data.Parser.next'.
--
-- /Pre-release/
--
{-# INLINE next #-}
next :: Monad m => Parser m a (Maybe a)
next = Parser step initial extract
  where
  initial = pure $ IPartial ()
  step _ a = pure $ Done 0 (Just a)
  extract _ = pure Nothing

-- | See 'Streamly.Internal.Data.Parser.maybe'.
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: MonadThrow m => (a -> Maybe b) -> Parser m a b
maybe parserF = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        case parserF a of
            Just b -> Done 0 b
            Nothing -> Error "maybe: predicate failed"

    extract _ = throwM $ ParseError "maybe: end of input"

-- | See 'Streamly.Internal.Data.Parser.either'.
--
-- /Pre-release/
--
{-# INLINE either #-}
either :: MonadThrow m => (a -> Either String b) -> Parser m a b
either parserF = Parser step initial extract

    where

    initial = return $ IPartial ()

    step () a = return $
        case parserF a of
            Right b -> Done 0 b
            Left err -> Error $ "either: " ++ err

    extract _ = throwM $ ParseError "either: end of input"

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- Required to fuse "take" with "many" in "chunksOf", for ghc-9.x
{-# ANN type Tuple'Fused Fuse #-}
data Tuple'Fused a b = Tuple'Fused !a !b deriving Show

-- | See 'Streamly.Internal.Data.Parser.takeBetween'.
--
-- /Pre-release/
--
{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> Int -> Fold m a b -> Parser m a b
takeBetween low high (Fold fstep finitial fextract) =

    Parser step initial (extract streamErr)

    where

    streamErr i =
           "takeBetween: Expecting alteast " ++ show low
        ++ " elements, got " ++ show i

    invalidRange =
        "takeBetween: lower bound - " ++ show low
            ++ " is greater than higher bound - " ++ show high

    foldErr :: Int -> String
    foldErr i =
        "takeBetween: the collecting fold terminated after"
            ++ " consuming" ++ show i ++ " elements"
            ++ " minimum" ++ show low ++ " elements needed"

    -- Exactly the same as snext except different constructors, we can possibly
    -- deduplicate the two.
    {-# INLINE inext #-}
    inext i res =
        let i1 = i + 1
        in case res of
            FL.Partial s -> do
                let s1 = Tuple'Fused i1 s
                if i1 < high
                -- XXX ideally this should be a Continue instead
                then return $ IPartial s1
                else IDone <$> extract foldErr s1
            FL.Done b ->
                return
                    $ if i1 >= low
                      then IDone b
                      else IError (foldErr i1)

    initial = do
        when (low >= 0 && high >= 0 && low > high)
            $ throwM $ ParseError invalidRange

        finitial >>= inext (-1)

    -- Keep the impl same as inext
    {-# INLINE snext #-}
    snext i res =
        let i1 = i + 1
        in case res of
            FL.Partial s -> do
                let s1 = Tuple'Fused i1 s
                if i1 < high
                then return $ Continue 0 s1
                else Done 0 <$> extract foldErr s1
            FL.Done b ->
                return
                    $ if i1 >= low
                      then Done 0 b
                      else Error (foldErr i1)

    step (Tuple'Fused i s) a = fstep s a >>= snext i

    extract f (Tuple'Fused i s)
        | i >= low && i <= high = fextract s
        | otherwise = throwM $ ParseError (f i)

-- | See 'Streamly.Internal.Data.Parser.takeEQ'.
--
-- /Pre-release/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeEQ n (Fold fstep finitial fextract) = Parser step initial extract

    where

    cnt = max n 0

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial $ Tuple' 0 s
            FL.Done b ->
                if cnt == 0
                then IDone b
                else IError
                         $ "takeEQ: Expecting exactly " ++ show cnt
                             ++ " elements, fold terminated without"
                             ++ " consuming any elements"

    step (Tuple' i r) a
        | i1 < cnt = do
            res <- fstep r a
            return
                $ case res of
                    FL.Partial s -> Continue 0 $ Tuple' i1 s
                    FL.Done _ ->
                        Error
                            $ "takeEQ: Expecting exactly " ++ show cnt
                                ++ " elements, fold terminated on " ++ show i1
        | i1 == cnt = do
            res <- fstep r a
            Done 0
                <$> case res of
                        FL.Partial s -> fextract s
                        FL.Done b -> return b
        -- XXX we should not reach here when initial returns Step type
        -- reachable only when n == 0
        | otherwise = Done 1 <$> fextract r

        where

        i1 = i + 1

    extract (Tuple' i r)
        | i == 0 && cnt == 0 = fextract r
        | otherwise =
            throwM
                $ ParseError
                $ "takeEQ: Expecting exactly " ++ show cnt
                    ++ " elements, input terminated on " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeGE'.
--
-- /Pre-release/
--
{-# INLINE takeGE #-}
takeGE :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeGE n (Fold fstep finitial fextract) = Parser step initial extract

    where

    cnt = max n 0
    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial $ Tuple' 0 s
            FL.Done b ->
                if cnt == 0
                then IDone b
                else IError
                         $ "takeGE: Expecting at least " ++ show cnt
                             ++ " elements, fold terminated without"
                             ++ " consuming any elements"

    step (Tuple' i r) a
        | i1 < cnt = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Continue 0 $ Tuple' i1 s
                      FL.Done _ ->
                        Error
                            $ "takeGE: Expecting at least " ++ show cnt
                                ++ " elements, fold terminated on " ++ show i1
        | otherwise = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Partial 0 $ Tuple' i1 s
                      FL.Done b -> Done 0 b

        where

        i1 = i + 1

    extract (Tuple' i r)
        | i >= cnt = fextract r
        | otherwise =
            throwM
                $ ParseError
                $ "takeGE: Expecting at least " ++ show cnt
                    ++ " elements, input terminated on " ++ show i

-- | See 'Streamly.Internal.Data.Parser.takeWhile'.
--
-- /Pre-release/
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial s
            FL.Done b -> IDone b

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      FL.Partial s1 -> Partial 0 s1
                      FL.Done b -> Done 0 b
        else Done 1 <$> fextract s

-- | See 'Streamly.Internal.Data.Parser.takeWhile1'.
--
-- /Pre-release/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadThrow m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial (Left s)
            FL.Done _ ->
                IError
                    $ "takeWhile1: fold terminated without consuming:"
                          ++ " any element"

    {-# INLINE process #-}
    process s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 (Right s1)
                  FL.Done b -> Done 0 b

    step (Left s) a =
        if predicate a
        then process s a
        else return $ Error "takeWhile1: predicate failed on first element"
    step (Right s) a =
        if predicate a
        then process s a
        else do
            b <- fextract s
            return $ Done 1 b

    extract (Left _) = throwM $ ParseError "takeWhile1: end of input"
    extract (Right s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.sliceSepByP'.
--
-- /Pre-release/
--
sliceSepByP :: MonadCatch m =>
    (a -> Bool) -> Parser m a b -> Parser m a b
sliceSepByP cond (Parser pstep pinitial pextract) =

    Parser step initial pextract

    where

    initial = pinitial

    step s a =
        if cond a
        then do
            res <- pextract s
            return $ Done 0 res
        else pstep s a

-- | See 'Streamly.Internal.Data.Parser.sliceBeginWith'.
--
-- /Pre-release/
--
data SliceBeginWithState s = Left' s | Right' s

{-# INLINE sliceBeginWith #-}
sliceBeginWith :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceBeginWith cond (Fold fstep finitial fextract) =

    Parser step initial extract

    where

    initial =  do
        res <- finitial
        return $
            case res of
                FL.Partial s -> IPartial (Left' s)
                FL.Done _ -> IError "sliceBeginWith : bad finitial"

    {-# INLINE process #-}
    process s a = do
        res <- fstep s a
        return
            $ case res of
                FL.Partial s1 -> Partial 0 (Right' s1)
                FL.Done b -> Done 0 b

    step (Left' s) a =
        if cond a
        then process s a
        else error $ "sliceBeginWith : slice begins with an element which "
                        ++ "fails the predicate"
    step (Right' s) a =
        if not (cond a)
        then process s a
        else Done 1 <$> fextract s

    extract (Left' s) = fextract s
    extract (Right' s) = fextract s

data WordByState s b = WBLeft !s | WBWord !s | WBRight !b

-- | See 'Streamly.Internal.Data.Parser.wordBy'.
--
--
{-# INLINE wordBy #-}
wordBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
wordBy predicate (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE worder #-}
    worder s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 $ WBWord s1
                  FL.Done b -> Done 0 b

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ WBLeft s
                  FL.Done b -> IDone b

    step (WBLeft s) a =
        if not (predicate a)
        then worder s a
        else return $ Partial 0 $ WBLeft s
    step (WBWord s) a =
        if not (predicate a)
        then worder s a
        else do
            b <- fextract s
            return $ Partial 0 $ WBRight b
    step (WBRight b) a =
        return
            $ if not (predicate a)
              then Done 1 b
              else Partial 0 $ WBRight b

    extract (WBLeft s) = fextract s
    extract (WBWord s) = fextract s
    extract (WBRight b) = return b

{-# ANN type GroupByState Fuse #-}
data GroupByState a s
    = GroupByInit !s
    | GroupByGrouping !a !s

-- | See 'Streamly.Internal.Data.Parser.groupBy'.
--
{-# INLINE groupBy #-}
groupBy :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy eq (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a0 a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a0 s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ GroupByInit s
                  FL.Done b -> IDone b

    step (GroupByInit s) a = grouper s a a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a0 a
        else Done 1 <$> fextract s

    extract (GroupByInit s) = fextract s
    extract (GroupByGrouping _ s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.groupByRolling'.
--
{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupByRolling eq (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ GroupByInit s
                  FL.Done b -> IDone b

    step (GroupByInit s) a = grouper s a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a
        else Done 1 <$> fextract s

    extract (GroupByInit s) = fextract s
    extract (GroupByGrouping _ s) = fextract s

{-# ANN type GroupByStatePair Fuse #-}
data GroupByStatePair a s1 s2
    = GroupByInitPair !s1 !s2
    | GroupByGroupingPair !a !s1 !s2
    | GroupByGroupingPairL !a !s1 !s2
    | GroupByGroupingPairR !a !s1 !s2

{-# INLINE groupByRollingEither #-}
groupByRollingEither :: MonadCatch m =>
    (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (Either b c)
groupByRollingEither
    eq
    (Fold fstep1 finitial1 fextract1)
    (Fold fstep2 finitial2 fextract2) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s1 s2 a = do
        return $ Continue 0 (GroupByGroupingPair a s1 s2)

    {-# INLINE grouperL2 #-}
    grouperL2 s1 s2 a = do
        res <- fstep1 s1 a
        return
            $ case res of
                FL.Done b -> Done 0 (Left b)
                FL.Partial s11 -> Partial 0 (GroupByGroupingPairL a s11 s2)

    {-# INLINE grouperL #-}
    grouperL s1 s2 a0 a = do
        res <- fstep1 s1 a0
        case res of
            FL.Done b -> return $ Done 0 (Left b)
            FL.Partial s11 -> grouperL2 s11 s2 a

    {-# INLINE grouperR2 #-}
    grouperR2 s1 s2 a = do
        res <- fstep2 s2 a
        return
            $ case res of
                FL.Done b -> Done 0 (Right b)
                FL.Partial s21 -> Partial 0 (GroupByGroupingPairR a s1 s21)

    {-# INLINE grouperR #-}
    grouperR s1 s2 a0 a = do
        res <- fstep2 s2 a0
        case res of
            FL.Done b -> return $ Done 0 (Right b)
            FL.Partial s21 -> grouperR2 s1 s21 a

    initial = do
        res1 <- finitial1
        res2 <- finitial2
        return
            $ case res1 of
                FL.Partial s1 ->
                    case res2 of
                        FL.Partial s2 -> IPartial $ GroupByInitPair s1 s2
                        FL.Done b -> IDone (Right b)
                FL.Done b -> IDone (Left b)

    step (GroupByInitPair s1 s2) a = grouper s1 s2 a

    step (GroupByGroupingPair a0 s1 s2) a =
        if not (eq a0 a)
        then grouperL s1 s2 a0 a
        else grouperR s1 s2 a0 a

    step (GroupByGroupingPairL a0 s1 s2) a =
        if not (eq a0 a)
        then grouperL2 s1 s2 a
        else Done 1 . Left <$> fextract1 s1

    step (GroupByGroupingPairR a0 s1 s2) a =
        if eq a0 a
        then grouperR2 s1 s2 a
        else Done 1 . Right <$> fextract2 s2

    extract (GroupByInitPair s1 _) = Left <$> fextract1 s1
    extract (GroupByGroupingPairL _ s1 _) = Left <$> fextract1 s1
    extract (GroupByGroupingPairR _ _ s2) = Right <$> fextract2 s2
    extract (GroupByGroupingPair a s1 _) = do
                res <- fstep1 s1 a
                case res of
                    FL.Done b -> return $ Left b
                    FL.Partial s11 -> Left <$> fextract1 s11

-- XXX use an Unfold instead of a list?
-- XXX custom combinators for matching list, array and stream?
--
-- | See 'Streamly.Internal.Data.Parser.eqBy'.
--
-- /Pre-release/
--
{-# INLINE eqBy #-}
eqBy :: MonadThrow m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp str = Parser step initial extract

    where

    initial = return $ IPartial str

    step [] _ = return $ Done 0 ()
    step [x] a =
        return
            $ if x `cmp` a
              then Done 0 ()
              else Error "eqBy: failed, yet to match the last element"
    step (x:xs) a =
        return
            $ if x `cmp` a
              then Continue 0 xs
              else Error
                       $ "eqBy: failed, yet to match "
                       ++ show (length xs + 1) ++ " elements"

    extract xs =
        throwM
            $ ParseError
            $ "eqBy: end of input, yet to match "
            ++ show (length xs) ++ " elements"

--------------------------------------------------------------------------------
--- Spanning
--------------------------------------------------------------------------------

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- @
-- > let span_ p xs = Stream.parse (Parser.span p Fold.toList Fold.toList) $ Stream.fromList xs
--
-- > span_ (< 1) [1,2,3]
-- ([],[1,2,3])
--
-- > span_ (< 2) [1,2,3]
-- ([1],[2,3])
--
-- > span_ (< 4) [1,2,3]
-- ([1,2,3],[])
--
-- @
--
-- /Pre-release/
{-# INLINE span #-}
span :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (b, c)
span p f1 f2 = noErrorUnsafeSplitWith (,) (takeWhile p f1) (fromFold f2)

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
-- /Pre-release/
--
{-# INLINE spanBy #-}
spanBy ::
       Monad m
    => (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (b, c)
spanBy eq f1 f2 = noErrorUnsafeSplitWith (,) (groupBy eq f1) (fromFold f2)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
--
-- /Pre-release/
{-# INLINE spanByRolling #-}
spanByRolling ::
       Monad m
    => (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (b, c)
spanByRolling eq f1 f2 =
    noErrorUnsafeSplitWith (,) (groupByRolling eq f1) (fromFold f2)

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.takeP'.
--
-- /Internal/
{-# INLINE takeP #-}
takeP :: Monad m => Int -> Parser m a b -> Parser m a b
takeP lim (Parser pstep pinitial pextract) = Parser step initial extract

    where

    initial = do
        res <- pinitial
        case res of
            IPartial s ->
                if lim > 0
                then return $ IPartial $ Tuple' 0 s
                else IDone <$> pextract s
            IDone b -> return $ IDone b
            IError e -> return $ IError e

    step (Tuple' cnt r) a = do
        assert (cnt < lim) (return ())
        res <- pstep r a
        let cnt1 = cnt + 1
        case res of
            Partial 0 s -> do
                assert (cnt1 >= 0) (return ())
                if cnt1 < lim
                then return $ Partial 0 $ Tuple' cnt1 s
                else Done 0 <$> pextract s
            Continue 0 s -> do
                assert (cnt1 >= 0) (return ())
                if cnt1 < lim
                then return $ Continue 0 $ Tuple' cnt1 s
                -- XXX This should error out?
                -- If designed properly, this will probably error out.
                -- "pextract" should error out
                --
                -- By Harendra,
                --
                -- This is a tricky case, we have the following options:
                --   1. Done 0 with extract as you have written
                --   2. Done n, will require buffering elements
                --   3. Use a backtracking fold and not a parser, once we have
                --      backtracking in folds
                else Done 0 <$> pextract s
            Partial n s -> do
                let taken = cnt1 - n
                assert (taken >= 0) (return ())
                return $ Partial n $ Tuple' taken s
            Continue n s -> do
                let taken = cnt1 - n
                assert (taken >= 0) (return ())
                return $ Continue n $ Tuple' taken s
            Done n b -> return $ Done n b
            Error str -> return $ Error str

    extract (Tuple' _ r) = pextract r

-- | See 'Streamly.Internal.Data.Parser.lookahead'.
--
-- /Pre-release/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Parser m a b -> Parser m a b
lookAhead (Parser step1 initial1 _) = Parser step initial extract

    where

    initial = do
        res <- initial1
        return $ case res of
            IPartial s -> IPartial (Tuple' 0 s)
            IDone b -> IDone b
            IError e -> IError e

    step (Tuple' cnt st) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        return
            $ case r of
                  Partial n s -> Continue n (Tuple' (cnt1 - n) s)
                  Continue n s -> Continue n (Tuple' (cnt1 - n) s)
                  Done _ b -> Done cnt1 b
                  Error err -> Error err

    -- XXX returning an error let's us backtrack.  To implement it in a way so
    -- that it terminates on eof without an error then we need a way to
    -- backtrack on eof, that will require extract to return 'Step' type.
    extract (Tuple' n _) =
        throwM
            $ ParseError
            $ "lookAhead: end of input after consuming "
            ++ show n ++ " elements"

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.deintercalate'.
--
-- /Unimplemented/
--
{-# INLINE deintercalate #-}
deintercalate ::
    -- Monad m =>
       Fold m a y -> Parser m x a
    -> Fold m b z -> Parser m x b
    -> Parser m x (y, z)
deintercalate = undefined

-------------------------------------------------------------------------------
-- Sequential Collection
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.sequence'.
--
-- /Unimplemented/
--
{-# INLINE sequence #-}
sequence ::
    -- Foldable t =>
    Fold m b c -> t (Parser m a b) -> Parser m a c
sequence _f _p = undefined

-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.choice'.
--
-- /Broken/
--
{-# INLINE choice #-}
choice :: (MonadCatch m, Foldable t) => t (Parser m a b) -> Parser m a b
choice = foldl1 shortest

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.many'.
--
-- /Pre-release/
--
{-# INLINE many #-}
many :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
many = splitMany
-- many = countBetween 0 maxBound

-- | See 'Streamly.Internal.Data.Parser.some'.
--
-- /Pre-release/
--
{-# INLINE some #-}
some :: MonadCatch m => Parser m a b -> Fold m b c -> Parser m a c
some = splitSome
-- some f p = many (takeGE 1 f) p
-- many = countBetween 1 maxBound

-- | See 'Streamly.Internal.Data.Parser.countBetween'.
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween ::
    -- MonadCatch m =>
    Int -> Int -> Parser m a b -> Fold m b c -> Parser m a c
countBetween _m _n _p = undefined
-- countBetween m n p f = many (takeBetween m n f) p

-- | See 'Streamly.Internal.Data.Parser.count'.
--
-- /Unimplemented/
--
{-# INLINE count #-}
count ::
    -- MonadCatch m =>
    Int -> Parser m a b -> Fold m b c -> Parser m a c
count n = countBetween n n
-- count n f p = many (takeEQ n f) p

data ManyTillState fs sr sl
    = ManyTillR Int fs sr
    | ManyTillL Int fs sl

-- | See 'Streamly.Internal.Data.Parser.manyTill'.
--
-- /Pre-release/
--
{-# INLINE manyTill #-}
manyTill :: MonadCatch m
    => Fold m b c -> Parser m a b -> Parser m a x -> Parser m a c
manyTill (Fold fstep finitial fextract)
         (Parser stepL initialL extractL)
         (Parser stepR initialR _) =
    Parser step initial extract

    where

    -- Caution: Mutual recursion

    scrutL fs p c d e = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ c (ManyTillL 0 fs sl)
            IDone bl -> do
                fr <- fstep fs bl
                case fr of
                    FL.Partial fs1 -> scrutR fs1 p c d e
                    FL.Done fb -> return $ d fb
            IError err -> return $ e err

    scrutR fs p c d e = do
        resR <- initialR
        case resR of
            IPartial sr -> return $ p (ManyTillR 0 fs sr)
            IDone _ -> d <$> fextract fs
            IError _ -> scrutL fs p c d e

    initial = do
        res <- finitial
        case res of
            FL.Partial fs -> scrutR fs IPartial IPartial IDone IError
            FL.Done b -> return $ IDone b

    step (ManyTillR cnt fs st) a = do
        r <- stepR st a
        case r of
            Partial n s -> return $ Partial n (ManyTillR 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillR (cnt + 1 - n) fs s)
            Done n _ -> do
                b <- fextract fs
                return $ Done n b
            Error _ -> do
                resL <- initialL
                case resL of
                    IPartial sl ->
                        return $ Continue (cnt + 1) (ManyTillL 0 fs sl)
                    IDone bl -> do
                        fr <- fstep fs bl
                        let cnt1 = cnt + 1
                            p = Partial cnt
                            c = Continue cnt
                            d = Done cnt
                        case fr of
                            FL.Partial fs1 -> scrutR fs1 p c d Error
                            FL.Done fb -> return $ Done cnt1 fb
                    IError err -> return $ Error err
    -- XXX the cnt is being used only by the assert
    step (ManyTillL cnt fs st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ManyTillL 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillL (cnt + 1 - n) fs s)
            Done n b -> do
                fs1 <- fstep fs b
                case fs1 of
                    FL.Partial s ->
                        scrutR s (Partial n) (Continue n) (Done n) Error
                    FL.Done b1 -> return $ Done n b1
            Error err -> return $ Error err

    extract (ManyTillL _ fs sR) = do
        res <- extractL sR >>= fstep fs
        case res of
            FL.Partial s -> fextract s
            FL.Done b -> return b
    extract (ManyTillR _ fs _) = fextract fs
