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

    -- First order parsers
    -- * Accumulators
    , fromFold
    , any
    , all
    , yield
    , yieldM
    , die
    , dieM

    -- * Element parsers
    , peek
    , eof
    , satisfy
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
    , take
    -- , takeBetween
    -- , takeLE -- take   -- takeBetween 0 n
    -- , takeLE1 -- take1 -- takeBetween 1 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , lookAhead
    , takeWhile
    , takeWhile1
    , sliceSepBy
    , sliceSepByMax
    -- , sliceSepByBetween
    , sliceEndWith
    , sliceBeginWith
    -- , sliceSepWith
    --
    -- , frameSepBy -- parse frames escaped by an escape char/sequence
    -- , frameEndWith
    --
    , wordBy
    , groupBy
    , eqBy
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- ** Sequential Applicative
    , splitWith
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

import Control.Exception (assert)
import Control.Monad.Catch (MonadCatch, MonadThrow(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Fold as FL

import Prelude hiding
       (any, all, take, takeWhile, sequence, concatMap, maybe, either)
import Streamly.Internal.Data.Parser.ParserD.Tee
import Streamly.Internal.Data.Parser.ParserD.Types

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.fromFold'.
--
-- /Internal/
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser m a b
fromFold (Fold fstep finitial fextract) = Parser step finitial fextract

    where

    step s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 s1
                  FL.Done b -> Done 0 b


-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> Parser m a Bool
any predicate = fromFold $ FL.any predicate

{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parser m a Bool
all predicate = fromFold $ FL.all predicate

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.peek'.
--
-- /Internal/
--
{-# INLINABLE peek #-}
peek :: MonadThrow m => Parser m a a
peek = Parser step initial extract

    where

    initial = return ()

    step () a = return $ Done 1 a

    extract () = throwM $ ParseError "peek: end of input"

-- | See 'Streamly.Internal.Data.Parser.eof'.
--
-- /Internal/
--
{-# INLINABLE eof #-}
eof :: Monad m => Parser m a ()
eof = Parser step initial return

    where

    initial = return ()

    step () _ = return $ Error "eof: not at end of input"

-- | See 'Streamly.Internal.Data.Parser.satisfy'.
--
-- /Internal/
--
{-# INLINE satisfy #-}
satisfy :: MonadThrow m => (a -> Bool) -> Parser m a a
satisfy predicate = Parser step initial extract

    where

    initial = return ()

    step () a = return $
        if predicate a
        then Done 0 a
        else Error "satisfy: predicate failed"

    extract _ = throwM $ ParseError "satisfy: end of input"

-- | See 'Streamly.Internal.Data.Parser.maybe'.
--
-- /Internal/
--
{-# INLINE maybe #-}
maybe :: MonadThrow m => (a -> Maybe b) -> Parser m a b
maybe parser = Parser step initial extract

    where

    initial = return ()

    step () a = return $
        case parser a of
            Just b -> Done 0 b
            Nothing -> Error "maybe: predicate failed"

    extract _ = throwM $ ParseError "maybe: end of input"

-- | See 'Streamly.Internal.Data.Parser.either'.
--
-- /Internal/
--
{-# INLINE either #-}
either :: MonadThrow m => (a -> Either String b) -> Parser m a b
either parser = Parser step initial extract

    where

    initial = return ()

    step () a = return $
        case parser a of
            Right b -> Done 0 b
            Left err -> Error $ "either: " ++ err

    extract _ = throwM $ ParseError "either: end of input"

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- It will be inconsistent with other takeish combinators.
-- This is takeLE
-- | See 'Streamly.Internal.Data.Parser.take'.
--
-- /Internal/
--
{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Parser m a b
take n fld = fromFold $ FL.ltake n fld

-- XXX We are purposefully ignoring the input here
-- | See 'Streamly.Internal.Data.Parser.takeEQ'.
--
-- /Internal/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeEQ cnt (Fold fstep finitial fextract) = Parser step initial extract

    where

    n = max cnt 0

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a
        | i < n = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Continue 0 $ Tuple' (i + 1) s
                      FL.Done _ -> Error $ err (i + 1)
        | otherwise = Done 1 <$> fextract r

    extract (Tuple' i r)
        | i == n = fextract r
        | otherwise = throwM $ ParseError $ err i

    err i =
        "takeEQ: Expecting exactly " ++ show n ++ " elements, got " ++ show i


-- | See 'Streamly.Internal.Data.Parser.takeGE'.
--
-- /Internal/
--
{-# INLINE takeGE #-}
takeGE :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeGE cnt (Fold fstep finitial fextract) = Parser step initial extract

    where

    n = max cnt 0
    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a
        | i < n = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Continue 0 $ Tuple' (i + 1) s
                      FL.Done _ -> Error $ err (i + 1)
        | otherwise = do
            res <- fstep r a
            return
                $ case res of
                      FL.Partial s -> Partial 0 $ Tuple' (i + 1) s
                      FL.Done b -> Done 0 b

    extract (Tuple' i b)
        | i >= n = fextract b
        | otherwise = throwM $ ParseError $ err i

    err i =
        "takeGE: Expecting at least "
            ++ show n ++ " elements, got only " ++ show i


-- | See 'Streamly.Internal.Data.Parser.takeWhile'.
--
-- /Internal/
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial

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
-- /Internal/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadThrow m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 predicate (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = return Nothing

    step Nothing a =
        if predicate a
        then do
            s <- finitial
            sr <- fstep s a
            return
                $ case sr of
                      FL.Partial r -> Partial 0 (Just r)
                      FL.Done b -> Done 0 b
        else return $ Error err
    step (Just s) a =
        if predicate a
        then do
            sr <- fstep s a
            case sr of
                FL.Partial r -> return $ Partial 0 (Just r)
                FL.Done b -> return $ Done 0 b
        else do
            b <- fextract s
            return $ Done 1 b

    extract Nothing = throwM $ ParseError err
    extract (Just s) = fextract s

    err = "takeWhile1: end of input"

-- | See 'Streamly.Internal.Data.Parser.sliceSepBy'.
--
-- /Internal/
--
{-# INLINABLE sliceSepBy #-}
sliceSepBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceSepBy predicate fld = fromFold $ FL.sliceSepBy predicate fld

-- | See 'Streamly.Internal.Data.Parser.sliceEndWith'.
--
-- /Unimplemented/
--
{-# INLINABLE sliceEndWith #-}
sliceEndWith ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceEndWith = undefined

-- | See 'Streamly.Internal.Data.Parser.sliceBeginWith'.
--
-- /Unimplemented/
--
{-# INLINABLE sliceBeginWith #-}
sliceBeginWith ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceBeginWith = undefined

-- | See 'Streamly.Internal.Data.Parser.sliceSepByMax'.
--
-- /Internal/
--
{-# INLINABLE sliceSepByMax #-}
sliceSepByMax :: Monad m
    => (a -> Bool) -> Int -> Fold m a b -> Parser m a b
sliceSepByMax p n = sliceSepBy p . FL.ltake n

-- | See 'Streamly.Internal.Data.Parser.wordBy'.
--
-- /Unimplemented/
--
{-# INLINABLE wordBy #-}
wordBy ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
wordBy = undefined

-- | See 'Streamly.Internal.Data.Parser.groupBy'.
--
-- /Unimplemented/
--
{-# INLINABLE groupBy #-}
groupBy ::
    -- Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy = undefined

-- XXX use an Unfold instead of a list?
-- XXX custom combinators for matching list, array and stream?
--
-- | See 'Streamly.Internal.Data.Parser.eqBy'.
--
-- /Internal/
--
{-# INLINE eqBy #-}
eqBy :: MonadThrow m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp str = Parser step initial extract

    where

    initial = return str

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

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | See 'Streamly.Internal.Data.Parser.lookahead'.
--
-- /Internal/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Parser m a b -> Parser m a b
lookAhead (Parser step1 initial1 _) = Parser step initial extract

    where

    initial = Tuple' 0 <$> initial1

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
--
-- | See 'Streamly.Internal.Data.Parser.choice'.
--
-- /Unimplemented/
--
{-# INLINE choice #-}
choice ::
    -- Foldable t =>
    t (Parser m a b) -> Parser m a b
choice _ps = undefined

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------
--
-- | See 'Streamly.Internal.Data.Parser.many'.
--
-- /Internal/
--
{-# INLINE many #-}
many :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
many = splitMany
-- many = countBetween 0 maxBound

-- | See 'Streamly.Internal.Data.Parser.some'.
--
-- /Internal/
--
{-# INLINE some #-}
some :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
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
    Int -> Int -> Fold m b c -> Parser m a b -> Parser m a c
countBetween _m _n _f = undefined
-- countBetween m n f p = many (takeBetween m n f) p

-- | See 'Streamly.Internal.Data.Parser.count'.
--
-- /Unimplemented/
--
{-# INLINE count #-}
count ::
    -- MonadCatch m =>
    Int -> Fold m b c -> Parser m a b -> Parser m a c
count n = countBetween n n
-- count n f p = many (takeEQ n f) p

data ManyTillState fs sr sl
    = ManyTillR Int fs sr
    | ManyTillL Int fs sl

-- | See 'Streamly.Internal.Data.Parser.manyTill'.
--
-- /Internal/
--
{-# INLINE manyTill #-}
manyTill :: MonadCatch m
    => Fold m b c -> Parser m a b -> Parser m a x -> Parser m a c
manyTill (Fold fstep finitial fextract)
         (Parser stepL initialL extractL)
         (Parser stepR initialR _) =
    Parser step initial extract

    where

    initial = ManyTillR 0 <$> finitial <*> initialR

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
                rR <- initialL
                return $ Continue (cnt + 1) (ManyTillL 0 fs rR)
    step (ManyTillL cnt fs st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ManyTillL 0 fs s)
            Continue n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Continue n (ManyTillL (cnt + 1 - n) fs s)
            Done n b -> do
                sfs1 <- fstep fs b
                case sfs1 of
                    FL.Partial fs1 -> do
                        l <- initialR
                        return $ Partial n (ManyTillR 0 fs1 l)
                    FL.Done fb -> return $ Done n fb
            Error err -> return $ Error err

    extract (ManyTillL _ fs sR) = do
        res <- extractL sR >>= fstep fs
        case res of
            FL.Partial sres -> fextract sres
            FL.Done bres -> return bres
    extract (ManyTillR _ fs _) = fextract fs
