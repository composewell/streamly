{-# LANGUAGE CPP                       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.Internal.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast streaming parsers.
--
-- 'Applicative' and 'Alternative' type class based combinators from the
-- <http://hackage.haskell.org/package/parser-combinators parser-combinators>
-- package can also be used with the 'Parser' type. However, there are two
-- important differences between @parser-combinators@ and the equivalent ones
-- provided in this module in terms of performance:
--
-- 1) @parser-combinators@ use plain Haskell lists to collect the results, in a
-- strict Monad like IO, the results are necessarily buffered before they can
-- be consumed.  This may not perform optimally in streaming applications
-- processing large amounts of data.  Equivalent combinators in this module can
-- consume the results of parsing using a 'Fold', thus providing a scalability
-- and a generic consumer.
--
-- 2) Several combinators in this module can be many times faster because of
-- stream fusion. For example, 'Streamly.Internal.Data.Parser.many' combinator
-- in this module is much faster than the 'Control.Applicative.many' combinator
-- of 'Alternative' type class.
--
-- Failing parsers in this module throw the 'ParseError' exception.

-- XXX As far as possible, try that the combinators in this module and in
-- "parser-combinators/parsec/megaparsec/attoparsec" have consistent names.
-- takeP/takeWhileP?

module Streamly.Internal.Data.Parser
    (
      Parser (..)

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

    -- * Sequence parsers
    -- Grab a sequence of input elements without inspecting them
    , take
    -- , takeBetween
    -- , takeLE -- take   -- takeBetween 0 n
    -- , takeLE1 -- take1 -- takeBetween 1 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , takeWhile
    -- , takeWhileBetween
    , sepBy
    , sepByMax
    -- , sepByBetween
    , sepWithSuffix
    , sepWithPrefix
    , wordBy
    , groupBy

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- Parallel parsers
    , teeWith
    , teeWithFst
    , teeWithMin

    -- Alternatives
    , shortest
    , longest
    -- , fastest

    -- * N-ary Combinators
    -- N-ary split applicative
    -- Run n actions in sequence
    , sequence

    -- Repeat the same action in sequence
    -- , splitLE
    -- , splitEQ -- splitN/takeN/parseN = replicateM n
    -- , splitGE -- splitEQ n, splitMany
    , count
    , countBetween
    , many -- splitMany/takeMany/parseMany
           -- = splitGE 0 = countBetween 0 maxBound
    , some -- splitMany1/takeMany1/parseMany1
           -- = splitGE 1 = countBetween 1 maxBound

    -- , countBetweenTill
    , manyTill -- splitBreakOn, manyTill, parseBreakOn

    -- Run n actions in parallel
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    , choice   -- first successful in position
    -- , choiceN  -- first N successful
    )
where

import Control.Exception (assert)
import Control.Monad.Catch (MonadCatch, MonadThrow(..))
import Prelude
       hiding (any, all, take, takeWhile, sequence)

import Streamly.Internal.Data.Fold.Types (Fold(..))

import Streamly.Internal.Data.Parser.Tee
import Streamly.Internal.Data.Parser.Types
import Streamly.Internal.Data.Strict

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | The resulting parse never terminates and never errors out.
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser m a b
fromFold (Fold fstep finitial fextract) = Parser step finitial fextract

    where

    step s a = Yield 0 <$> fstep s a

-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
-- |
-- >>> S.parse (PR.any (== 0)) $ S.fromList [1,0,1]
-- > Right True
--
{-# INLINABLE any #-}
any :: Monad m => (a -> Bool) -> Parser m a Bool
any predicate = Parser step initial return

    where

    initial = return False

    step s a = return $
        if s
        then Stop 0 True
        else
            if predicate a
            then Stop 0 True
            else Yield 0 False

-- |
-- >>> S.parse (PR.all (== 0)) $ S.fromList [1,0,1]
-- > Right False
--
{-# INLINABLE all #-}
all :: Monad m => (a -> Bool) -> Parser m a Bool
all predicate = Parser step initial return

    where

    initial = return True

    step s a = return $
        if s
        then
            if predicate a
            then Yield 0 True
            else Stop 0 False
        else Stop 0 False

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | Peek the head element of a stream, without consuming it. Fails if it
-- encounters end of input.
--
-- >>> S.parse ((,) <$> PR.peek <*> PR.satisfy (> 0)) $ S.fromList [1]
-- (1,1)
--
-- /Internal/
--
{-# INLINABLE peek #-}
peek :: MonadThrow m => Parser m a a
peek = Parser step initial extract

    where

    initial = return ()

    step () a = return $ Stop 1 a

    extract () = throwM $ ParseError "peek: end of input"

-- | Succeeds if we are at the end of input, fails otherwise.
--
-- >>> S.parse ((,) <$> PR.satisfy (> 0) <*> PR.eof) $ S.fromList [1]
-- > (1,())
--
-- /Internal/
--
{-# INLINABLE eof #-}
eof :: Monad m => Parser m a ()
eof = Parser step initial return

    where

    initial = return ()

    step () _ = return $ Error "eof: not at end of input"

-- | Returns the next element if it passes the predicate, fails otherwise.
--
-- >>> S.parse (PR.satisfy (== 1)) $ S.fromList [1,0,1]
-- > 1
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
        then Stop 0 a
        else Error "satisfy: predicate failed"

    extract _ = throwM $ ParseError "satisfy: end of input"

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------
--
-- XXX Once we have terminating folds, this Parse should get replaced by Fold.
-- Alternatively, we can name it "chunkOf" and the corresponding time domain
-- combinator as "intervalOf" or even "chunk" and "interval".
--
-- | Take at most @n@ input elements and fold them using the supplied fold.
--
-- Stops after @n@ elements.
-- Never fails.
--
-- >>> S.parse (PR.take 1 FL.toList) $ S.fromList [1]
-- [1]
--
-- @
-- S.chunksOf n f = S.splitParse (FL.take n f)
-- @
--
-- /Internal/
--
{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Parser m a b
take n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        if i1 < n
        then return $ Yield 0 s1
        else Stop 0 <$> fextract res

    extract (Tuple' _ r) = fextract r

--
-- XXX can we use a "cmp" operation in a common implementation?
--
-- | Stops after taking exactly @n@ input elements.
--
-- * Stops - after @n@ elements.
-- * Fails - if the stream ends before it can collect @n@ elements.
--
-- >>> S.parse (PR.takeEQ 4 FL.toList) $ S.fromList [1,0,1]
-- > "takeEQ: Expecting exactly 4 elements, got 3"
--
-- /Internal/
--
{-# INLINE takeEQ #-}
takeEQ :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeEQ n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        if i1 < n then return (Skip 0 s1) else Stop 0 <$> fextract res

    extract (Tuple' i r) =
        if n == i
        then fextract r
        else throwM $ ParseError err

        where

        err =
               "takeEQ: Expecting exactly " ++ show n
            ++ " elements, got " ++ show i

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - never.
-- * Fails - if the stream ends before producing @n@ elements.
--
-- >>> S.parse (PR.takeGE 4 FL.toList) $ S.fromList [1,0,1]
-- > "takeGE: Expecting at least 4 elements, got only 3"
--
-- >>> S.parse (PR.takeGE 4 FL.toList) $ S.fromList [1,0,1,0,1]
-- > [1,0,1,0,1]
--
-- /Internal/
--
{-# INLINE takeGE #-}
takeGE :: MonadThrow m => Int -> Fold m a b -> Parser m a b
takeGE n (Fold fstep finitial fextract) = Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial

    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        return $
            if i1 < n
            then Skip 0 s1
            else Yield 0 s1

    extract (Tuple' i r) = fextract r >>= f

        where

        err =
              "takeGE: Expecting at least " ++ show n
           ++ " elements, got only " ++ show i

        f x =
            if i >= n
            then return x
            else throwM $ ParseError err

-- | Collect stream elements until an element fails the predicate. The element
-- on which the predicate fails is returned back to the input stream.
--
-- * Stops - when the predicate fails.
-- * Fails - never.
--
-- >>> S.parse (PR.takeWhile (== 0) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [0,0]
--
-- We can implement a @breakOn@ using 'takeWhile':
--
-- @
-- breakOn p = takeWhile (not p)
-- @
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
        then Yield 0 <$> fstep s a
        else Stop 1 <$> fextract s

-- | Collect stream elements until an element succeeds the predicate. Drop the
-- element on which the predicate succeeded. The succeeding element is treated
-- as an infix separator which is dropped from the output.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- >>> S.parse (PR.sepBy (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [0,0]
--
-- S.splitOn pred f = S.splitParse (PR.sepBy pred f)
--
-- >>> S.toList $ S.splitParse (PR.sepBy (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [[0,0],[0],[]]
--
-- /Internal/
--
{-# INLINABLE sepBy #-}
sepBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sepBy predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = finitial
    step s a =
        if not (predicate a)
        then Yield 0 <$> fstep s a
        else Stop 0 <$> fextract s

-- | Collect stream elements until an element succeeds the predicate. Also take
-- the element on which the predicate succeeded. The succeeding element is
-- treated as a suffix separator which is kept in the output segement.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- S.splitWithSuffix pred f = S.splitParse (PR.sepWithSuffix pred f)
--
-- /Unimplemented/
--
{-# INLINABLE sepWithSuffix #-}
sepWithSuffix ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sepWithSuffix = undefined

-- | Collect stream elements until an elements passes the predicate, return the
-- last element on which the predicate succeeded back to the input stream.  If
-- the predicate succeeds on the first element itself then it is kept in the
-- stream and we continue collecting. The succeeding element is treated as a
-- prefix separator which is kept in the output segement.
--
-- * Stops - when the predicate succeeds in non-leading position.
-- * Fails - never.
--
-- S.splitWithPrefix pred f = S.splitParse (PR.sepWithPrefix pred f)
--
-- /Unimplemented/
--
{-# INLINABLE sepWithPrefix #-}
sepWithPrefix ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sepWithPrefix = undefined

-- | Split using a condition or a count whichever occurs first. This is a
-- hybrid of 'splitOn' and 'take'. The element on which the condition succeeds
-- is dropped.
--
-- /Internal/
--
{-# INLINABLE sepByMax #-}
sepByMax :: Monad m
    => (a -> Bool) -> Int -> Fold m a b -> Parser m a b
sepByMax predicate cnt (Fold fstep finitial fextract) =
    Parser step initial extract

    where

    initial = Tuple' 0 <$> finitial
    step (Tuple' i r) a = do
        res <- fstep r a
        let i1 = i + 1
            s1 = Tuple' i1 res
        if not (predicate a) && i1 < cnt
        then return $ Yield 0 s1
        else do
            b <- fextract res
            return $ Stop 0 b
    extract (Tuple' _ r) = fextract r

-- | Like 'splitOn' but strips leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ having '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- * Stops - when it finds a word separator after a non-word element
-- * Fails - never.
--
-- @
-- S.wordsBy pred f = S.splitParse (PR.wordBy pred f)
-- @
--
-- /Unimplemented/
--
{-# INLINABLE wordBy #-}
wordBy ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
wordBy = undefined

-- | @groupBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, then if @a \`cmp` b@ is 'True' @b@ is also assigned to the same
-- group.  If @a \`cmp` c@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the 'Fold' @f@ and the result of the fold is emitted
-- in the output stream.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- @
-- S.groupsBy cmp f = S.splitParse (PR.groupBy cmp f)
-- @
--
-- /Unimplemented/
--
{-# INLINABLE groupBy #-}
groupBy ::
    -- Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy = undefined

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | @sequence f t@ collects sequential parses of parsers in the container @t@
-- using the fold @f@. Fails if the input ends or any of the parsers fail.
--
-- /Unimplemented/
--
{-# INLINE sequence #-}
sequence ::
    -- Foldable t =>
    Fold m b c -> t (Parser m a b) -> Parser m a c
sequence _f _p = undefined

-- | @choice parsers@ applies the @parsers@ in order and returns the first
-- successful parse.
--
{-# INLINE choice #-}
choice ::
    -- Foldable t =>
    t (Parser m a b) -> Parser m a b
choice _ps = undefined

-- XXX "many" is essentially a Fold because it cannot fail. So it can be
-- downgraded to a Fold. Or we can make the return type a Fold instead and
-- upgrade that to a parser when needed.
--
-- | Collect zero or more parses. Apply the parser repeatedly on the input
-- stream, stop when the parser fails, accumulate zero or more parse results
-- using the supplied 'Fold'. This parser never fails, in case the first
-- application of parser fails it returns an empty result.
--
-- Compare with 'Control.Applicative.many'.
--
-- /Internal/
--
{-# INLINE many #-}
many :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
many = splitMany

-- | Collect one or more parses. Apply the supplied parser repeatedly on the
-- input stream and accumulate the parse results as long as the parser
-- succeeds, stop when it fails.  This parser fails if not even one result is
-- collected.
--
-- Compare with 'Control.Applicative.some'.
--
-- /Internal/
--
{-# INLINE some #-}
some :: MonadCatch m => Fold m b c -> Parser m a b -> Parser m a c
some = splitSome

-- | @countBetween m n f p@ collects between @m@ and @n@ sequential parses of
-- parser @p@ using the fold @f@. Stop after collecting @n@ results. Fails if
-- the input ends or the parser fails before @m@ results are collected.
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween ::
    -- MonadCatch m =>
    Int -> Int -> Fold m b c -> Parser m a b -> Parser m a c
countBetween _m _n _f = undefined

-- | @count n f p@ collects exactly @n@ sequential parses of parser @p@ using
-- the fold @f@.  Fails if the input ends or the parser fails before @n@
-- results are collected.
--
-- /Unimplemented/
--
{-# INLINE count #-}
count ::
    -- MonadCatch m =>
    Int -> Fold m b c -> Parser m a b -> Parser m a c
count n = countBetween n n

data ManyTillState fs sr sl = ManyTillR Int fs sr | ManyTillL fs sl

-- | @manyTill f collect test@ tries the parser @test@ on the input, if @test@
-- fails it backtracks and tries @collect@, after @collect@ succeeds @test@ is
-- tried again and so on. The parser stops when @test@ succeeds.  The output of
-- @test@ is discarded and the output of @collect@ is accumulated by the
-- supplied fold. The parser fails if @collect@ fails.
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

    initial = do
        fs <- finitial
        ManyTillR 0 fs <$> initialR

    step (ManyTillR cnt fs st) a = do
        r <- stepR st a
        case r of
            Yield n s -> return $ Yield n (ManyTillR 0 fs s)
            Skip n s -> do
                assert (cnt + 1 - n >= 0) (return ())
                return $ Skip n (ManyTillR (cnt + 1 - n) fs s)
            Stop n _ -> do
                b <- fextract fs
                return $ Stop n b
            Error _ -> do
                rR <- initialL
                return $ Skip (cnt + 1) (ManyTillL fs rR)

    step (ManyTillL fs st) a = do
        r <- stepL st a
        case r of
            Yield n s -> return $ Yield n (ManyTillL fs s)
            Skip n s -> return $ Skip n (ManyTillL fs s)
            Stop n b -> do
                fs1 <- fstep fs b
                l <- initialR
                -- XXX we need a yield with backtrack here
                -- return $ Yield n (ManyTillR 0 fs1 l)
                return $ Skip n (ManyTillR 0 fs1 l)
            Error err -> return $ Error err

    extract (ManyTillL fs sR) = extractL sR >>= fstep fs >>= fextract
    extract (ManyTillR _ fs _) = fextract fs
