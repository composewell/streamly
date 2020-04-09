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
-- "Text.ParserCombinators.ReadP/parser-combinators/parsec/megaparsec/attoparsec"
-- have consistent names. takeP/takeWhileP/munch?

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

    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate

    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest

    -- * N-ary Combinators
    -- ** Sequential Collection
    , sequence

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
-- @
-- peek = lookAhead (satisfy True)
-- @
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

-- | Like 'takeWhile' but takes at least one element otherwise fails.
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
            r <- fstep s a
            return $ Yield 0 (Just r)
        else return $ Error "takeWhile1: empty"
    step (Just s) a =
        if predicate a
        then do
            r <- fstep s a
            return $ Yield 0 (Just r)
        else do
            b <- fextract s
            return $ Stop 1 b

    extract Nothing = throwM $ ParseError "takeWhile1: end of input"
    extract (Just s) = fextract s

-- | Collect stream elements until an element succeeds the predicate. Drop the
-- element on which the predicate succeeded. The succeeding element is treated
-- as an infix separator which is dropped from the output.
--
-- * Stops - when the predicate succeeds.
-- * Fails - never.
--
-- >>> S.parse (PR.sliceSepBy (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [0,0]
--
-- S.splitOn pred f = S.splitParse (PR.sliceSepBy pred f)
--
-- >>> S.toList $ S.splitParse (PR.sliceSepBy (== 1) FL.toList) $ S.fromList [0,0,1,0,1]
-- > [[0,0],[0],[]]
--
-- /Internal/
--
{-# INLINABLE sliceSepBy #-}
sliceSepBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceSepBy predicate (Fold fstep finitial fextract) =
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
-- S.splitWithSuffix pred f = S.splitParse (PR.sliceEndWith pred f)
--
-- /Unimplemented/
--
{-# INLINABLE sliceEndWith #-}
sliceEndWith ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceEndWith = undefined

-- | Collect stream elements until an elements passes the predicate, return the
-- last element on which the predicate succeeded back to the input stream.  If
-- the predicate succeeds on the first element itself then it is kept in the
-- stream and we continue collecting. The succeeding element is treated as a
-- prefix separator which is kept in the output segement.
--
-- * Stops - when the predicate succeeds in non-leading position.
-- * Fails - never.
--
-- S.splitWithPrefix pred f = S.splitParse (PR.sliceBeginWith pred f)
--
-- /Unimplemented/
--
{-# INLINABLE sliceBeginWith #-}
sliceBeginWith ::
    -- Monad m =>
    (a -> Bool) -> Fold m a b -> Parser m a b
sliceBeginWith = undefined

-- | Split using a condition or a count whichever occurs first. This is a
-- hybrid of 'splitOn' and 'take'. The element on which the condition succeeds
-- is dropped.
--
-- /Internal/
--
{-# INLINABLE sliceSepByMax #-}
sliceSepByMax :: Monad m
    => (a -> Bool) -> Int -> Fold m a b -> Parser m a b
sliceSepByMax predicate cnt (Fold fstep finitial fextract) =
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

-- XXX use an Unfold instead of a list?
-- XXX custom combinators for matching list, array and stream?
--
-- | Match the given sequence of elements using the given comparison function.
--
-- /Internal/
--
{-# INLINE eqBy #-}
eqBy :: MonadThrow m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp str = Parser step initial extract

    where

    initial = return str

    step [] _ = error "Bug: unreachable"
    step [x] a = return $
        if x `cmp` a
        then Stop 0 ()
        else Error $
            "eqBy: failed, at the last element"
    step (x:xs) a = return $
        if x `cmp` a
        then Skip 0 xs
        else Error $
            "eqBy: failed, yet to match " ++ show (length xs) ++ " elements"

    extract xs = throwM $ ParseError $
        "eqBy: end of input, yet to match " ++ show (length xs) ++ " elements"

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Parser m a b -> Parser m a b
lookAhead (Parser step1 initial1 _) =
    Parser step initial extract

    where

    initial = Tuple' 0 <$> initial1

    step (Tuple' cnt st) a = do
        r <- step1 st a
        let cnt1 = cnt + 1
        return $ case r of
            Yield _ s -> Skip 0 (Tuple' cnt1 s)
            Skip n s -> Skip n (Tuple' (cnt1 - n) s)
            Stop _ b -> Stop cnt1 b
            Error err -> Error err

    -- XXX returning an error let's us backtrack.  To implement it in a way so
    -- that it terminates on eof without an error then we need a way to
    -- backtrack on eof, that will require extract to return 'Step' type.
    extract (Tuple' n _) = throwM $ ParseError $
        "lookAhead: end of input after consuming " ++ show n ++ " elements"

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------
--
-- To deinterleave we can chain two parsers one behind the other. The input is
-- given to the first parser and the input definitively rejected by the first
-- parser is given to the second parser.
--
-- We can either have the parsers themselves buffer the input or use the shared
-- global buffer to hold it until none of the parsers need it. When the first
-- parser returns Skip (i.e. rewind) we let the second parser consume the
-- rejected input and when it is done we move the cursor forward to the first
-- parser again. This will require a "move forward" command as well.
--
-- To implement grep we can use three parsers, one to find the pattern, one
-- to store the context behind the pattern and one to store the context in
-- front of the pattern. When a match occurs we need to emit the accumulator of
-- all the three parsers. One parser can count the line numbers to provide the
-- line number info.
--
-- | Apply two parsers alternately to an input stream. The input stream is
-- considered an interleaving of two patterns. The two parsers represent the
-- two patterns.
--
-- This undoes a "gintercalate" of two streams.
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

-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------
--
-- | @choice parsers@ applies the @parsers@ in order and returns the first
-- successful parse.
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
-- many = countBetween 0 maxBound

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
-- some f p = many (takeGE 1 f) p
-- many = countBetween 1 maxBound

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
-- countBetween m n f p = many (takeBetween m n f) p

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
-- count n f p = many (takeEQ n f) p

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
