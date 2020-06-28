#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans  #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast backtracking parsers with stream fusion and native streaming
-- capability.
--
-- 'Applicative' and 'Control.Applicative.Alternative' type class based
-- combinators from the
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
-- and a composable consumer.
--
-- 2) Several combinators in this module can be many times faster because of
-- stream fusion. For example, 'Streamly.Internal.Data.Parser.many' combinator
-- in this module is much faster than the 'Control.Applicative.many' combinator
-- of 'Control.Applicative.Alternative' type class.
--
-- = Errors
--
-- Failing parsers in this module throw the 'D.ParseError' exception.
--
-- = Naming
--
-- As far as possible, try that the names of the combinators in this module are
-- consistent with:
--
-- * <https://hackage.haskell.org/package/base/docs/Text-ParserCombinators-ReadP.html base/Text.ParserCombinators.ReadP>
-- * <http://hackage.haskell.org/package/parser-combinators parser-combinators>
-- * <http://hackage.haskell.org/package/megaparsec megaparsec>
-- * <http://hackage.haskell.org/package/attoparsec attoparsec>
-- * <http://hackage.haskell.org/package/parsec parsec>

module Streamly.Internal.Data.Parser
    (
      K.Parser (..)
    , D.ParseError (..)
    , D.Step (..)

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
    -- | Parsers chained in series, if one parser terminates the composition
    -- terminates.
    --
    -- TODO: Currently we are using folds to collect the output of the parsers
    -- but we can use Parsers instead of folds to make the composition more
    -- powerful. For example, we can do:
    --
    -- @
    -- sliceSepByMax cond n p = sliceBy cond (take n p)
    -- sliceSepByBetween cond m n p = sliceBy cond (takeBetween m n p)
    -- takeWhileBetween cond m n p = takeWhile cond (takeBetween m n p)
    -- @
    --
    -- TODO: Like toList fold we can have toNonEmpty Parser to fold to a
    -- nonempty list. If we cannot collect even one element the parser will
    -- fail.  toNonEmpty can be used in takeWhile to implement takeWhile1.

    -- | Grab a sequence of input elements without inspecting them
    , take
    -- $take
    -- | Unimplemented
    --
    -- @
    -- , takeBetween
    -- , takeLE -- take   -- takeBetween 0 n
    -- , takeLE1 -- take1 -- takeBetween 1 n
    -- @
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound

    -- Grab a sequence of input elements by inspecting them
    , lookAhead
    , takeWhile
    -- $takeWhile
    , takeWhile1
    , sliceSepBy
    , sliceSepByMax
    -- | Unimplemented
    --
    -- @
    -- , sliceSepByBetween
    -- @
    , sliceEndWith
    , sliceBeginWith
    -- | Unimplemented
    --
    -- @
    -- , sliceSepWith
    --
    -- , frameSepBy -- parse frames escaped by an escape char/sequence
    -- , frameEndWith
    -- @
    , wordBy
    , groupBy
    , eqBy
    -- | Unimplemented
    --
    -- @
    -- , prefixOf -- match any prefix of a given string
    -- , suffixOf -- match any suffix of a given string
    -- , infixOf -- match any substring of a given string
    -- @

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

    -- ** Special cases
    -- | TODO: traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by @\\r\\n@. The main parser
    -- will have to detect and exclude the sequence @\\r\\n@ anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- @
    -- , endBy
    -- , sepBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy
    -- @

    -- * Distribution
    --
    -- | A simple and stupid impl would be to just convert the stream to an
    -- array and give the array reference to all consumers. The array can be
    -- grown on demand by any consumer and truncated when nonbody needs it.

    -- ** Distribute to collection
    -- ** Distribute to repetition

    -- ** Interleaved collection
    -- |
    --
    -- 1. Round robin
    -- 2. Priority based

    -- ** Interleaved repetition
    -- | repeat one parser and when it fails run an error recovery parser
    -- e.g. to find a key frame in the stream after an error

    -- ** Collection of Alternatives
    -- | Unimplemented
    --
    -- @
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    -- @
    , choice   -- first successful in position

    -- ** Repeated Alternatives
    -- |
    --
    -- @
    -- , retryMax    -- try N times
    -- , retryUntil  -- try until successful
    -- , retryUntilN -- try until successful n times
    -- @
    )
where

import Control.Monad.Catch (MonadCatch)
import Prelude
       hiding (any, all, take, takeWhile, sequence, concatMap)

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Parser.ParserK.Types (Parser)

import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Data.Parser.ParserK.Types as K

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | Make a 'Parser' from a 'Fold'.
--
-- /Internal/
--
{-# INLINE fromFold #-}
fromFold :: MonadCatch m => Fold m a b -> Parser m a b
fromFold = D.toParserK . D.fromFold

-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
-- |
-- >>> S.parse (PR.any (== 0)) $ S.fromList [1,0,1]
-- > True
--
{-# INLINE any #-}
any :: MonadCatch m => (a -> Bool) -> Parser m a Bool
any = D.toParserK . D.any

-- |
-- >>> S.parse (PR.all (== 0)) $ S.fromList [1,0,1]
-- > False
--
{-# INLINE all #-}
all :: MonadCatch m => (a -> Bool) -> Parser m a Bool
all = D.toParserK . D.all

-- This is the dual of stream "yield".
--
-- | A parser that always yields a pure value without consuming any input.
--
-- /Internal/
--
{-# INLINE [3] yield #-}
yield :: MonadCatch m => b -> Parser m a b
yield = D.toParserK . D.yield
{-# RULES "yield fallback to CPS" [2]
    forall a. D.toParserK (D.yield a) = K.yield a #-}

-- This is the dual of stream "yieldM".
--
-- | A parser that always yields the result of an effectful action without
-- consuming any input.
--
-- /Internal/
--
{-# INLINE yieldM #-}
yieldM :: MonadCatch m => m b -> Parser m a b
yieldM = D.toParserK . D.yieldM

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
-- /Internal/
--
{-# INLINE [3] die #-}
die :: MonadCatch m => String -> Parser m a b
die = D.toParserK . D.die
{-# RULES "die fallback to CPS" [2]
    forall a. D.toParserK (D.die a) = K.die a #-}

-- This is the dual of "nilM".
--
-- | A parser that always fails with an effectful error message and without
-- consuming any input.
--
-- /Internal/
--
{-# INLINE dieM #-}
dieM :: MonadCatch m => m String -> Parser m a b
dieM = D.toParserK . D.dieM

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
{-# INLINE peek #-}
peek :: MonadCatch m => Parser m a a
peek = D.toParserK D.peek

-- | Succeeds if we are at the end of input, fails otherwise.
--
-- >>> S.parse ((,) <$> PR.satisfy (> 0) <*> PR.eof) $ S.fromList [1]
-- > (1,())
--
-- /Internal/
--
{-# INLINE eof #-}
eof :: MonadCatch m => Parser m a ()
eof = D.toParserK D.eof

-- | Returns the next element if it passes the predicate, fails otherwise.
--
-- >>> S.parse (PR.satisfy (== 1)) $ S.fromList [1,0,1]
-- > 1
--
-- /Internal/
--
{-# INLINE satisfy #-}
satisfy :: MonadCatch m => (a -> Bool) -> Parser m a a
satisfy = D.toParserK . D.satisfy

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------
--
-- $take
-- Note: this is called takeP in some parser libraries.
--
-- TODO Once we have terminating folds, this Parse should get replaced by Fold.
-- Alternatively, we can name it "chunkOf" and the corresponding time domain
-- combinator as "intervalOf" or even "chunk" and "interval".

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
take :: MonadCatch m => Int -> Fold m a b -> Parser m a b
take n = D.toParserK . D.take n

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
takeEQ :: MonadCatch m => Int -> Fold m a b -> Parser m a b
takeEQ n = D.toParserK . D.takeEQ n

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - never.
-- * Fails - if the stream end before producing @n@ elements.
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
takeGE :: MonadCatch m => Int -> Fold m a b -> Parser m a b
takeGE n = D.toParserK . D.takeGE n

-- $takeWhile
-- Note: This is called @takeWhileP@ and @munch@ in some parser libraries.

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
takeWhile :: MonadCatch m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile cond = D.toParserK . D.takeWhile cond

-- | Like 'takeWhile' but takes at least one element otherwise fails.
--
-- /Internal/
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: MonadCatch m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile1 cond = D.toParserK . D.takeWhile1 cond

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
sliceSepBy :: MonadCatch m => (a -> Bool) -> Fold m a b -> Parser m a b
sliceSepBy cond = D.toParserK . D.sliceSepBy cond

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
sliceSepByMax :: MonadCatch m
    => (a -> Bool) -> Int -> Fold m a b -> Parser m a b
sliceSepByMax cond cnt = D.toParserK . D.sliceSepByMax cond cnt

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

-- | Match the given sequence of elements using the given comparison function.
--
-- >>> S.parse $ S.eqBy (==) "string" $ S.fromList "string"
--
-- >>> S.parse $ S.eqBy (==) "mismatch" $ S.fromList "match"
-- > *** Exception: ParseError "eqBy: failed, yet to match 7 elements"
--
-- /Internal/
--
{-# INLINE eqBy #-}
eqBy :: MonadCatch m => (a -> a -> Bool) -> [a] -> Parser m a ()
eqBy cmp = D.toParserK . D.eqBy cmp

-------------------------------------------------------------------------------
-- nested parsers
-------------------------------------------------------------------------------

-- | Sequential parser application. Apply two parsers sequentially to an input
-- stream.  The input is provided to the first parser, when it is done the
-- remaining input is provided to the second parser. If both the parsers
-- succeed their outputs are combined using the supplied function. The
-- operation fails if any of the parsers fail.
--
-- Note: This is a parsing dual of appending streams using
-- 'Streamly.Prelude.serial', it splits the streams using two parsers and zips
-- the results.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> S.parse (PR.splitWith const (PR.satisfy (> 0)) undefined) $ S.fromList [1]
--
-- Compare with 'Applicative' instance method '<*>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be faster than 'Applicative' instance for small number
-- (less than 8) of compositions.
--
-- /Internal/
--
{-# INLINE splitWith #-}
splitWith :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
splitWith f p1 p2 =
    D.toParserK $ D.splitWith f (D.fromParserK p1) (D.fromParserK p2)

-- | Sequential parser application ignoring the output of the first parser.
-- Apply two parsers sequentially to an input stream.  The input is provided to
-- the first parser, when it is done the remaining input is provided to the
-- second parser. The output of the parser is the output of the second parser.
-- The operation fails if any of the parsers fail.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> S.parse (split_ (PR.satisfy (> 0)) undefined) $ S.fromList [1]
--
-- Compare with 'Applicative' instance method '*>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations, and can be faster than 'Applicative' instance for small
-- number (less than 8) of compositions.
--
-- /Internal/
--
{-# INLINE split_ #-}
split_ :: MonadCatch m => Parser m x a -> Parser m x b -> Parser m x b
split_ p1 p2 = D.toParserK $ D.split_ (D.fromParserK p1) (D.fromParserK p2)

-- | @teeWith f p1 p2@ distributes its input to both @p1@ and @p2@ until both
-- of them succeed or anyone of them fails and combines their output using @f@.
-- The parser succeeds if both the parsers succeed.
--
-- /Internal/
--
{-# INLINE teeWith #-}
teeWith :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWith f p1 p2 =
    D.toParserK $ D.teeWith f (D.fromParserK p1) (D.fromParserK p2)

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever the first parser ends.
--
-- /Internal/
--
{-# INLINE teeWithFst #-}
teeWithFst :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWithFst f p1 p2 =
    D.toParserK $ D.teeWithFst f (D.fromParserK p1) (D.fromParserK p2)

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever any of the parsers ends or fails.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin :: MonadCatch m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
teeWithMin f p1 p2 =
    D.toParserK $ D.teeWithMin f (D.fromParserK p1) (D.fromParserK p2)

-- | Sequential alternative. Apply the input to the first parser and return the
-- result if the parser succeeds. If the first parser fails then backtrack and
-- apply the same input to the second parser and return the result.
--
-- Note: This implementation is not lazy in the second argument. The following
-- will fail:
--
-- >>> S.parse (PR.satisfy (> 0) `PR.alt` undefined) $ S.fromList [1..10]
--
-- Compare with 'Alternative' instance method '<|>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be much faster than 'Alternative' instance for small
-- number (less than 8) of alternatives.
--
-- /Internal/
--
{-# INLINE alt #-}
alt :: MonadCatch m => Parser m x a -> Parser m x a -> Parser m x a
alt p1 p2 = D.toParserK $ D.alt (D.fromParserK p1) (D.fromParserK p2)

-- | Shortest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- parse.
--
-- /Internal/
--
{-# INLINE shortest #-}
shortest :: MonadCatch m
    => Parser m x a -> Parser m x a -> Parser m x a
shortest p1 p2 = D.toParserK $ D.shortest (D.fromParserK p1) (D.fromParserK p2)

-- | Longest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- parse.
--
-- /Internal/
--
{-# INLINE longest #-}
longest :: MonadCatch m
    => Parser m x a -> Parser m x a -> Parser m x a
longest p1 p2 = D.toParserK $ D.longest (D.fromParserK p1) (D.fromParserK p2)

-- | Run a parser without consuming the input.
--
-- /Internal/
--
{-# INLINE lookAhead #-}
lookAhead :: MonadCatch m => Parser m a b -> Parser m a b
lookAhead p = D.toParserK $ D.lookAhead $ D.fromParserK p

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

-- | Map a 'Parser' returning function on the result of a 'Parser'.
--
-- Compare with 'Monad' instance method '>>='. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be much faster than 'Monad' instance for small number
-- (less than 8) of compositions.
--
-- /Internal/
--
{-# INLINE concatMap #-}
concatMap :: MonadCatch m
    => (b -> Parser m a c) -> Parser m a b -> Parser m a c
concatMap f p = D.toParserK $ D.concatMap (D.fromParserK . f) (D.fromParserK p)

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
-- $many
-- TODO "many" is essentially a Fold because it cannot fail. So it can be
-- downgraded to a Fold. Or we can make the return type a Fold instead and
-- upgrade that to a parser when needed.

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
many f p = D.toParserK $ D.many f (D.fromParserK p)
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
some f p = D.toParserK $ D.some f (D.fromParserK p)
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
manyTill f p1 p2 =
    D.toParserK $ D.manyTill f (D.fromParserK p1) (D.fromParserK p2)
