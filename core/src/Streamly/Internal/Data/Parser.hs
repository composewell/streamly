{-# OPTIONS_GHC -Wno-orphans  #-}

-- |
-- Module      : Streamly.Internal.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
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

    -- * Downgrade to Fold
    , toFold

    -- First order parsers
    -- * Accumulators
    , fromFold
    , fromFoldMaybe
    , fromPure
    , fromEffect
    , die
    , dieM

    -- * Map on input
    , lmap
    , lmapM
    , filter

    -- * Map on output
    , rmapM

    -- * Element parsers
    , peek

    -- All of these can be expressed in terms of either
    , one
    , oneEq
    , oneNotEq
    , oneOf
    , noneOf
    , eof
    , satisfy
    , maybe
    , either

    -- * Sequence parsers (tokenizers)
    --
    -- | Parsers chained in series, if one parser terminates the composition
    -- terminates.

    , lookAhead

    -- ** By length
    -- | Grab a sequence of input elements without inspecting them
    , takeBetween
    -- , take   -- takeBetween 0 n
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound
    , takeP

    -- Grab a sequence of input elements by inspecting them
    -- ** Exact match
    , listEq
    , listEqBy
    , streamEqBy
    , subsequenceBy

    -- ** By predicate
    , takeWhileP
    , takeWhile
    -- $takeWhile
    , takeWhile1
    , dropWhile

    -- ** Separators
    , takeEndBy
    , takeEndBy_
    , takeEndByEsc
    -- , takeEndByEsc_
    , takeStartBy
    , takeStartBy_
    , takeEitherSepBy
    , wordBy
    -- , wordByEsc

    -- ** By comparing
    , groupBy
    , groupByRolling
    , groupByRollingEither

    -- ** Framing
    -- , takeFramedBy
    , takeFramedBy_
    , takeFramedByEsc_
    , takeFramedByGeneric
    , wordFramedBy
    , wordQuotedBy

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

{-
    -- ** Parallel Applicatives
    , teeWith
    , teeWithFst
    , teeWithMin
    -- , teeTill -- like manyTill but parallel
-}

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate
    -- , deintercalatePrefix
    -- , deintercalateSuffix

    -- *** Special cases
    -- | TODO: traditional implmentations of these may be of limited use. For
    -- example, consider parsing lines separated by @\\r\\n@. The main parser
    -- will have to detect and exclude the sequence @\\r\\n@ anyway so that we
    -- can apply the "sep" parser.
    --
    -- We can instead implement these as special cases of deintercalate.
    --
    -- @
    -- , endBy
    -- , sepEndBy
    -- , beginBy
    -- , sepBeginBy
    -- , sepAroundBy
    -- @
    , sepBy1
    , sepBy

    -- ** Sequential Alternative
    , alt

{-
    -- ** Parallel Alternatives
    , shortest
    , longest
    -- , fastest
-}

    -- * N-ary Combinators
    -- ** Sequential Collection
    , concatSequence
    , concatMap

    -- ** Sequential Repetition
    , count
    , countBetween

    , manyP
    , many
    , some
    , manyTillP
    , manyTill
    , manyThen

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
    , roundRobin

    -- ** Collection of Alternatives
    -- | Unimplemented
    --
    -- @
    -- , shortestN
    -- , longestN
    -- , fastestN -- first N successful in time
    -- , choiceN  -- first N successful in position
    -- @
    -- , choice   -- first successful in position

    -- ** Repeated Alternatives
    , retryMaxTotal
    , retryMaxSuccessive
    , retry

     -- * Deprecated
    , next
    )
where

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser.ParserK.Type (Parser)

import qualified Data.Foldable as Foldable
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Parser.ParserD as D
import qualified Streamly.Internal.Data.Parser.ParserK.Type as K

#ifdef USE_STREAMK
import Streamly.Internal.Data.StreamK (Stream)
import qualified Streamly.Internal.Data.StreamK as Stream
#else
import Streamly.Internal.Data.Stream.StreamD.Type (Stream)
import qualified Streamly.Internal.Data.Stream.StreamD.Type as Stream
#endif

import Prelude hiding
    ( any, all, dropWhile, take, takeWhile, sequence, concatMap, maybe, either
    , filter )

--
-- $setup
-- >>> :m
-- >>> import Prelude hiding (any, all, dropWhile, take, takeWhile, sequence, concatMap, maybe, either, filter)
-- >>> import Control.Applicative ((<|>))
-- >>> import Data.Char (isSpace)
-- >>> import qualified Data.Maybe as Maybe
-- >>> import qualified Data.Foldable as Foldable
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream as Stream (parse, parseMany)
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser

-------------------------------------------------------------------------------
-- Downgrade a parser to a Fold
-------------------------------------------------------------------------------
--
-- | Make a 'Fold' from a 'Parser'. The fold just throws an exception if the
-- parser fails or tries to backtrack.
--
-- This can be useful in combinators that accept a Fold and we know that a
-- Parser cannot fail or failure exception is acceptable as there is no way to
-- recover.
--
-- /Pre-release/
--
{-# INLINE toFold #-}
toFold :: Monad m => Parser a m b -> Fold m a b
toFold p = D.toFold $ D.fromParserK p

-------------------------------------------------------------------------------
-- Upgrade folds to parses
-------------------------------------------------------------------------------
--
-- | Make a 'Parser' from a 'Fold'.
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser a m b
fromFold = D.toParserK . D.fromFold

-- | Convert a Maybe returning fold to an error returning parser. The first
-- argument is the error message that the parser would return when the fold
-- returns Nothing.
--
-- /Pre-release/
--
{-# INLINE fromFoldMaybe #-}
fromFoldMaybe :: Monad m => String -> Fold m a (Maybe b) -> Parser a m b
fromFoldMaybe err = D.toParserK . D.fromFoldMaybe err

-------------------------------------------------------------------------------
-- Terminating but not failing folds
-------------------------------------------------------------------------------
--
-- This is the dual of stream "fromPure".
--
-- | A parser that always yields a pure value without consuming any input.
--
{-# INLINE [3] fromPure #-}
fromPure :: Monad m => b -> Parser a m b
fromPure = D.toParserK . D.fromPure
{-# RULES "fromPure fallback to CPS" [2]
    forall a. D.toParserK (D.fromPure a) = K.fromPure a #-}

-- This is the dual of stream "fromEffect".
--
-- | A parser that always yields the result of an effectful action without
-- consuming any input.
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Parser a m b
fromEffect = K.fromEffect -- D.toParserK . D.fromEffect

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
{-# INLINE [3] die #-}
die :: Monad m => String -> Parser a m b
die = D.toParserK . D.die
{-# RULES "die fallback to CPS" [2]
    forall a. D.toParserK (D.die a) = K.die a #-}

-- This is the dual of "nilM".
--
-- | A parser that always fails with an effectful error message and without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE dieM #-}
dieM :: Monad m => m String -> Parser a m b
dieM = D.toParserK . D.dieM

-------------------------------------------------------------------------------
-- Map on input
-------------------------------------------------------------------------------

-- | @lmap f parser@ maps the function @f@ on the input of the parser.
--
-- >>> Stream.parse (Parser.lmap (\x -> x * x) (Parser.fromFold Fold.sum)) (Stream.enumerateFromTo 1 100)
-- Right 338350
--
-- > lmap = Parser.lmapM return
--
{-# INLINE lmap #-}
lmap :: Monad m => (a -> b) -> Parser b m r -> Parser a m r
lmap f p = D.toParserK $ D.lmap f $ D.fromParserK p

-- | @lmapM f parser@ maps the monadic function @f@ on the input of the parser.
--
{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Parser b m r -> Parser a m r
lmapM f p = D.toParserK $ D.lmapM f $ D.fromParserK p

-- | @rmapM f parser@ maps the monadic function @f@ on the output of the parser.
--
-- >>> rmap = fmap
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Parser a m b -> Parser a m c
rmapM f p = D.toParserK $ D.rmapM f $ D.fromParserK p

-- | Include only those elements that pass a predicate.
--
-- >>> Stream.parse (Parser.filter (> 5) (Parser.fromFold Fold.sum)) $ Stream.fromList [1..10]
-- Right 40
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
filter f p = D.toParserK $ D.filter f $ D.fromParserK p

-------------------------------------------------------------------------------
-- Failing Parsers
-------------------------------------------------------------------------------

-- | Peek the head element of a stream, without consuming it. Fails if it
-- encounters end of input.
--
-- >>> Stream.parse ((,) <$> Parser.peek <*> Parser.satisfy (> 0)) $ Stream.fromList [1]
-- Right (1,1)
--
-- @
-- peek = lookAhead (satisfy True)
-- @
--
{-# INLINE peek #-}
peek :: Monad m => Parser a m a
peek = D.toParserK D.peek

-- | Succeeds if we are at the end of input, fails otherwise.
--
-- >>> Stream.parse ((,) <$> Parser.satisfy (> 0) <*> Parser.eof) $ Stream.fromList [1]
-- Right (1,())
--
{-# INLINE eof #-}
eof :: Monad m => Parser a m ()
eof = D.toParserK D.eof

-- | Returns the next element if it passes the predicate, fails otherwise.
--
-- >>> Stream.parse (Parser.satisfy (== 1)) $ Stream.fromList [1,0,1]
-- Right 1
--
-- >>> toMaybe f x = if f x then Just x else Nothing
-- >>> satisfy f = Parser.maybe (toMaybe f)
--
{-# INLINE satisfy #-}
satisfy :: Monad m => (a -> Bool) -> Parser a m a
satisfy = D.toParserK . D.satisfy

-- | Consume one element from the head of the stream.  Fails if it encounters
-- end of input.
--
-- >>> one = Parser.satisfy $ const True
--
{-# INLINE one #-}
one :: Monad m => Parser a m a
one = satisfy $ const True

-- Alternate names: "only", "onlyThis".

-- | Match a specific element.
--
-- >>> oneEq x = Parser.satisfy (== x)
--
{-# INLINE oneEq #-}
oneEq :: (Monad m, Eq a) => a -> Parser a m a
oneEq x = satisfy (== x)

-- Alternate names: "exclude", "notThis".

-- | Match anything other than the supplied element.
--
-- >>> oneNotEq x = Parser.satisfy (/= x)
--
{-# INLINE oneNotEq #-}
oneNotEq :: (Monad m, Eq a) => a -> Parser a m a
oneNotEq x = satisfy (/= x)

-- | Match any one of the elements in the supplied list.
--
-- >>> oneOf xs = Parser.satisfy (`Foldable.elem` xs)
--
-- When performance matters a pattern matching predicate could be more
-- efficient than a 'Foldable' datatype:
--
-- @
-- let p x =
--    case x of
--       'a' -> True
--       'e' -> True
--        _  -> False
-- in satisfy p
-- @
--
-- GHC may use a binary search instead of linear search in the list.
-- Alternatively, you can also use an array instead of list for storage and
-- search.
--
{-# INLINE oneOf #-}
oneOf :: (Monad m, Eq a, Foldable f) => f a -> Parser a m a
oneOf xs = satisfy (`Foldable.elem` xs)

-- | See performance notes in 'oneOf'.
--
-- >>> noneOf xs = Parser.satisfy (`Foldable.notElem` xs)
--
{-# INLINE noneOf #-}
noneOf :: (Monad m, Eq a, Foldable f) => f a -> Parser a m a
noneOf xs = satisfy (`Foldable.notElem` xs)

-- | Return the next element of the input. Returns 'Nothing'
-- on end of input. Also known as 'head'.
--
-- /Pre-release/
--
{-# DEPRECATED next "Please use \"fromFold Fold.one\" instead" #-}
{-# INLINE next #-}
next :: Monad m => Parser a m (Maybe a)
next = D.toParserK D.next

-- | Map a 'Maybe' returning function on the next element in the stream. The
-- parser fails if the function returns 'Nothing' otherwise returns the 'Just'
-- value.
--
-- >>> toEither = Maybe.maybe (Left "maybe: predicate failed") Right
-- >>> maybe f = Parser.either (toEither . f)
--
-- >>> maybe f = Parser.fromFoldMaybe "maybe: predicate failed" (Fold.maybe f)
--
-- /Pre-release/
--
{-# INLINE maybe #-}
maybe :: Monad m => (a -> Maybe b) -> Parser a m b
maybe = D.toParserK . D.maybe

-- | Map an 'Either' returning function on the next element in the stream.  If
-- the function returns 'Left err', the parser fails with the error message
-- @err@ otherwise returns the 'Right' value.
--
-- /Pre-release/
--
{-# INLINE either #-}
either :: Monad m => (a -> Either String b) -> Parser a m b
either = D.toParserK . D.either

-------------------------------------------------------------------------------
-- Taking elements
-------------------------------------------------------------------------------

-- | @takeBetween m n@ takes a minimum of @m@ and a maximum of @n@ input
-- elements and folds them using the supplied fold.
--
-- Stops after @n@ elements.
-- Fails if the stream ends before @m@ elements could be taken.
--
-- Examples: -
--
-- @
-- >>> :{
--   takeBetween' low high ls = Stream.parse prsr (Stream.fromList ls)
--     where prsr = Parser.takeBetween low high Fold.toList
-- :}
--
-- @
--
-- >>> takeBetween' 2 4 [1, 2, 3, 4, 5]
-- Right [1,2,3,4]
--
-- >>> takeBetween' 2 4 [1, 2]
-- Right [1,2]
--
-- >>> takeBetween' 2 4 [1]
-- Left (ParseError "takeBetween: Expecting alteast 2 elements, got 1")
--
-- >>> takeBetween' 0 0 [1, 2]
-- Right []
--
-- >>> takeBetween' 0 1 []
-- Right []
--
-- @takeBetween@ is the most general take operation, other take operations can
-- be defined in terms of takeBetween. For example:
--
-- >>> take n = Parser.takeBetween 0 n
-- >>> takeEQ n = Parser.takeBetween n n
-- >>> takeGE n = Parser.takeBetween n maxBound
--
-- /Pre-release/
--
{-# INLINE takeBetween #-}
takeBetween ::  Monad m =>
    Int -> Int -> Fold m a b -> Parser a m b
takeBetween m n = D.toParserK . D.takeBetween m n

-- | Stops after taking exactly @n@ input elements.
--
-- * Stops - after consuming @n@ elements.
-- * Fails - if the stream or the collecting fold ends before it can collect
--           exactly @n@ elements.
--
-- >>> Stream.parse (Parser.takeEQ 4 Fold.toList) $ Stream.fromList [1,0,1]
-- Left (ParseError "takeEQ: Expecting exactly 4 elements, input terminated on 3")
--
{-# INLINE takeEQ #-}
takeEQ :: Monad m => Int -> Fold m a b -> Parser a m b
takeEQ n = D.toParserK . D.takeEQ n

-- | Take at least @n@ input elements, but can collect more.
--
-- * Stops - when the collecting fold stops.
-- * Fails - if the stream or the collecting fold ends before producing @n@
--           elements.
--
-- >>> Stream.parse (Parser.takeGE 4 Fold.toList) $ Stream.fromList [1,0,1]
-- Left (ParseError "takeGE: Expecting at least 4 elements, input terminated on 3")
--
-- >>> Stream.parse (Parser.takeGE 4 Fold.toList) $ Stream.fromList [1,0,1,0,1]
-- Right [1,0,1,0,1]
--
-- /Pre-release/
--
{-# INLINE takeGE #-}
takeGE :: Monad m => Int -> Fold m a b -> Parser a m b
takeGE n = D.toParserK . D.takeGE n

-------------------------------------------------------------------------------
-- Take until a condition
-------------------------------------------------------------------------------

-- $takeWhile
-- Note: This is called @takeWhileP@ and @munch@ in some parser libraries.

-- XXX We should perhaps use only takeWhileP and rename it to takeWhile.
--
-- | Like 'takeWhile' but uses a 'Parser' instead of a 'Fold' to collect the
-- input. The combinator stops when the condition fails or if the collecting
-- parser stops.
--
-- Other interesting parsers can be implemented in terms of this parser:
--
-- >>> takeWhile1 cond p = Parser.takeWhileP cond (Parser.takeBetween 1 maxBound p)
-- >>> takeWhileBetween cond m n p = Parser.takeWhileP cond (Parser.takeBetween m n p)
--
-- Stops: when the condition fails or the collecting parser stops.
-- Fails: when the collecting parser fails.
--
-- /Pre-release/
--
{-# INLINE takeWhileP #-}
takeWhileP :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
takeWhileP cond p = D.toParserK $ D.takeWhileP cond (D.fromParserK p)

-- | Collect stream elements until an element fails the predicate. The element
-- on which the predicate fails is returned back to the input stream.
--
-- * Stops - when the predicate fails or the collecting fold stops.
-- * Fails - never.
--
-- >>> Stream.parse (Parser.takeWhile (== 0) Fold.toList) $ Stream.fromList [0,0,1,0,1]
-- Right [0,0]
--
-- >>> takeWhile cond f = Parser.takeWhileP cond (Parser.fromFold f)
--
-- We can implement a @breakOn@ using 'takeWhile':
--
-- @
-- breakOn p = takeWhile (not p)
-- @
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeWhile cond = D.toParserK . D.takeWhile cond
-- takeWhile cond f = takeWhileP cond (fromFold f)

-- | Like 'takeWhile' but takes at least one element otherwise fails.
--
-- >>> takeWhile1 cond p = Parser.takeWhileP cond (Parser.takeBetween 1 maxBound p)
--
{-# INLINE takeWhile1 #-}
takeWhile1 :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeWhile1 cond = D.toParserK . D.takeWhile1 cond
-- takeWhile1 cond f = takeWhileP cond (takeBetween 1 maxBound f)

-- | Drain the input as long as the predicate succeeds, running the effects and
-- discarding the results.
--
-- This is also called @skipWhile@ in some parsing libraries.
--
-- >>> dropWhile p = Parser.takeWhile p Fold.drain
--
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Parser a m ()
dropWhile p = takeWhile p FL.drain

-------------------------------------------------------------------------------
-- Separators
-------------------------------------------------------------------------------

-- XXX We can remove Maybe from esc
{-# INLINE takeFramedByGeneric #-}
takeFramedByGeneric :: Monad m =>
       Maybe (a -> Bool)
    -> Maybe (a -> Bool)
    -> Maybe (a -> Bool)
    -> Fold m a b
    -> Parser a m b
takeFramedByGeneric esc begin end f =
    D.toParserK $ D.takeFramedByGeneric esc begin end f

-- | @takeEndBy cond parser@ parses a token that ends by a separator chosen by
-- the supplied predicate. The separator is also taken with the token.
--
-- This can be combined with other parsers to implement other interesting
-- parsers as follows:
--
-- >>> takeEndByLE cond n p = Parser.takeEndBy cond (Parser.fromFold $ Fold.take n p)
-- >>> takeEndByBetween cond m n p = Parser.takeEndBy cond (Parser.takeBetween m n p)
--
-- >>> takeEndBy = Parser.takeEndByEsc (const False)
--
-- See also "Streamly.Data.Fold.takeEndBy". Unlike the fold, the collecting
-- parser in the takeEndBy parser can decide whether to fail or not if the
-- stream does not end with separator.
--
-- /Pre-release/
--
{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
takeEndBy cond = D.toParserK . D.takeEndBy cond . D.fromParserK
-- takeEndBy = takeEndByEsc (const False)

-- | Like 'takeEndBy' but the separator is dropped.
--
-- See also "Streamly.Data.Fold.takeEndBy_".
--
-- /Pre-release/
--
{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m => (a -> Bool) -> Parser a m b -> Parser a m b
takeEndBy_ cond = D.toParserK . D.takeEndBy_ cond . D.fromParserK
{-
takeEndBy_ isEnd p =
    takeFramedByGeneric Nothing Nothing (Just isEnd) (toFold p)
-}

-- | Take either the separator or the token. Separator is a Left value and
-- token is Right value.
--
-- /Unimplemented/
{-# INLINE takeEitherSepBy #-}
takeEitherSepBy :: -- Monad m =>
    (a -> Bool) -> Fold m (Either a b) c -> Parser a m c
takeEitherSepBy _cond = undefined -- D.toParserK . D.takeEitherSepBy cond

-- | Parse a token that starts with an element chosen by the predicate.  The
-- parser fails if the input does not start with the selected element.
--
-- * Stops - when the predicate succeeds in non-leading position.
-- * Fails - when the predicate fails in the leading position.
--
-- >>> splitWithPrefix p f = Stream.parseMany (Parser.takeStartBy p f)
--
-- Examples: -
--
-- >>> p = Parser.takeStartBy (== ',') Fold.toList
-- >>> leadingComma = Stream.parse p . Stream.fromList
-- >>> leadingComma "a,b"
-- Left (ParseError "takeStartBy: missing frame start")
-- ...
-- >>> leadingComma ",,"
-- Right ","
-- >>> leadingComma ",a,b"
-- Right ",a"
-- >>> leadingComma ""
-- Right ""
--
-- /Pre-release/
--
{-# INLINE takeStartBy #-}
takeStartBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeStartBy cond = D.toParserK . D.takeStartBy cond

-- | Like 'takeStartBy' but drops the separator.
--
-- >>> takeStartBy_ isBegin = Parser.takeFramedByGeneric Nothing (Just isBegin) Nothing
--
{-# INLINE takeStartBy_ #-}
takeStartBy_ :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeStartBy_ isBegin = takeFramedByGeneric Nothing (Just isBegin) Nothing

-------------------------------------------------------------------------------
-- Quoting and Escaping
-------------------------------------------------------------------------------

-- | Like 'takeEndBy' but the separator elements can be escaped using an
-- escape char determined by the first predicate. The escape characters are
-- removed.
--
-- /pre-release/
{-# INLINE takeEndByEsc #-}
takeEndByEsc :: Monad m =>
    (a -> Bool) -> (a -> Bool) -> Parser a m b -> Parser a m b
takeEndByEsc isEsc isEnd p =
    D.toParserK $ D.takeEndByEsc isEsc isEnd (D.fromParserK p)

-- | @takeFramedByEsc_ isEsc isBegin isEnd fold@ parses a token framed using a
-- begin and end predicate, and an escape character. The frame begin and end
-- characters lose their special meaning if preceded by the escape character.
--
-- Nested frames are allowed if begin and end markers are different, nested
-- frames must be balanced unless escaped, nested frame markers are emitted as
-- it is.
--
-- For example,
--
-- >>> p = Parser.takeFramedByEsc_ (== '\\') (== '{') (== '}') Fold.toList
-- >>> Stream.parse p $ Stream.fromList "{hello}"
-- Right "hello"
-- >>> Stream.parse p $ Stream.fromList "{hello {world}}"
-- Right "hello {world}"
-- >>> Stream.parse p $ Stream.fromList "{hello \\{world}"
-- Right "hello {world"
-- >>> Stream.parse p $ Stream.fromList "{hello {world}"
-- Left (ParseError "takeFramedByEsc_: missing frame end")
--
-- /Pre-release/
{-# INLINE takeFramedByEsc_ #-}
takeFramedByEsc_ :: Monad m =>
    (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser a m b
takeFramedByEsc_ isEsc isBegin isEnd f =
    D.toParserK $ D.takeFramedByEsc_ isEsc isBegin isEnd f
-- takeEndByEsc_ isEsc isEnd p =
--    takeFramedByGeneric (Just isEsc) Nothing (Just isEnd) (toFold p)

-- | @takeFramedBy_ isBegin isEnd fold@ parses a token framed by a begin and an
-- end predicate.
--
-- >>> takeFramedBy_ = Parser.takeFramedByEsc_ (const False)
--
{-# INLINE takeFramedBy_ #-}
takeFramedBy_ :: Monad m =>
    (a -> Bool) -> (a -> Bool) -> Fold m a b -> Parser a m b
takeFramedBy_ isBegin isEnd f = D.toParserK $ D.takeFramedBy_ isBegin isEnd f
-- takeFramedBy_ isBegin isEnd =
--    takeFramedByGeneric (Just (const False)) (Just isBegin) (Just isEnd)

-------------------------------------------------------------------------------
-- Grouping and words
-------------------------------------------------------------------------------

-- Note we can also get words using something like:
-- sepBy FL.toList (takeWhile (not . p) Fold.toList) (dropWhile p)
--
-- But that won't be as efficient and ergonomic.
--
-- | Like 'splitOn' but strips leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ having '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- * Stops - when it finds a word separator after a non-word element
-- * Fails - never.
--
-- >>> wordBy = Parser.wordFramedBy (const False) (const False) (const False)
--
-- @
-- S.wordsBy pred f = S.parseMany (PR.wordBy pred f)
-- @
--
{-# INLINE wordBy #-}
wordBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
wordBy f = D.toParserK . D.wordBy f

-- | Like 'wordBy' but treats anything inside a pair of quotes as a single
-- word, the quotes can be escaped by an escape character.  Recursive quotes
-- are possible if quote begin and end characters are different, quotes must be
-- balanced. Outermost quotes are stripped.
--
-- >>> braces = Parser.wordFramedBy (== '\\') (== '{') (== '}') isSpace Fold.toList
-- >>> Stream.parse braces $ Stream.fromList "{ab} cd"
-- Right "ab"
-- >>> Stream.parse braces $ Stream.fromList "{ab}{cd}"
-- Right "abcd"
-- >>> Stream.parse braces $ Stream.fromList "a{b} cd"
-- Right "ab"
-- >>> Stream.parse braces $ Stream.fromList "a{{b}} cd"
-- Right "a{b}"
--
-- >>> quotes = Parser.wordFramedBy (== '\\') (== '"') (== '"') isSpace Fold.toList
-- >>> Stream.parse quotes $ Stream.fromList "\"a\"\"b\""
-- Right "ab"
--
{-# INLINE wordFramedBy #-}
wordFramedBy :: Monad m =>
       (a -> Bool)  -- ^ Escape
    -> (a -> Bool)  -- ^ left quote
    -> (a -> Bool)  -- ^ right quote
    -> (a -> Bool)  -- ^ word seperator
    -> Fold m a b
    -> Parser a m b
wordFramedBy isEsc isBegin isEnd isSpc =
    D.toParserK . D.wordFramedBy isEsc isBegin isEnd isSpc

-- | Like 'wordFramedBy' but the closing quote is determined by the opening
-- quote. The first quote begin starts a quote that is closed by its
-- corresponding closing quote.
--
-- 'wordFramedBy' and 'wordQuotedBy' both allow multiple quote characters based
-- on the predicates but 'wordQuotedBy' always fixes the quote at the first
-- occurrence and then it is closed only by the corresponding closing quote.
-- Therefore, other quoting characters can be embedded inside it as normal
-- characters. On the other hand, 'wordFramedBy' would close the quote as soon
-- as it encounters any of the closing quotes.
--
-- >>> q = (`elem` ['"', '\''])
-- >>> p kQ = Parser.wordQuotedBy kQ (== '\\') q q id isSpace Fold.toList
--
-- >>> Stream.parse (p False) $ Stream.fromList "a\"b'c\";'d\"e'f ghi"
-- Right "ab'c;d\"ef"
--
-- >>> Stream.parse (p True) $ Stream.fromList "a\"b'c\";'d\"e'f ghi"
-- Right "a\"b'c\";'d\"e'f"
--
{-# INLINE wordQuotedBy #-}
wordQuotedBy :: (Monad m, Eq a) =>
       Bool         -- ^ keep the quotes in the output
    -> (a -> Bool)  -- ^ Escape
    -> (a -> Bool)  -- ^ left quote
    -> (a -> Bool)  -- ^ right quote
    -> (a -> a)     -- ^ get right quote from left quote
    -> (a -> Bool)  -- ^ word seperator
    -> Fold m a b
    -> Parser a m b
wordQuotedBy keepQuotes isEsc isBegin isEnd toRight isSpc =
    D.toParserK . D.wordQuotedBy keepQuotes isEsc isBegin isEnd toRight isSpc

-- | Given an input stream @[a,b,c,...]@ and a comparison function @cmp@, the
-- parser assigns the element @a@ to the first group, then if @a \`cmp` b@ is
-- 'True' @b@ is also assigned to the same group.  If @a \`cmp` c@ is 'True'
-- then @c@ is also assigned to the same group and so on. When the comparison
-- fails the parser is terminated. Each group is folded using the 'Fold' @f@ and
-- the result of the fold is the result of the parser.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- >>> :{
--  runGroupsBy eq =
--      Stream.fold Fold.toList
--          . Stream.parseMany (Parser.groupBy eq Fold.toList)
--          . Stream.fromList
-- :}
--
-- >>> runGroupsBy (<) []
-- []
--
-- >>> runGroupsBy (<) [1]
-- [Right [1]]
--
-- >>> runGroupsBy (<) [3, 5, 4, 1, 2, 0]
-- [Right [3,5,4],Right [1,2],Right [0]]
--
{-# INLINE groupBy #-}
groupBy :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser a m b
groupBy eq = D.toParserK . D.groupBy eq

-- | Unlike 'groupBy' this combinator performs a rolling comparison of two
-- successive elements in the input stream.  Assuming the input stream
-- is @[a,b,c,...]@ and the comparison function is @cmp@, the parser
-- first assigns the element @a@ to the first group, then if @a \`cmp` b@ is
-- 'True' @b@ is also assigned to the same group.  If @b \`cmp` c@ is 'True'
-- then @c@ is also assigned to the same group and so on. When the comparison
-- fails the parser is terminated. Each group is folded using the 'Fold' @f@ and
-- the result of the fold is the result of the parser.
--
-- * Stops - when the comparison fails.
-- * Fails - never.
--
-- >>> :{
--  runGroupsByRolling eq =
--      Stream.fold Fold.toList
--          . Stream.parseMany (Parser.groupByRolling eq Fold.toList)
--          . Stream.fromList
-- :}
--
-- >>> runGroupsByRolling (<) []
-- []
--
-- >>> runGroupsByRolling (<) [1]
-- [Right [1]]
--
-- >>> runGroupsByRolling (<) [3, 5, 4, 1, 2, 0]
-- [Right [3,5],Right [4],Right [1,2],Right [0]]
--
-- /Pre-release/
--
{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser a m b
groupByRolling eq = D.toParserK . D.groupByRolling eq

-- | Like 'groupByRolling', but if the predicate is 'True' then collects using
-- the first fold as long as the predicate holds 'True', if the predicate is
-- 'False' collects using the second fold as long as it remains 'False'.
-- Returns 'Left' for the first case and 'Right' for the second case.
--
-- For example, if we want to detect sorted sequences in a stream, both
-- ascending and descending cases we can use 'groupByRollingEither (<=)
-- Fold.toList Fold.toList'.
--
-- /Pre-release/
{-# INLINE groupByRollingEither #-}
groupByRollingEither :: Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser a m (Either b c)
groupByRollingEither eq f1 = D.toParserK . D.groupByRollingEither eq f1

-- | Like 'listEqBy' but uses a stream instead of a list and does not return
-- the stream.
--
-- See also: "Streamly.Data.Stream.streamEqBy"
--
{-# INLINE streamEqBy #-}
streamEqBy :: Monad m => (a -> a -> Bool) -> Stream m a -> Parser a m ()
streamEqBy cmp = D.toParserK . D.streamEqBy cmp

-- | Match the given sequence of elements using the given comparison function.
-- Returns the original sequence if successful.
--
-- Definition:
--
-- >>> listEqBy cmp xs = Parser.streamEqBy cmp (Stream.fromList xs) *> Parser.fromPure xs
--
-- Examples:
--
-- >>> Stream.parse (Parser.listEqBy (==) "string") $ Stream.fromList "string"
-- Right "string"
--
-- >>> Stream.parse (Parser.listEqBy (==) "mismatch") $ Stream.fromList "match"
-- Left (ParseError "streamEqBy: mismtach occurred")
--
{-# INLINE listEqBy #-}
listEqBy :: Monad m => (a -> a -> Bool) -> [a] -> Parser a m [a]
-- listEqBy cmp xs = D.toParserK (D.listEqBy cmp xs)
listEqBy cmp xs = streamEqBy cmp (Stream.fromList xs) *> fromPure xs

-- Rename to "list".
-- | Match the input sequence with the supplied list and return it if
-- successful.
--
-- >>> listEq = Parser.listEqBy (==)
--
{-# INLINE listEq #-}
listEq :: (Monad m, Eq a) => [a] -> Parser a m [a]
listEq = listEqBy (==)

-- | Match if the input stream is a subsequence of the argument stream i.e. all
-- the elements of the input stream occur, in order, in the argument stream.
-- The elements do not have to occur consecutively. A sequence is considered a
-- subsequence of itself.
{-# INLINE subsequenceBy #-}
subsequenceBy :: -- Monad m =>
    (a -> a -> Bool) -> Stream m a -> Parser a m ()
subsequenceBy = undefined

{-
-- Should go in Data.Parser.Regex in streamly package so that it can depend on
-- regex backends.
{-# INLINE regexPosix #-}
regexPosix :: -- Monad m =>
    Regex -> Parser m a (Maybe (Array (MatchOffset, MatchLength)))
regexPosix = undefined

{-# INLINE regexPCRE #-}
regexPCRE :: -- Monad m =>
    Regex -> Parser m a (Maybe (Array (MatchOffset, MatchLength)))
regexPCRE = undefined
-}

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
-- 'Streamly.Data.Stream.append', it splits the streams using two parsers and zips
-- the results.
--
-- This implementation is strict in the second argument, therefore, the
-- following will fail:
--
-- >>> Stream.parse (Parser.splitWith const (Parser.satisfy (> 0)) undefined) $ Stream.fromList [1]
-- *** Exception: Prelude.undefined
-- ...
--
-- Compare with 'Applicative' instance method '<*>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be faster than 'Applicative' instance for small number
-- (less than 8) of compositions.
--
-- Many combinators can be expressed using @splitWith@ and other parser
-- primitives. Some common idioms are described below,
--
-- @
-- span :: (a -> Bool) -> Fold m a b -> Fold m a b -> Parser a m b
-- span pred f1 f2 = splitWith (,) ('takeWhile' pred f1) ('fromFold' f2)
-- @
--
-- @
-- spanBy :: (a -> a -> Bool) -> Fold m a b -> Fold m a b -> Parser a m b
-- spanBy eq f1 f2 = splitWith (,) ('groupBy' eq f1) ('fromFold' f2)
-- @
--
-- @
-- spanByRolling :: (a -> a -> Bool) -> Fold m a b -> Fold m a b -> Parser a m b
-- spanByRolling eq f1 f2 = splitWith (,) ('groupByRolling' eq f1) ('fromFold' f2)
-- @
--
-- /Pre-release/
--
{-# INLINE splitWith #-}
splitWith :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
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
-- >>> Stream.parse (Parser.split_ (Parser.satisfy (> 0)) undefined) $ Stream.fromList [1]
-- *** Exception: Prelude.undefined
-- ...
--
-- Compare with 'Applicative' instance method '*>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations, and can be faster than 'Applicative' instance for small
-- number (less than 8) of compositions.
--
-- /Pre-release/
--
{-# INLINE split_ #-}
split_ :: Monad m => Parser x m a -> Parser x m b -> Parser x m b
split_ p1 p2 = D.toParserK $ D.split_ (D.fromParserK p1) (D.fromParserK p2)

{-
-- | @teeWith f p1 p2@ distributes its input to both @p1@ and @p2@ until both
-- of them succeed or anyone of them fails and combines their output using @f@.
-- The parser succeeds if both the parsers succeed.
--
-- /Pre-release/
--
{-# INLINE teeWith #-}
teeWith :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
teeWith f p1 p2 =
    D.toParserK $ D.teeWith f (D.fromParserK p1) (D.fromParserK p2)

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever the first parser ends.
--
-- /Pre-release/
--
{-# INLINE teeWithFst #-}
teeWithFst :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
teeWithFst f p1 p2 =
    D.toParserK $ D.teeWithFst f (D.fromParserK p1) (D.fromParserK p2)

-- | Like 'teeWith' but ends parsing and zips the results, if available,
-- whenever any of the parsers ends or fails.
--
-- /Unimplemented/
--
{-# INLINE teeWithMin #-}
teeWithMin :: Monad m
    => (a -> b -> c) -> Parser x m a -> Parser x m b -> Parser x m c
teeWithMin f p1 p2 =
    D.toParserK $ D.teeWithMin f (D.fromParserK p1) (D.fromParserK p2)
-}

-- | Sequential alternative. Apply the input to the first parser and return the
-- result if the parser succeeds. If the first parser fails then backtrack and
-- apply the same input to the second parser and return the result.
--
-- Note: This implementation is not lazy in the second argument. The following
-- will fail:
--
-- >>> Stream.parse (Parser.satisfy (> 0) `Parser.alt` undefined) $ Stream.fromList [1..10]
-- Right 1
--
-- Compare with 'Alternative' instance method '<|>'. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be much faster than 'Alternative' instance for small
-- number (less than 8) of alternatives.
--
-- /Pre-release/
--
{-# INLINE alt #-}
alt :: Monad m => Parser x m a -> Parser x m a -> Parser x m a
alt p1 p2 = D.toParserK $ D.alt (D.fromParserK p1) (D.fromParserK p2)

{-
-- | Shortest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- parse.
--
-- /Pre-release/
--
{-# INLINE shortest #-}
shortest :: Monad m
    => Parser x m a -> Parser x m a -> Parser x m a
shortest p1 p2 = D.toParserK $ D.shortest (D.fromParserK p1) (D.fromParserK p2)

-- | Longest alternative. Apply both parsers in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- parse.
--
-- /Pre-release/
--
{-# INLINE longest #-}
longest :: Monad m
    => Parser x m a -> Parser x m a -> Parser x m a
longest p1 p2 = D.toParserK $ D.longest (D.fromParserK p1) (D.fromParserK p2)
-}

-- | Run a parser without consuming the input.
--
{-# INLINE lookAhead #-}
lookAhead :: Monad m => Parser a m b -> Parser a m b
lookAhead p = D.toParserK $ D.lookAhead $ D.fromParserK p

-- | Takes at-most @n@ input elements.
--
-- * Stops - when the collecting parser stops.
-- * Fails - when the collecting parser fails.
--
-- >>> Stream.parse (Parser.takeP 4 (Parser.takeEQ 2 Fold.toList)) $ Stream.fromList [1, 2, 3, 4, 5]
-- Right [1,2]
--
-- >>> Stream.parse (Parser.takeP 4 (Parser.takeEQ 5 Fold.toList)) $ Stream.fromList [1, 2, 3, 4, 5]
-- Left (ParseError "takeEQ: Expecting exactly 5 elements, input terminated on 4")
--
-- /Internal/
{-# INLINE takeP #-}
takeP :: Monad m => Int -> Parser a m b -> Parser a m b
takeP i p = D.toParserK $ D.takeP i $ D.fromParserK p

-------------------------------------------------------------------------------
-- Sequential Collection
-------------------------------------------------------------------------------
--
-- | @concatSequence f p@ collects sequential parses of parsers in a
-- serial stream @p@ using the fold @f@. Fails if the input ends or any
-- of the parsers fail.
--
-- An even more efficient implementation can use ParserD type Parser in
-- the stream.
--
-- /Pre-release/
--
{-# INLINE concatSequence #-}
concatSequence ::
    Monad m =>
    Fold m b c -> Stream m (Parser a m b) -> Parser a m c
concatSequence f p =
    let sp = fmap D.fromParserK p
        in D.toParserK $ D.sequence sp f

-- | Map a 'Parser' returning function on the result of a 'Parser'.
--
-- Compare with 'Monad' instance method '>>='. This implementation allows
-- stream fusion but has quadratic complexity. This can fuse with other
-- operations and can be much faster than 'Monad' instance for small number
-- (less than 8) of compositions.
--
-- /Pre-release/
--
{-# INLINE concatMap #-}
concatMap :: Monad m
    => (b -> Parser a m c) -> Parser a m b -> Parser a m c
concatMap f p = D.toParserK $ D.concatMap (D.fromParserK . f) (D.fromParserK p)

{-
-------------------------------------------------------------------------------
-- Alternative Collection
-------------------------------------------------------------------------------
--
-- | @choice parsers@ applies the @parsers@ in order and returns the first
-- successful parse.
--
-- This is same as 'asum' but more efficient.
--
-- /Broken/
--
{-# INLINE choice #-}
choice ::
       (Functor t, Foldable t, Monad m) => t (Parser a m b) -> Parser a m b
choice ps = D.toParserK $ D.choice $ D.fromParserK <$> ps
-}

-------------------------------------------------------------------------------
-- Sequential Repetition
-------------------------------------------------------------------------------

-- | Like 'many' but uses a 'Parser' instead of a 'Fold' to collect the
-- results. Parsing stops or fails if the collecting parser stops or fails.
--
-- /Unimplemented/
--
{-# INLINE manyP #-}
manyP :: -- Monad m =>
    Parser a m b -> Parser b m c -> Parser a m c
manyP _p _f = undefined -- D.toParserK $ D.manyP (D.fromParserK p) f

-- | Collect zero or more parses. Apply the supplied parser repeatedly on the
-- input stream and push the parse results to a downstream fold.
--
--  Stops: when the downstream fold stops or the parser fails.
--  Fails: never, produces zero or more results.
--
-- >>> many = Parser.countBetween 0 maxBound
--
-- Compare with 'Control.Applicative.many'.
--
{-# INLINE many #-}
many :: Monad m => Parser a m b -> Fold m b c -> Parser a m c
many p f = D.toParserK $ D.many (D.fromParserK p) f

-- Note: many1 would perhaps be a better name for this and consistent with
-- other names like takeWhile1. But we retain the name "some" for
-- compatibility.
--
-- | Collect one or more parses. Apply the supplied parser repeatedly on the
-- input stream and push the parse results to a downstream fold.
--
--  Stops: when the downstream fold stops or the parser fails.
--  Fails: if it stops without producing a single result.
--
-- >>> some p f = Parser.manyP p (Parser.takeGE 1 f)
-- >>> some = Parser.countBetween 1 maxBound
--
-- Compare with 'Control.Applicative.some'.
--
{-# INLINE some #-}
some :: Monad m => Parser a m b -> Fold m b c -> Parser a m c
some p f = D.toParserK $ D.some (D.fromParserK p) f
-- some p f = manyP p (takeGE 1 f)
-- some = countBetween 1 maxBound

-- | @countBetween m n f p@ collects between @m@ and @n@ sequential parses of
-- parser @p@ using the fold @f@. Stop after collecting @n@ results. Fails if
-- the input ends or the parser fails before @m@ results are collected.
--
-- >>> countBetween m n p f = Parser.manyP p (Parser.takeBetween m n f)
--
-- /Unimplemented/
--
{-# INLINE countBetween #-}
countBetween :: -- Monad m =>
    Int -> Int -> Parser a m b -> Fold m b c -> Parser a m c
countBetween _m _n _p = undefined
-- countBetween m n p f = manyP p (takeBetween m n f)

-- | @count n f p@ collects exactly @n@ sequential parses of parser @p@ using
-- the fold @f@.  Fails if the input ends or the parser fails before @n@
-- results are collected.
--
-- >>> count n = Parser.countBetween n n
-- >>> count n p f = Parser.manyP p (Parser.takeEQ n f)
--
-- /Unimplemented/
--
{-# INLINE count #-}
count :: -- Monad m =>
    Int -> Parser a m b -> Fold m b c -> Parser a m c
count n = countBetween n n
-- count n p f = manyP p (takeEQ n f)

-- | Like 'manyTill' but uses a 'Parser' to collect the results instead of a
-- 'Fold'.  Parsing stops or fails if the collecting parser stops or fails.
--
-- We can implemnent parsers like the following using 'manyTillP':
--
-- @
-- countBetweenTill m n f p = manyTillP (takeBetween m n f) p
-- @
--
-- /Unimplemented/
--
{-# INLINE manyTillP #-}
manyTillP :: -- Monad m =>
    Parser a m b -> Parser a m x -> Parser b m c -> Parser a m c
manyTillP _p1 _p2 _f = undefined
    -- D.toParserK $ D.manyTillP (D.fromParserK p1) (D.fromParserK p2) f

-- | @manyTill chunking test f@ tries the parser @test@ on the input, if @test@
-- fails it backtracks and tries @chunking@, after @chunking@ succeeds @test@ is
-- tried again and so on. The parser stops when @test@ succeeds.  The output of
-- @test@ is discarded and the output of @chunking@ is accumulated by the
-- supplied fold. The parser fails if @chunking@ fails.
--
-- Stops when the fold @f@ stops.
--
{-# INLINE manyTill #-}
manyTill :: Monad m
    => Parser a m b -- ^ Chunking parser. Parses chunks of input.
    -> Parser a m x -- ^ Test parser. Parsing stops when this parser succeeds
                    -- else backtract and run the chunking parser.
    -> Fold m b c   -- ^ Folds the output of the chunking parser.
    -> Parser a m c
manyTill collect test f =
    D.toParserK $ D.manyTill (D.fromParserK collect) (D.fromParserK test) f

-- | @manyThen f collect recover@ repeats the parser @collect@ on the input and
-- collects the output in the supplied fold. If the the parser @collect@ fails,
-- parser @recover@ is run until it stops and then we start repeating the
-- parser @collect@ again. The parser fails if the recovery parser fails.
--
-- For example, this can be used to find a key frame in a video stream after an
-- error.
--
-- /Unimplemented/
--
{-# INLINE manyThen #-}
manyThen :: -- (Foldable t, Monad m) =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
manyThen _parser _recover _f = undefined

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

-- XXX rename this to intercalate
-- | Apply two parsers alternately to an input stream. The input stream is
-- considered an interleaving of two patterns. The two parsers represent the
-- two patterns.
--
{-# INLINE deintercalate #-}
deintercalate :: Monad m =>
       Parser a m x
    -> Parser a m y
    -> Fold m (Either x y) z
    -> Parser a m z
deintercalate contentL contentR sink =
    D.toParserK
        $ D.deintercalate
            (D.fromParserK contentL) (D.fromParserK contentR) sink

-- | Parse items separated by a separator parsed by the supplied parser. At
-- least one item must be present for the parser to succeed.
--
-- Note that this can go in infinite loop if both the parsers fail on some
-- input. Detection of that would make the implementation more complex.
--
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
sepBy1 p sep sink = do
    x <- p
    f <- fromEffect $ FL.reduce sink
    f1 <- fromEffect $ FL.snoc f x
    many (sep >> p) f1

-- | Run the content parser first, when it is done, the separator parser is
-- run, when it is done content parser is run again and so on. If none of the
-- parsers consumes an input then parser returns a failure.
--
-- >>> sepBy p1 p2 sink = Parser.deintercalate p1 p2 (Fold.catLefts sink)
-- >>> sepBy content sep sink = Parser.sepBy1 content sep sink <|> return mempty
--
{-# INLINE sepBy #-}
sepBy :: Monad m =>
    Parser a m b -> Parser a m x -> Fold m b c -> Parser a m c
sepBy content sep sink =
    D.toParserK $ D.sepBy (D.fromParserK content) (D.fromParserK sep) sink
-- sepBy sink = deintercalate (FL.catLefts sink)

-------------------------------------------------------------------------------
-- Interleaving a collection of parsers
-------------------------------------------------------------------------------
--
-- | Apply a collection of parsers to an input stream in a round robin fashion.
-- Each parser is applied until it stops and then we repeat starting with the
-- the first parser again.
--
-- /Unimplemented/
--
{-# INLINE roundRobin #-}
roundRobin :: -- (Foldable t, Monad m) =>
    t (Parser a m b) -> Fold m b c -> Parser a m c
roundRobin _ps _f = undefined

-------------------------------------------------------------------------------
-- Repeated Alternatives
-------------------------------------------------------------------------------

-- | Keep trying a parser up to a maximum of @n@ failures.  When the parser
-- fails the input consumed till now is dropped and the new instance is tried
-- on the fresh input.
--
-- /Unimplemented/
--
{-# INLINE retryMaxTotal #-}
retryMaxTotal :: -- (Monad m) =>
    Int -> Parser a m b -> Fold m b c -> Parser a m c
retryMaxTotal _n _p _f  = undefined

-- | Like 'retryMaxTotal' but aborts after @n@ successive failures.
--
-- /Unimplemented/
--
{-# INLINE retryMaxSuccessive #-}
retryMaxSuccessive :: -- (Monad m) =>
    Int -> Parser a m b -> Fold m b c -> Parser a m c
retryMaxSuccessive _n _p _f = undefined

-- | Keep trying a parser until it succeeds.  When the parser fails the input
-- consumed till now is dropped and the new instance is tried on the fresh
-- input.
--
-- /Unimplemented/
--
{-# INLINE retry #-}
retry :: -- (Monad m) =>
    Parser a m b -> Parser a m b
retry _p = undefined
