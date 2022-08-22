-- |
-- Module      : Streamly.Internal.Data.Fold.Type
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- = Stream Consumers
--
-- We can classify stream consumers in the following categories in order of
-- increasing complexity and power:
--
-- * Accumulators: Tee/Zip is simple, cannot be appended, good for scanning.
-- * Terminating folds: Tee/Zip varies based on termination, can be appended,
--   good for scanning, nesting (many) is easy.
-- * Non-failing (backtracking only) parsers: cannot be used as scans because
--   of backtracking, nesting is complicated because of backtracking, appending
--   is efficient because of no Alternative, Alternative does not make sense
--   because it cannot fail.
-- * Parsers: Alternative on failure, appending is not as efficient because of
--   buffering for Alternative.
--
-- First two are represented by the 'Fold' type and the last two by the
-- 'Parser' type.
--
-- == Folds that never terminate (Accumulators)
--
-- An @Accumulator@ is the simplest type of fold, it never fails and never
-- terminates. It can always accept more inputs (never terminates) and the
-- accumulator is always valid.  For example 'Streamly.Internal.Data.Fold.sum'.
-- Traditional Haskell left folds like 'foldl' are accumulators.
--
-- Accumulators can be composed in parallel where we distribute the input
-- stream to all accumulators.  Since accumulators never terminate they cannot
-- be appended.
--
-- An accumulator can be represented as:
--
-- @
-- data Fold0 m a b =
--   forall s. Fold0
--      (s -> a -> m s) -- step
--      (m s)           -- initial
--      (s -> m b)      -- extract
-- @
--
-- This is just a traditional left fold, compare with @foldl@. The driver of
-- the fold would call @initial@ at the beginning and then keep accumulating
-- inputs into its result using @step@ and finally extract the result using
-- @extract@.
--
-- == Folds that terminate after one or more input
--
-- @Terminating folds@ are accumulators that can terminate, like accumulators
-- they do not fail. Once a fold terminates it no longer accepts any more
-- inputs.  Terminating folds can be appended, the next fold can be
-- applied after the first one terminates.  Because they cannot fail, they do
-- not need backtracking.
--
-- The 'Streamly.Internal.Data.Fold.take' operation is an example of a
-- terminating fold. It terminates after consuming @n@ items. Coupled with an
-- accumulator (e.g. sum) it can be used to process the stream into chunks of
-- fixed size.
--
-- A terminating fold can be represented as:
--
-- @
-- data Step s b
--     = Partial !s -- the fold can accept more input
--     | Done !b    -- the fold is done
--
-- data Fold1 m a b =
--   forall s. Fold1
--      (s -> a -> m (Step s b)) -- step
--      (m s)                    -- initial
--      (s -> m b)               -- extract
-- @
--
-- The fold driver stops driving the fold as soon as the fold returns a @Done@.
-- @extract@ is required only if the fold has not stopped yet and the input
-- ends. @extract@ can never be called if the fold is @Done@.
--
-- Notice that the @initial@ of `Fold1` type does not return a "Step" type,
-- therefore, it cannot say "Done" in initial. It always has to consume at
-- least one element before it can say "Done" for termination, via the @step@
-- function.
--
-- == Folds that terminate after 0 or more input
--
-- The `Fold1` type makes combinators like @take 0@ impossible to implement
-- because they need to terminate even before they can consume any elements at
-- all.  Implementing this requires the @initial@ function to be able to return
-- @Done@.
--
-- @
-- data Fold m a b =
--   forall s. Fold
--      (s -> a -> m (Step s b)) -- step
--      (m (Step s b))           -- initial
--      (s -> m b)               -- extract
-- @
--
-- This is also required if we want to compose terminating folds using an
-- Applicative or Monadic composition. @pure@ needs to yield an output without
-- having to consume an input.
--
-- @initial@ now has the ability to terminate the fold without consuming any
-- input based on the state of the monad.
--
-- In some cases it does not make sense to use a fold that does not consume any
-- items at all, and it may even lead to an infinite loop. It might make sense
-- to use a `Fold1` type for such cases because it guarantees to consume at
-- least one input, therefore, guarantees progress. For example, in
-- classifySessionsBy or any other splitting operations it may not make sense
-- to pass a fold that never consumes an input. However, we do not have a
-- separate Fold1 type for the sake of simplicity of types/API.
--
-- Adding this capability adds a certain amount of complexity in the
-- implementation of fold combinators. @initial@ has to always handle two cases
-- now.  We could potentially not implement this in folds to keep fold
-- implementation simpler, and these use cases can be transferred to the parser
-- type. However, it would be a bit inconvenient to not have a `take` operation
-- or to not be able to use `take 0` if we have it. Also, applicative and
-- monadic composition of folds would not be possible.
--
-- == Terminating Folds with backtracking
--
-- Consider the example of @takeWhile@ operation, it needs to inspect an
-- element for termination decision. However, it does not consume the element
-- on which it terminates. To implement @takeWhile@ a terminating fold will
-- have to implement a way to return the unconsumed input to the fold driver.
--
-- Single element leftover case is quite common and its easy to implement it in
-- terminating folds by adding a @Done1@ constructor in the 'Step' type which
-- indicates that the last element was not consumed by the fold. The following
-- additional operations can be implemented as terminating folds if we do that.
--
-- @
-- takeWhile
-- groupBy
-- wordBy
-- @
--
-- However, it creates several complications. The most important one is that we
-- cannot use such folds for scanning. We cannot backtrack after producing an
-- output in a scan.
--
-- === Nested backtracking
--
-- Nesting of backtracking folds increases the amount of backtracking required
-- exponentially.
--
-- For example, the combinator @many inner outer@ applies the outer fold on the
-- input stream and applies the inner fold on the results of the outer fold.
--
-- many :: Monad m => Fold m b c -> Fold m a b -> Fold m a c
--
-- If the inner fold itself returns a @Done1@ then we need to backtrack all
-- the elements that have been consumed by the outer fold to generate that
-- value. We need backtracking of more than one element.
--
-- Arbitrary backtracking requires arbitrary buffering. However, we do not want
-- to buffer unconditionally, only if the buffer is needed. One way to do this
-- is to use a "Continue" constructor like parsers. When we have nested folds,
-- the top level fold always returns a "Continue" to the driver until an output
-- is generated by it, this means the top level driver keeps buffering until an
-- output is generated via Partial or Done. Intermediate level "Continue" keep
-- propagating up to the top level.
--
-- === Parallel backtracking
--
-- In compositions like Alternative and Distributive we may have several
-- branches. Each branch can backtrack independently. We need to keep the input
-- as long as any of the branches need it. We can use a single copy of the
-- buffer and maintain it based on all the branches, or we can make each branch
-- have its own buffer. The latter approach may be simpler to implement.
-- Whenever we branch we can introduce an independent buffer for backtracking.
-- Or we can use a newtype that allows branched composition to handle
-- backtracking.
--
-- === Implementation Approach
--
-- To avoid these issues we can enforce, by using types, that the collecting
-- folds can never return a leftover.  This leads us to define a type that can
-- never return a leftover. The use cases of single leftover can be transferred
-- to parsers where we have general backtracking mechanism and single leftover
-- is just a special case of backtracking.
--
-- This means: takeWhile, groupBy, wordBy would be implemented as parsers.
--
-- A proposed design is to use the same Step type with Error in Folds as well
-- as Parsers. Folds won't use the Error constructor and even if they use, it
-- will be equivalent to just throwing an error. They won't have an
-- alternative.
--
-- Because of the complexity of implementing a distributive composition in
-- presence of backtracking we could possibly have a type without backtracking
-- but with the "Continue" constructor, and use either the Parser type or
-- another type for backtracking.
--
-- == Folds with an additional input
--
-- The `Fold` type does not allow a dynamic input to be used to generate the
-- initial value of the fold accumulator. We can extend the type further to
-- allow that:
--
-- @
-- data Refold m i a b =
--   forall s. Refold
--      (s -> a -> m (Step s b)) -- step
--      (i -> m (Step s b))      -- initial
--      (s -> m b)               -- extract
-- @
--
-- == Parsers
--
-- The next upgrade after terminating folds with a leftover are parsers.
-- Parsers are terminating folds that can fail and backtrack. Parsers can be
-- composed using an @alternative@ style composition where they can backtrack
-- and apply another parser if one parser fails.
-- 'Streamly.Internal.Data.Parser.satisfy' is a simple example of a parser, it
-- would succeed if the condition is satisfied and it would fail otherwise, on
-- failure an alternative parser can be used on the same input.
--
-- We add @Error@ and @Continue@ to the @Step@ type of fold. @Continue@ is to
-- skip producing an output or to backtrack. We also add the ability to
-- backtrack in @Partial@ and @Done@.:
--
-- Also @extract@ now needs to be able to express an error. We could have it
-- return the @Step@ type as well but that makes the implementation more
-- complicated.
--
-- @
-- data Step s b =
--       Partial Int s   -- partial result and how much to backtrack
--     | Done Int b      -- final result and how much to backtrack
--     | Continue Int s  -- no result and how much to backtrack
--     | Error String    -- error
--
-- data Parser m a b =
--   forall s. Fold
--      (s -> a -> m (Step s b))   -- step
--      (m (Step s b))             -- initial
--      (s -> m (Either String b)) -- extract
-- @
--
-- = Types for Stream Consumers
--
-- We do not have a separate type for accumulators. Terminating folds are a
-- superset of accumulators and to avoid too many types we represent both using
-- the same type, 'Fold'.
--
-- We do not club the leftovers functionality with terminating folds because of
-- the reasons explained earlier. Instead combinators that require leftovers
-- are implemented as the 'Streamly.Internal.Data.Parser.Parser' type.  This is
-- a sweet spot to balance ease of use, type safety and performance.  Using
-- separate Accumulator and terminating fold types would encode more
-- information in types but it would make ease of use, implementation,
-- maintenance effort worse. Combining Accumulator, terminating folds and
-- Parser into a single 'Streamly.Internal.Data.Parser.Parser' type would make
-- ease of use even better but type safety and performance worse.
--
-- One of the design requirements that we have placed for better ease of use
-- and code reuse is that 'Streamly.Internal.Data.Parser.Parser' type should be
-- a strict superset of the 'Fold' type i.e. it can do everything that a 'Fold'
-- can do and more. Therefore, folds can be easily upgraded to parsers and we
-- can use parser combinators on folds as well when needed.
--
-- = Fold Design
--
-- A fold is represented by a collection of "initial", "step" and "extract"
-- functions. The "initial" action generates the initial state of the fold. The
-- state is internal to the fold and maintains the accumulated output. The
-- "step" function is invoked using the current state and the next input value
-- and results in a @Partial@ or @Done@. A @Partial@ returns the next intermediate
-- state of the fold, a @Done@ indicates that the fold has terminated and
-- returns the final value of the accumulator.
--
-- Every @Partial@ indicates that a new accumulated output is available.  The
-- accumulated output can be extracted from the state at any point using
-- "extract". "extract" can never fail. A fold returns a valid output even
-- without any input i.e. even if you call "extract" on "initial" state it
-- provides an output. This is not true for parsers.
--
-- In general, "extract" is used in two cases:
--
-- * When the fold is used as a scan @extract@ is called on the intermediate
-- state every time it is yielded by the fold, the resulting value is yielded
-- as a stream.
-- * When the fold is used as a regular fold, @extract@ is called once when
-- we are done feeding input to the fold.
--
-- = Alternate Designs
--
-- An alternate and simpler design would be to return the intermediate output
-- via @Partial@ along with the state, instead of using "extract" on the yielded
-- state and remove the extract function altogether.
--
-- This may even facilitate more efficient implementation.  Extract from the
-- intermediate state after each yield may be more costly compared to the fold
-- step itself yielding the output. The fold may have more efficient ways to
-- retrieve the output rather than stuffing it in the state and using extract
-- on the state.
--
-- However, removing extract altogether may lead to less optimal code in some
-- cases because the driver of the fold needs to thread around the intermediate
-- output to return it if the stream stops before the fold could return @Done@.
-- When using this approach, the @parseMany (FL.take filesize)@ benchmark shows
-- a 2x worse performance even after ensuring everything fuses.  So we keep the
-- "extract" approach to ensure better perf in all cases.
--
-- But we could still yield both state and the output in @Partial@, the output
-- can be used for the scan use case, instead of using extract. Extract would
-- then be used only for the case when the stream stops before the fold
-- completes.
--
-- = Monoids
--
-- Monoids allow generalized, modular folding.  The accumulators in this module
-- can be expressed using 'mconcat' and a suitable 'Monoid'.  Instead of
-- writing folds we can write Monoids and turn them into folds.
--
-- = Performance Notes
--
-- 'Streamly.Prelude' module provides fold functions to directly fold streams
-- e.g.  Streamly.Prelude/'Streamly.Prelude.sum' serves the same purpose as
-- Fold/'sum'.  However, the functions in Streamly.Prelude cannot be
-- efficiently combined together e.g. we cannot drive the input stream through
-- @sum@ and @length@ fold functions simultaneously.  Using the 'Fold' type we
-- can efficiently split the stream across multiple folds because it allows the
-- compiler to perform stream fusion optimizations.
--
module Streamly.Internal.Data.Fold.Type
    (
    -- * Types
      Step (..)
    , Fold (..)

    -- * Constructors
    , foldl'
    , foldlM'
    , foldl1'
    , foldt'
    , foldtM'
    , foldr
    , foldrM

    -- * Folds
    , fromPure
    , fromEffect
    , fromRefold
    , drain
    , toList
    , toStreamK
    , toStreamKRev

    -- * Combinators

    -- ** Mapping output
    , rmapM

    -- ** Mapping Input
    , lmap
    , lmapM
    , postscan

    -- ** Filtering
    , catMaybes
    , scanMaybe
    , filter
    , filtering
    , filterM
    , lefts
    , rights
    , both

    -- ** Trimming
    , take
    , taking
    , dropping

    -- ** Sequential application
    , serialWith -- rename to "append"
    , serial_

    -- ** Repeated Application (Splitting)
    , ManyState
    , many
    , manyPost
    , chunksOf
    , refoldMany
    , refoldMany1

    -- ** Nested Application
    , concatMap
    , refold

    -- ** Parallel Distribution
    , teeWith
    , teeWithFst
    , teeWithMin

    -- ** Parallel Alternative
    , shortest
    , longest

    -- * Running A Fold
    , initialize
    , snoc
    , duplicate
    , finish
    , isDone
    )
where

#include "inline.hs"

import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor(..))
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Step (Step(..), mapMStep, chainStepM)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Refold.Type (Refold(..))

import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (concatMap, filter, foldr, map, take)

-- $setup
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> :set -package streamly
-- >>> import Data.Maybe (fromJust, isJust)
-- >>> import Streamly.Data.Fold (Fold)
-- >>> import Streamly.Internal.Data.Stream.Type (Stream)
-- >>> import qualified Data.Foldable as Foldable
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.StreamK as StreamK
-- >>> import Prelude hiding (concatMap, filter, map)

------------------------------------------------------------------------------
-- The Fold type
------------------------------------------------------------------------------

-- An fold is akin to a writer. It is the streaming equivalent of a writer.
-- The type @b@ is the accumulator of the writer. That's the reason the
-- default folds in various modules are called "write".

-- | The type @Fold m a b@ having constructor @Fold step initial extract@
-- represents a fold over an input stream of values of type @a@ to a final
-- value of type @b@ in 'Monad' @m@.
--
-- The fold uses an intermediate state @s@ as accumulator, the type @s@ is
-- internal to the specific fold definition. The initial value of the fold
-- state @s@ is returned by @initial@. The @step@ function consumes an input
-- and either returns the final result @b@ if the fold is done or the next
-- intermediate state (see 'Step'). At any point the fold driver can extract
-- the result from the intermediate state using the @extract@ function.
--
-- NOTE: The constructor is not yet exposed via exposed modules, smart
-- constructors are provided to create folds.  If you think you need the
-- constructor of this type please consider using the smart constructors in
-- "Streamly.Internal.Data.Fold" instead.
--
-- /since 0.8.0 (type changed)/
--
-- @since 0.7.0

data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall s. Fold (s -> a -> m (Step s b)) (m (Step s b)) (s -> m b)

------------------------------------------------------------------------------
-- Mapping on the output
------------------------------------------------------------------------------

-- | Map a monadic function on the output of a fold.
--
-- @since 0.8.0
{-# INLINE rmapM #-}
rmapM :: Monad m => (b -> m c) -> Fold m a b -> Fold m a c
rmapM f (Fold step initial extract) = Fold step1 initial1 (extract >=> f)

    where

    initial1 = initial >>= mapMStep f
    step1 s a = step s a >>= mapMStep f

------------------------------------------------------------------------------
-- Left fold constructors
------------------------------------------------------------------------------

-- | Make a fold from a left fold style pure step function and initial value of
-- the accumulator.
--
-- If your 'Fold' returns only 'Partial' (i.e. never returns a 'Done') then you
-- can use @foldl'*@ constructors.
--
-- A fold with an extract function can be expressed using fmap:
--
-- @
-- mkfoldlx :: Monad m => (s -> a -> s) -> s -> (s -> b) -> Fold m a b
-- mkfoldlx step initial extract = fmap extract (foldl' step initial)
-- @
--
-- See also: @Streamly.Prelude.foldl'@
--
-- @since 0.8.0
--
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Fold m a b
foldl' step initial =
    Fold
        (\s a -> return $ Partial $ step s a)
        (return (Partial initial))
        return

-- | Make a fold from a left fold style monadic step function and initial value
-- of the accumulator.
--
-- A fold with an extract function can be expressed using rmapM:
--
-- @
-- mkFoldlxM :: Functor m => (s -> a -> m s) -> m s -> (s -> m b) -> Fold m a b
-- mkFoldlxM step initial extract = rmapM extract (foldlM' step initial)
-- @
--
-- See also: @Streamly.Prelude.foldlM'@
--
-- @since 0.8.0
--
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> Fold m a b
foldlM' step initial =
    Fold (\s a -> Partial <$> step s a) (Partial <$> initial) return

-- | Make a strict left fold, for non-empty streams, using first element as the
-- starting value. Returns Nothing if the stream is empty.
--
-- See also: @Streamly.Prelude.foldl1'@
--
-- /Pre-release/
{-# INLINE foldl1' #-}
foldl1' :: Monad m => (a -> a -> a) -> Fold m a (Maybe a)
foldl1' step = fmap toMaybe $ foldl' step1 Nothing'

    where

    step1 Nothing' a = Just' a
    step1 (Just' x) a = Just' $ step x a

------------------------------------------------------------------------------
-- Right fold constructors
------------------------------------------------------------------------------

-- | Make a fold using a right fold style step function and a terminal value.
-- It performs a strict right fold via a left fold using function composition.
-- Note that this is strict fold, it can only be useful for constructing strict
-- structures in memory. For reductions this will be very inefficient.
--
-- For example,
--
-- > toList = foldr (:) []
--
-- See also: 'Streamly.Prelude.foldr'
--
-- @since 0.8.0
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Fold m a b
foldr g z = fmap ($ z) $ foldl' (\f x -> f . g x) id

-- XXX we have not seen any use of this yet, not releasing until we have a use
-- case.
--
-- | Like 'foldr' but with a monadic step function.
--
-- For example,
--
-- > toList = foldrM (\a xs -> return $ a : xs) (return [])
--
-- See also: 'Streamly.Prelude.foldrM'
--
-- /Pre-release/
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> m b -> Fold m a b
foldrM g z =
    rmapM (z >>=) $ foldlM' (\f x -> return $ g x >=> f) (return return)

------------------------------------------------------------------------------
-- General fold constructors
------------------------------------------------------------------------------

-- XXX If the Step yield gives the result each time along with the state then
-- we can make the type of this as
--
-- mkFold :: Monad m => (s -> a -> Step s b) -> Step s b -> Fold m a b
--
-- Then similar to foldl' and foldr we can just fmap extract on it to extend
-- it to the version where an 'extract' function is required. Or do we even
-- need that?
--
-- Until we investigate this we are not releasing these.
--
-- XXX The above text would apply to
-- Streamly.Internal.Data.Parser.ParserD.Type.parser

-- | Make a terminating fold using a pure step function, a pure initial state
-- and a pure state extraction function.
--
-- /Pre-release/
--
{-# INLINE foldt' #-}
foldt' :: Monad m => (s -> a -> Step s b) -> Step s b -> (s -> b) -> Fold m a b
foldt' step initial extract =
    Fold (\s a -> return $ step s a) (return initial) (return . extract)

-- | Make a terminating fold with an effectful step function and initial state,
-- and a state extraction function.
--
-- >>> foldtM' = Fold.Fold
--
--  We can just use 'Fold' but it is provided for completeness.
--
-- /Pre-release/
--
{-# INLINE foldtM' #-}
foldtM' :: (s -> a -> m (Step s b)) -> m (Step s b) -> (s -> m b) -> Fold m a b
foldtM' = Fold

------------------------------------------------------------------------------
-- Refold
------------------------------------------------------------------------------

-- This is similar to how we run an Unfold to generate a Stream. A Fold is like
-- a Stream and a Fold2 is like an Unfold.
--
-- | Make a fold from a consumer.
--
-- /Internal/
fromRefold :: Refold m c a b -> c -> Fold m a b
fromRefold (Refold step inject extract) c =
    Fold step (inject c) extract

------------------------------------------------------------------------------
-- Basic Folds
------------------------------------------------------------------------------

-- | A fold that drains all its input, running the effects and discarding the
-- results.
--
-- > drain = drainBy (const (return ()))
--
-- @since 0.7.0
{-# INLINE drain #-}
drain :: Monad m => Fold m a ()
drain = foldl' (\_ _ -> ()) ()

-- | Folds the input stream to a list.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Data.Array.Unboxed"
-- instead.
--
-- > toList = foldr (:) []
--
-- @since 0.7.0
{-# INLINE toList #-}
toList :: Monad m => Fold m a [a]
toList = foldr (:) []

-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- >>> toStreamKRev = Foldable.foldl' (flip StreamK.cons) StreamK.nil
--
-- This is more efficient than 'toStreamK'. toStreamK has exactly the same
-- performance as reversing the stream after toStreamKRev.
--
-- /Pre-release/

--  xn : ... : x2 : x1 : []
{-# INLINE toStreamKRev #-}
toStreamKRev :: Monad m => Fold m a (K.Stream n a)
toStreamKRev = foldl' (flip K.cons) K.nil

-- | A fold that buffers its input to a pure stream.
--
-- >>> toStreamK = foldr StreamK.cons StreamK.nil
-- >>> toStreamK = fmap StreamK.reverse Fold.toStreamKRev
--
-- /Internal/
{-# INLINE toStreamK #-}
toStreamK :: Monad m => Fold m a (K.Stream n a)
toStreamK = foldr K.cons K.nil

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step1 initial1 extract) = Fold step initial (fmap2 f extract)

        where

        initial = fmap2 f initial1
        step s b = fmap2 f (step1 s b)
        fmap2 g = fmap (fmap g)

-- This is the dual of stream "fromPure".
--
-- | A fold that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: Applicative m => b -> Fold m a b
fromPure b = Fold undefined (pure $ Done b) pure

-- This is the dual of stream "fromEffect".
--
-- | A fold that always yields the result of an effectful action without
-- consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Applicative m => m b -> Fold m a b
fromEffect b = Fold undefined (Done <$> b) pure

{-# ANN type SeqFoldState Fuse #-}
data SeqFoldState sl f sr = SeqFoldL !sl | SeqFoldR !f !sr

-- | Sequential fold application. Apply two folds sequentially to an input
-- stream.  The input is provided to the first fold, when it is done - the
-- remaining input is provided to the second fold. When the second fold is done
-- or if the input stream is over, the outputs of the two folds are combined
-- using the supplied function.
--
-- >>> f = Fold.serialWith (,) (Fold.take 8 Fold.toList) (Fold.takeEndBy (== '\n') Fold.toList)
-- >>> Stream.fold f $ Stream.fromList "header: hello\n"
-- ("header: ","hello\n")
--
-- Note: This is dual to appending streams using 'Streamly.Prelude.serial'.
--
-- Note: this implementation allows for stream fusion but has quadratic time
-- complexity, because each composition adds a new branch that each subsequent
-- fold's input element has to traverse, therefore, it cannot scale to a large
-- number of compositions. After around 100 compositions the performance starts
-- dipping rapidly compared to a CPS style implementation.
--
-- /Time: O(n^2) where n is the number of compositions./
--
-- @since 0.8.0
--
{-# INLINE serialWith #-}
serialWith :: Monad m =>
    (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
serialWith func (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runR #-}
    runR action f = bimap (SeqFoldR f) f <$> action

    {-# INLINE runL #-}
    runL action = do
        resL <- action
        chainStepM (return . SeqFoldL) (runR initialR . func) resL

    initial = runL initialL

    step (SeqFoldL st) a = runL (stepL st a)
    step (SeqFoldR f st) a = runR (stepR st a) f

    extract (SeqFoldR f sR) = fmap f (extractR sR)
    extract (SeqFoldL sL) = do
        rL <- extractL sL
        res <- initialR
        fmap (func rL)
            $ case res of
                Partial sR -> extractR sR
                Done rR -> return rR

{-# ANN type SeqFoldState_ Fuse #-}
data SeqFoldState_ sl sr = SeqFoldL_ !sl | SeqFoldR_ !sr

-- | Same as applicative '*>'. Run two folds serially one after the other
-- discarding the result of the first.
--
-- This was written in the hope that it might be faster than implementing it
-- using serialWith, but the current benchmarks show that it has the same
-- performance. So do not expose it unless some benchmark shows benefit.
--
{-# INLINE serial_ #-}
serial_ :: Monad m => Fold m x a -> Fold m x b -> Fold m x b
serial_ (Fold stepL initialL _) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            Partial sl -> return $ Partial $ SeqFoldL_ sl
            Done _ -> do
                resR <- initialR
                return $ first SeqFoldR_ resR

    step (SeqFoldL_ st) a = do
        r <- stepL st a
        case r of
            Partial s -> return $ Partial (SeqFoldL_ s)
            Done _ -> do
                resR <- initialR
                return $ first SeqFoldR_ resR
    step (SeqFoldR_ st) a = do
        resR <- stepR st a
        return $ first SeqFoldR_ resR

    extract (SeqFoldR_ sR) = extractR sR
    extract (SeqFoldL_ _) = do
        res <- initialR
        case res of
            Partial sR -> extractR sR
            Done rR -> return rR

{-# ANN type TeeState Fuse #-}
data TeeState sL sR bL bR
    = TeeBoth !sL !sR
    | TeeLeft !bR !sL
    | TeeRight !bL !sR

-- | @teeWith k f1 f2@ distributes its input to both @f1@ and @f2@ until both
-- of them terminate and combines their output using @k@.
--
-- >>> avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> Stream.fold avg $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- > teeWith k f1 f2 = fmap (uncurry k) ((Fold.tee f1 f2)
--
-- For applicative composition using this combinator see
-- "Streamly.Internal.Data.Fold.Tee".
--
-- See also: "Streamly.Internal.Data.Fold.Tee"
--
-- @since 0.8.0
--
{-# INLINE teeWith #-}
teeWith :: Monad m => (a -> b -> c) -> Fold m x a -> Fold m x b -> Fold m x c
teeWith f (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        return
            $ case resL of
                  Partial sl ->
                      Partial
                          $ case resR of
                                Partial sr -> TeeBoth sl sr
                                Done br -> TeeLeft br sl
                  Done bl -> bimap (TeeRight bl) (f bl) resR

    initial = runBoth initialL initialR

    step (TeeBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
    step (TeeLeft bR sL) a = bimap (TeeLeft bR) (`f` bR) <$> stepL sL a
    step (TeeRight bL sR) a = bimap (TeeRight bL) (f bL) <$> stepR sR a

    extract (TeeBoth sL sR) = f <$> extractL sL <*> extractR sR
    extract (TeeLeft bR sL) = (`f` bR) <$> extractL sL
    extract (TeeRight bL sR) = f bL <$> extractR sR

{-# ANN type TeeFstState Fuse #-}
data TeeFstState sL sR b
    = TeeFstBoth !sL !sR
    | TeeFstLeft !b !sL

-- | Like 'teeWith' but terminates as soon as the first fold terminates.
--
-- /Pre-release/
--
{-# INLINE teeWithFst #-}
teeWithFst :: Monad m =>
    (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithFst f (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR

        case resL of
            Partial sl ->
                return
                    $ Partial
                    $ case resR of
                        Partial sr -> TeeFstBoth sl sr
                        Done br -> TeeFstLeft br sl
            Done bl -> do
                Done . f bl <$>
                    case resR of
                        Partial sr -> extractR sr
                        Done br -> return br

    initial = runBoth initialL initialR

    step (TeeFstBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
    step (TeeFstLeft bR sL) a = bimap (TeeFstLeft bR) (`f` bR) <$> stepL sL a

    extract (TeeFstBoth sL sR) = f <$> extractL sL <*> extractR sR
    extract (TeeFstLeft bR sL) = (`f` bR) <$> extractL sL

-- | Like 'teeWith' but terminates as soon as any one of the two folds
-- terminates.
--
-- /Pre-release/
--
{-# INLINE teeWithMin #-}
teeWithMin :: Monad m =>
    (b -> c -> d) -> Fold m a b -> Fold m a c -> Fold m a d
teeWithMin f (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        case resL of
            Partial sl -> do
                case resR of
                    Partial sr -> return $ Partial $ Tuple' sl sr
                    Done br -> Done . (`f` br) <$> extractL sl

            Done bl -> do
                Done . f bl <$>
                    case resR of
                        Partial sr -> extractR sr
                        Done br -> return br

    initial = runBoth initialL initialR

    step (Tuple' sL sR) a = runBoth (stepL sL a) (stepR sR a)

    extract (Tuple' sL sR) = f <$> extractL sL <*> extractR sR

-- | Shortest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed least input i.e. take the shortest succeeding
-- fold.
--
-- If both the folds finish at the same time or if the result is extracted
-- before any of the folds could finish then the left one is taken.
--
-- /Pre-release/
--
{-# INLINE shortest #-}
shortest :: Monad m => Fold m x a -> Fold m x b -> Fold m x (Either a b)
shortest (Fold stepL initialL extractL) (Fold stepR initialR _) =
    Fold step initial extract

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        return $
            case resL of
                Partial sL -> bimap (Tuple' sL) Right resR
                Done bL -> Done $ Left bL

    initial = runBoth initialL initialR

    step (Tuple' sL sR) a = runBoth (stepL sL a) (stepR sR a)

    extract (Tuple' sL _) = Left <$> extractL sL

{-# ANN type LongestState Fuse #-}
data LongestState sL sR
    = LongestBoth !sL !sR
    | LongestLeft !sL
    | LongestRight !sR

-- | Longest alternative. Apply both folds in parallel but choose the result
-- from the one which consumed more input i.e. take the longest succeeding
-- fold.
--
-- If both the folds finish at the same time or if the result is extracted
-- before any of the folds could finish then the left one is taken.
--
-- /Pre-release/
--
{-# INLINE longest #-}
longest :: Monad m => Fold m x a -> Fold m x b -> Fold m x (Either a b)
longest (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runBoth #-}
    runBoth actionL actionR = do
        resL <- actionL
        resR <- actionR
        return $
            case resL of
                Partial sL ->
                    Partial $
                        case resR of
                            Partial sR -> LongestBoth sL sR
                            Done _ -> LongestLeft sL
                Done bL -> bimap LongestRight (const (Left bL)) resR

    initial = runBoth initialL initialR

    step (LongestBoth sL sR) a = runBoth (stepL sL a) (stepR sR a)
    step (LongestLeft sL) a = bimap LongestLeft Left <$> stepL sL a
    step (LongestRight sR) a = bimap LongestRight Right <$> stepR sR a

    left sL = Left <$> extractL sL
    extract (LongestLeft sL) = left sL
    extract (LongestRight sR) = Right <$> extractR sR
    extract (LongestBoth sL _) = left sL

data ConcatMapState m sa a c
    = B !sa
    | forall s. C (s -> a -> m (Step s c)) !s (s -> m c)

-- | Map a 'Fold' returning function on the result of a 'Fold' and run the
-- returned fold. This operation can be used to express data dependencies
-- between fold operations.
--
-- Let's say the first element in the stream is a count of the following
-- elements that we have to add, then:
--
-- >>> import Data.Maybe (fromJust)
-- >>> count = fmap fromJust Fold.head
-- >>> total n = Fold.take n Fold.sum
-- >>> Stream.fold (Fold.concatMap total count) $ Stream.fromList [10,9..1]
-- 45
--
-- This does not fuse completely, see 'refold' for a fusible alternative.
--
-- /Time: O(n^2) where @n@ is the number of compositions./
--
-- See also: 'Streamly.Internal.Data.Stream.foldIterateM', 'refold'
--
-- @since 0.8.0
--
{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Fold m a c) -> Fold m a b -> Fold m a c
concatMap f (Fold stepa initiala extracta) = Fold stepc initialc extractc
  where
    initialc = do
        r <- initiala
        case r of
            Partial s -> return $ Partial (B s)
            Done b -> initInnerFold (f b)

    stepc (B s) a = do
        r <- stepa s a
        case r of
            Partial s1 -> return $ Partial (B s1)
            Done b -> initInnerFold (f b)

    stepc (C stepInner s extractInner) a = do
        r <- stepInner s a
        return $ case r of
            Partial sc -> Partial (C stepInner sc extractInner)
            Done c -> Done c

    extractc (B s) = do
        r <- extracta s
        initExtract (f r)
    extractc (C _ sInner extractInner) = extractInner sInner

    initInnerFold (Fold step i e) = do
        r <- i
        return $ case r of
            Partial s -> Partial (C step s e)
            Done c -> Done c

    initExtract (Fold _ i e) = do
        r <- i
        case r of
            Partial s -> e s
            Done c -> return c

------------------------------------------------------------------------------
-- Mapping on input
------------------------------------------------------------------------------

-- | @lmap f fold@ maps the function @f@ on the input of the fold.
--
-- >>> Stream.fold (Fold.lmap (\x -> x * x) Fold.sum) (Stream.enumerateFromTo 1 100)
-- 338350
--
-- > lmap = Fold.lmapM return
--
-- @since 0.8.0
{-# INLINE lmap #-}
lmap :: (a -> b) -> Fold m b r -> Fold m a r
lmap f (Fold step begin done) = Fold step' begin done
    where
    step' x a = step x (f a)

-- | @lmapM f fold@ maps the monadic function @f@ on the input of the fold.
--
-- @since 0.8.0
{-# INLINE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = f a >>= step x

-- | Postscan the input of a 'Fold' to change it in a stateful manner using
-- another 'Fold'.
--
-- @postscan scanner collector@
--
-- /Pre-release/
{-# INLINE postscan #-}
postscan :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
postscan (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    {-# INLINE runStep #-}
    runStep actionL sR = do
        rL <- actionL
        case rL of
            Done bL -> do
                rR <- stepR sR bL
                case rR of
                    Partial sR1 -> Done <$> extractR sR1
                    Done bR -> return $ Done bR
            Partial sL -> do
                !b <- extractL sL
                rR <- stepR sR b
                return
                    $ case rR of
                        Partial sR1 -> Partial (sL, sR1)
                        Done bR -> Done bR

    initial = do
        r <- initialR
        rL <- initialL
        case r of
            Partial sR ->
                case rL of
                    Done _ -> Done <$> extractR sR
                    Partial sL -> return $ Partial (sL, sR)
            Done b -> return $ Done b

    step (sL, sR) x = runStep (stepL sL x) sR

    extract = extractR . snd

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Modify a fold to receive a 'Maybe' input, the 'Just' values are unwrapped
-- and sent to the original fold, 'Nothing' values are discarded.
--
-- >>> catMaybes = Fold.mapMaybe id
-- >>> catMaybes = Fold.filter isJust . Fold.lmap fromJust
--
-- @since 0.8.0
{-# INLINE_NORMAL catMaybes #-}
catMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
catMaybes (Fold step initial extract) = Fold step1 initial extract

    where

    step1 s a =
        case a of
            Nothing -> return $ Partial s
            Just x -> step s x

-- | Use a 'Maybe' returning fold as a filtering scan.
--
-- >>> scanMaybe p f = Fold.postscan p (Fold.catMaybes f)
--
-- /Pre-release/
{-# INLINE scanMaybe #-}
scanMaybe :: Monad m => Fold m a (Maybe b) -> Fold m b c -> Fold m a c
scanMaybe f1 f2 = postscan f1 (catMaybes f2)

-- | A scanning fold for filtering elements based on a predicate.
--
{-# INLINE filtering #-}
filtering :: Monad m => (a -> Bool) -> Fold m a (Maybe a)
filtering f = foldl' step Nothing

    where

    step _ a = if f a then Just a else Nothing

-- | Include only those elements that pass a predicate.
--
-- >>> Stream.fold (Fold.filter (> 5) Fold.sum) $ Stream.fromList [1..10]
-- 40
--
-- >>> filter p = Fold.scanMaybe (Fold.filtering p)
-- >>> filter p = Fold.filterM (return . p)
-- >>> filter p = Fold.mapMaybe (\x -> if p x then Just x else Nothing)
--
-- @since 0.8.0
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
-- filter p = scanMaybe (filtering p)
filter f (Fold step begin done) = Fold step' begin done
    where
    step' x a = if f a then step x a else return $ Partial x

-- | Like 'filter' but with a monadic predicate.
--
-- >>> f p x = p x >>= \r -> return $ if r then Just x else Nothing
-- >>> filterM p = Fold.mapMaybeM (f p)
--
-- @since 0.8.0
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
filterM f (Fold step begin done) = Fold step' begin done
    where
    step' x a = do
      use <- f a
      if use then step x a else return $ Partial x

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE lefts #-}
lefts :: (Monad m) => Fold m a c -> Fold m (Either a b) c
lefts = filter isLeft . lmap (fromLeft undefined)

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE rights #-}
rights :: (Monad m) => Fold m b c -> Fold m (Either a b) c
rights = filter isRight . lmap (fromRight undefined)

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- /Pre-release/
--
{-# INLINE both #-}
both :: Fold m a b -> Fold m (Either a a) b
both = lmap (either id id)

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- Required to fuse "take" with "many" in "chunksOf", for ghc-9.x
{-# ANN type Tuple'Fused Fuse #-}
data Tuple'Fused a b = Tuple'Fused !a !b deriving Show

{-# INLINE taking #-}
taking :: Monad m => Int -> Fold m a (Maybe a)
taking n = foldt' step initial extract

    where

    initial =
        if n <= 0
        then Done Nothing
        else Partial (Tuple'Fused n Nothing)

    step (Tuple'Fused i _) a =
        if i > 1
        then Partial (Tuple'Fused (i - 1) (Just a))
        else Done (Just a)

    extract (Tuple'Fused _ r) = r

{-# INLINE dropping #-}
dropping :: Monad m => Int -> Fold m a (Maybe a)
dropping n = foldt' step initial extract

    where

    initial = Partial (Tuple'Fused n Nothing)

    step (Tuple'Fused i _) a =
        if i > 0
        then Partial (Tuple'Fused (i - 1) Nothing)
        else Partial (Tuple'Fused i (Just a))

    extract (Tuple'Fused _ r) = r

-- | Take at most @n@ input elements and fold them using the supplied fold. A
-- negative count is treated as 0.
--
-- >>> Stream.fold (Fold.take 2 Fold.toList) $ Stream.fromList [1..10]
-- [1,2]
--
-- @since 0.8.0
{-# INLINE take #-}
take :: Monad m => Int -> Fold m a b -> Fold m a b
-- take n = scanMaybe (taking n)
take n (Fold fstep finitial fextract) = Fold step initial extract

    where

    {-# INLINE next #-}
    next i res =
        case res of
            Partial s -> do
                let i1 = i + 1
                    s1 = Tuple'Fused i1 s
                if i1 < n
                then return $ Partial s1
                else Done <$> fextract s
            Done b -> return $ Done b

    initial = finitial >>= next (-1)

    step (Tuple'Fused i r) a = fstep r a >>= next i

    extract (Tuple'Fused _ r) = fextract r

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | 'duplicate' provides the ability to run a fold in parts.  The duplicated
-- fold consumes the input and returns the same fold as output instead of
-- returning the final result, the returned fold can be run later to consume
-- more input.
--
-- We can append a stream to a fold as follows:
--
-- >>> :{
-- foldAppend :: Monad m => Fold m a b -> Stream m a -> m (Fold m a b)
-- foldAppend f = Stream.fold (Fold.duplicate f)
-- :}
--
-- >>> :{
-- do
--  sum1 <- foldAppend Fold.sum (Stream.enumerateFromTo 1 10)
--  sum2 <- foldAppend sum1 (Stream.enumerateFromTo 11 20)
--  Stream.fold sum2 (Stream.enumerateFromTo 21 30)
-- :}
-- 465
--
-- 'duplicate' essentially appends a stream to the fold without finishing the
-- fold.  Compare with 'snoc' which appends a singleton value to the fold.
--
-- /Pre-release/
{-# INLINE duplicate #-}
duplicate :: Monad m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step1 initial1 extract1) =
    Fold step initial (\s -> pure $ Fold step1 (pure $ Partial s) extract1)

    where

    initial = second fromPure <$> initial1

    step s a = second fromPure <$> step1 s a

-- | Run the initialization effect of a fold. The returned fold would use the
-- value returned by this effect as its initial value.
--
-- /Pre-release/
{-# INLINE initialize #-}
initialize :: Monad m => Fold m a b -> m (Fold m a b)
initialize (Fold step initial extract) = do
    i <- initial
    return $ Fold step (return i) extract

-- | Append a singleton value to the fold.
--
-- >>> import qualified Data.Foldable as Foldable
-- >>> Foldable.foldlM Fold.snoc Fold.toList [1..3] >>= Fold.finish
-- [1,2,3]
--
-- Compare with 'duplicate' which allows appending a stream to the fold.
--
-- /Pre-release/
{-# INLINE snoc #-}
snoc :: Monad m => Fold m a b -> a -> m (Fold m a b)
snoc (Fold step initial extract) a = do
    res <- initial
    r <- case res of
          Partial fs -> step fs a
          Done _ -> return res
    return $ Fold step (return r) extract

-- | Finish the fold to extract the current value of the fold.
--
-- >>> Fold.finish Fold.toList
-- []
--
-- /Pre-release/
{-# INLINE finish #-}
finish :: Monad m => Fold m a b -> m b
finish (Fold _ initial extract) = do
    res <- initial
    case res of
          Partial fs -> extract fs
          Done b -> return b

-- | Check if the fold is done and can take no more input.
--
-- /Pre-release/
{-# INLINE isDone #-}
isDone :: Monad m => Fold m a b -> m (Maybe b)
isDone (Fold _ initial _) = do
    res <- initial
    return $ case res of
          Partial _ -> Nothing
          Done b -> Just b

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- All the grouping transformation that we apply to a stream can also be
-- applied to a fold input stream. groupBy et al can be written as terminating
-- folds and then we can apply "many" to use those repeatedly on a stream.

{-# ANN type ManyState Fuse #-}
data ManyState s1 s2
    = ManyFirst !s1 !s2
    | ManyLoop !s1 !s2

-- | Collect zero or more applications of a fold.  @many split collect@ applies
-- the @split@ fold repeatedly on the input stream and accumulates zero or more
-- fold results using @collect@.
--
-- >>> two = Fold.take 2 Fold.toList
-- >>> twos = Fold.many two Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- Stops when @collect@ stops.
--
-- See also: 'Streamly.Prelude.concatMap', 'Streamly.Prelude.foldMany'
--
-- @since 0.8.0
--
{-# INLINE many #-}
many :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
many (Fold sstep sinitial sextract) (Fold cstep cinitial cextract) =
    Fold step initial extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split f cs sres =
        case sres of
            Partial ss -> return $ Partial $ f ss cs
            Done sb -> cstep cs sb >>= collect

    collect cres =
        case cres of
            Partial cs -> sinitial >>= split ManyFirst cs
            Done cb -> return $ Done cb

    -- A fold may terminate even without accepting a single input.  So we run
    -- the split fold's initial action even if no input is received.  However,
    -- this means that if no input was ever received by "step" we discard the
    -- fold's initial result which could have generated an effect. However,
    -- note that if "sinitial" results in Done we do collect its output even
    -- though the fold may not have received any input. XXX Is this
    -- inconsistent?
    initial = cinitial >>= collect

    {-# INLINE step_ #-}
    step_ ss cs a = sstep ss a >>= split ManyLoop cs

    {-# INLINE step #-}
    step (ManyFirst ss cs) a = step_ ss cs a
    step (ManyLoop ss cs) a = step_ ss cs a

    -- Do not extract the split fold if no item was consumed.
    extract (ManyFirst _ cs) = cextract cs
    extract (ManyLoop ss cs) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | Like many, but inner fold emits an output at the end even if no input is
-- received.
--
-- /Internal/
--
-- /See also: 'Streamly.Prelude.concatMap', 'Streamly.Prelude.foldMany'/
--
{-# INLINE manyPost #-}
manyPost :: Monad m => Fold m a b -> Fold m b c -> Fold m a c
manyPost (Fold sstep sinitial sextract) (Fold cstep cinitial cextract) =
    Fold step initial extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split cs sres =
        case sres of
            Partial ss1 -> return $ Partial $ Tuple' ss1 cs
            Done sb -> cstep cs sb >>= collect

    collect cres =
        case cres of
            Partial cs -> sinitial >>= split cs
            Done cb -> return $ Done cb

    initial = cinitial >>= collect

    {-# INLINE step #-}
    step (Tuple' ss cs) a = sstep ss a >>= split cs

    extract (Tuple' ss cs) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | @chunksOf n split collect@ repeatedly applies the @split@ fold to chunks
-- of @n@ items in the input stream and supplies the result to the @collect@
-- fold.
--
-- >>> twos = Fold.chunksOf 2 Fold.toList Fold.toList
-- >>> Stream.fold twos $ Stream.fromList [1..10]
-- [[1,2],[3,4],[5,6],[7,8],[9,10]]
--
-- > chunksOf n split = many (take n split)
--
-- Stops when @collect@ stops.
--
-- @since 0.8.0
--
{-# INLINE chunksOf #-}
chunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
chunksOf n split = many (take n split)

------------------------------------------------------------------------------
-- Refold and Fold Combinators
------------------------------------------------------------------------------

-- | Like 'many' but uses a 'Refold' for collecting.
--
{-# INLINE refoldMany #-}
refoldMany :: Monad m => Fold m a b -> Refold m x b c -> Refold m x a c
refoldMany (Fold sstep sinitial sextract) (Refold cstep cinject cextract) =
    Refold step inject extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split cs f sres =
        case sres of
            Partial ss -> return $ Partial $ Tuple' cs (f ss)
            Done sb -> cstep cs sb >>= collect

    collect cres =
        case cres of
            Partial cs -> sinitial >>= split cs Left
            Done cb -> return $ Done cb

    inject x = cinject x >>= collect

    {-# INLINE step_ #-}
    step_ ss cs a = sstep ss a >>= split cs Right

    {-# INLINE step #-}
    step (Tuple' cs (Left ss)) a = step_ ss cs a
    step (Tuple' cs (Right ss)) a = step_ ss cs a

    -- Do not extract the split fold if no item was consumed.
    extract (Tuple' cs (Left _)) = cextract cs
    extract (Tuple' cs (Right ss )) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

{-# ANN type ConsumeManyState Fuse #-}
data ConsumeManyState x cs ss = ConsumeMany x cs (Either ss ss)

-- | Like 'many' but uses a 'Refold' for splitting.
--
-- /Internal/
{-# INLINE refoldMany1 #-}
refoldMany1 :: Monad m => Refold m x a b -> Fold m b c -> Refold m x a c
refoldMany1 (Refold sstep sinject sextract) (Fold cstep cinitial cextract) =
    Refold step inject extract

    where

    -- cs = collect state
    -- ss = split state
    -- cres = collect state result
    -- sres = split state result
    -- cb = collect done
    -- sb = split done

    -- Caution! There is mutual recursion here, inlining the right functions is
    -- important.

    {-# INLINE split #-}
    split x cs f sres =
        case sres of
            Partial ss -> return $ Partial $ ConsumeMany x cs (f ss)
            Done sb -> cstep cs sb >>= collect x

    collect x cres =
        case cres of
            Partial cs -> sinject x >>= split x cs Left
            Done cb -> return $ Done cb

    inject x = cinitial >>= collect x

    {-# INLINE step_ #-}
    step_ x ss cs a = sstep ss a >>= split x cs Right

    {-# INLINE step #-}
    step (ConsumeMany x cs (Left ss)) a = step_ x ss cs a
    step (ConsumeMany x cs (Right ss)) a = step_ x ss cs a

    -- Do not extract the split fold if no item was consumed.
    extract (ConsumeMany _ cs (Left _)) = cextract cs
    extract (ConsumeMany _ cs (Right ss )) = do
        cres <- sextract ss >>= cstep cs
        case cres of
            Partial s -> cextract s
            Done b -> return b

-- | Extract the output of a fold and refold it using a 'Refold'.
--
-- A fusible alternative to 'concatMap'.
--
-- /Internal/
{-# INLINE refold #-}
refold :: Monad m => Refold m b a c -> Fold m a b -> Fold m a c
refold (Refold step inject extract) f = Fold step (finish f >>= inject) extract
