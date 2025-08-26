{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Unfold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- An unfold is akin to a reader. It is the streaming equivalent of a reader.
-- The argument @a@ is the environment of the reader. That's the reason the
-- default unfolds in various modules are named "reader".

-- = Performance Notes
--
-- 'Unfold' representation is more efficient than using streams when combining
-- streams.  'Unfold' type allows multiple unfold actions to be composed into a
-- single unfold function in an efficient manner by enabling the compiler to
-- perform stream fusion optimization.
-- @Unfold m a b@ can be considered roughly equivalent to an action @a -> t m
-- b@ (where @t@ is a stream type). Instead of using an 'Unfold' one could just
-- use a function of the shape @a -> t m b@. However, working with stream types
-- like t'Streamly.SerialT' does not allow the compiler to perform stream fusion
-- optimization when merging, appending or concatenating multiple streams.
-- Even though stream based combinator have excellent performance, they are
-- much less efficient when compared to combinators using 'Unfold'.  For
-- example, the 'Streamly.Data.Stream.concatMap' combinator which uses @a -> t m b@
-- (where @t@ is a stream type) to generate streams is much less efficient
-- compared to 'Streamly.Data.Stream.unfoldEach'.
--
-- On the other hand, transformation operations on stream types are as
-- efficient as transformations on 'Unfold'.
--
-- We should note that in some cases working with stream types may be more
-- convenient compared to working with the 'Unfold' type.  However, if extra
-- performance boost is important then 'Unfold' based composition should be
-- preferred compared to stream based composition when merging or concatenating
-- streams.

module Streamly.Internal.Data.Unfold.Type
    (
    -- * General Notes
    -- $notes

    -- * Type
    -- StreamD Step type re-exported
      Step(..)
    , Unfold (..)

    -- * Basic Constructors
    , mkUnfoldM
    , mkUnfoldrM
    , unfoldrM
    , unfoldr
    , functionM
    , function
    , identity

    -- * From Values
    , fromEffect
    , fromPure

    -- * From Containers
    , fromList
    , fromTuple

    -- * Transformations
    , lmap
    , lmapM
    , map
    , mapM
    , both
    , supply
    , first
    , second
    , carry

    -- * Trimming
    , takeWhileM
    , takeWhile

    -- * Nesting
    , interleave
    , ConcatState (..)
    , unfoldEach
    , unfoldEachInterleave

    -- Applicative
    , crossApplySnd
    , crossApplyFst
    , crossWithM
    , fairCrossWithM
    , fairCrossWith
    , fairCross
    , crossWith
    , cross
    , crossApply

    -- Monad
    , concatMapM
    , concatMap
    , bind

    , zipArrowWithM
    , zipArrowWith
    , zipWithM
    , zipWith

    -- * Deprecated
    , many
    , many2
    , manyInterleave
    , map2
    , mapM2
    , takeWhileMWithInput
    )
where

#include "deprecation.h"
#include "inline.hs"

-- import Control.Arrow (Arrow(..))
-- import Control.Category (Category(..))
import Control.Monad ((>=>))
import Data.Void (Void)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Stream.Step (Step(..))

import Prelude hiding (map, mapM, concatMap, zipWith, takeWhile)

#include "DocTestDataUnfold.hs"

-- $notes
--
-- What makes streams less efficient is also what makes them more convenient to
-- use and powerful. The stream data type (Stream m a) bundles the state along
-- with the stream generator function making it opaque, whereas an unfold
-- exposes the state (Unfold m s a) to the user. This allows the Unfold to be
-- unfolded (inlined) inside a nested loop without having to bundle the state
-- and the generator together, the stream state can be saved and passed
-- independent of the generator function. On the other hand in a stream type we
-- have to bundle the stream state and the generator function together to save
-- the stream. This makes it inefficient because it requires boxing and
-- constructor allocation. However, this makes streams more convenient as we do
-- not need to pass around the state/seed separately.
--
-- Unfold Type:
--
-- The order of arguments allows 'Category' and 'Arrow' instances but precludes
-- contravariant and contra-applicative.
--
-- = Unfolds and Streams
--
-- An 'Unfold' type is the same as the direct style 'Stream' type except that
-- it uses an inject function to determine the initial state of the stream
-- based on an input.  A stream is a special case of Unfold when the static
-- input is unit or Void.
--
-- This allows an important optimization to occur in several cases, making the
-- 'Unfold' a more efficient abstraction. Consider the 'concatMap' and
-- 'unfoldEach' operations, the latter is more efficient.  'concatMap'
-- generates a new stream object from each element in the stream by applying
-- the supplied function to the element, the stream object includes the "step"
-- function as well as the initial "state" of the stream.  Since the stream is
-- generated dynamically the compiler does not know the step function or the
-- state type statically at compile time, therefore, it cannot inline it. On
-- the other hand in case of 'unfoldEach' the compiler has visibility into
-- the unfold's state generation function, therefore, the compiler knows all
-- the types statically and it can inline the inject as well as the step
-- functions, generating efficient code. Essentially, the stream is not opaque
-- to the consumer in case of unfolds, the consumer knows how to generate the
-- stream from a seed using a known "inject" and "step" functions.
--
-- A Stream is like a data object whereas unfold is like a function.  Being
-- function like, an Unfold is an instance of 'Category' and 'Arrow' type
-- classes.
--
-- = Unfolds and Folds
--
-- Streams forcing a closed control flow loop can be categorized under
-- two types, unfolds and folds, both of these are duals of each other.
--
-- Unfold streams are really generators of a sequence of elements, we can also
-- call them pull style streams. These are lazy producers of streams. On each
-- evaluation the producer generates the next element.  A consumer can
-- therefore pull elements from the stream whenever it wants to.  A stream
-- consumer can multiplex pull streams by pulling elements from the chosen
-- streams, therefore, pull streams allow merging or multiplexing.  On the
-- other hand, with this representation we cannot split or demultiplex a
-- stream.  So really these are stream sources that can be generated from a
-- seed and can be merged or zipped into a single stream.
--
-- The dual of Unfolds are Folds. Folds can also be called as push style
-- streams or reducers. These are strict consumers of streams. We keep pushing
-- elements to a fold and we can extract the result at any point. A driver can
-- choose which fold to push to and can also push the same element to multiple
-- folds. Therefore, folds allow splitting or demultiplexing a stream. On the
-- other hand, we cannot merge streams using this representation. So really
-- these are stream consumers that reduce the stream to a single value, these
-- consumers can be composed such that a stream can be split over multiple
-- consumers.
--
-- Performance:
--
-- Composing a tree or graph of computations with unfolds can be much more
-- efficient compared to composing with the Monad instance.  The reason is that
-- unfolds allow the compiler to statically know the state and optimize it
-- using stream fusion whereas it is not possible with the monad bind because
-- the state is determined dynamically.
--
-- Reader:
--
-- An unfold acts as a reader (see 'Reader' monad). The input to an unfold acts
-- as the read-only environment. The environment can be extracted using the
-- 'identity' unfold (equivalent to 'ask') and transformed using 'lmap'.

------------------------------------------------------------------------------
-- Monadic Unfolds
------------------------------------------------------------------------------

-- | An @Unfold m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
data Unfold m a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

------------------------------------------------------------------------------
-- Basic constructors
------------------------------------------------------------------------------

-- XXX unfoldWith?

-- | Make an unfold from @step@ and @inject@ functions.
--
-- /Pre-release/
{-# INLINE mkUnfoldM #-}
mkUnfoldM :: (s -> m (Step s b)) -> (a -> m s) -> Unfold m a b
mkUnfoldM = Unfold

-- | Make an unfold from a step function.
--
-- See also: 'unfoldrM'
--
-- /Pre-release/
{-# INLINE mkUnfoldrM #-}
mkUnfoldrM :: Applicative m => (a -> m (Step a b)) -> Unfold m a b
mkUnfoldrM step = Unfold step pure

-- The type 'Step' is isomorphic to 'Maybe'. Ideally unfoldrM should be the
-- same as mkUnfoldrM, this is for compatibility with traditional Maybe based
-- unfold step functions.

-- | Build a stream by unfolding a /monadic/ step function starting from a seed.
-- The step function returns the next element in the stream and the next seed
-- value. When it is done it returns 'Nothing' and the stream ends.
--
{-# INLINE unfoldrM #-}
unfoldrM :: Applicative m => (a -> m (Maybe (b, a))) -> Unfold m a b
unfoldrM next = Unfold step pure
  where
    {-# INLINE_LATE step #-}
    step st =
        (\case
            Just (x, s) -> Yield x s
            Nothing     -> Stop) <$> next st

-- | Like 'unfoldrM' but uses a pure step function.
--
-- >>> :{
--  f [] = Nothing
--  f (x:xs) = Just (x, xs)
-- :}
--
-- >>> Unfold.fold Fold.toList (Unfold.unfoldr f) [1,2,3]
-- [1,2,3]
--
{-# INLINE unfoldr #-}
unfoldr :: Applicative m => (a -> Maybe (b, a)) -> Unfold m a b
unfoldr step = unfoldrM (pure . step)

------------------------------------------------------------------------------
-- Map input
------------------------------------------------------------------------------

-- | Map a function on the input argument of the 'Unfold'.
--
-- >>> u = Unfold.lmap (fmap (+1)) Unfold.fromList
-- >>> Unfold.fold Fold.toList u [1..5]
-- [2,3,4,5,6]
--
-- Definition:
--
-- >>> lmap f = Unfold.unfoldEach (Unfold.function f)
--
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (uinject Prelude.. f)

-- | Map an action on the input argument of the 'Unfold'.
--
-- Definition:
--
-- lmapM f = Unfold.unfoldEach (Unfold.functionM f)
--
{-# INLINE_NORMAL lmapM #-}
lmapM :: Monad m => (a -> m c) -> Unfold m c b -> Unfold m a b
lmapM f (Unfold ustep uinject) = Unfold ustep (f >=> uinject)

-- | Supply the seed to an unfold closing the input end of the unfold.
--
-- >>> supply a = Unfold.lmap (Prelude.const a)
--
-- /Pre-release/
--
{-# INLINE_NORMAL supply #-}
supply :: a -> Unfold m a b -> Unfold m () b
supply a = lmap (Prelude.const a)

{-# DEPRECATED both "Use supply instead." #-}
{-# INLINE_NORMAL both #-}
both :: a -> Unfold m a b -> Unfold m Void b
both a = lmap (Prelude.const a)

-- | Supply the first component of the tuple to an unfold that accepts a tuple
-- as a seed resulting in a fold that accepts the second component of the tuple
-- as a seed.
--
-- @
-- first a = Unfold.lmap (a, )
-- @
--
-- /Pre-release/
--
{-# INLINE_NORMAL first #-}
first :: a -> Unfold m (a, b) c -> Unfold m b c
first a = lmap (a, )

-- | Supply the second component of the tuple to an unfold that accepts a tuple
-- as a seed resulting in a fold that accepts the first component of the tuple
-- as a seed.
--
-- @
-- second b = Unfold.lmap (, b)
-- @
--
-- /Pre-release/
--
{-# INLINE_NORMAL second #-}
second :: b -> Unfold m (a, b) c -> Unfold m a c
second b = lmap (, b)

------------------------------------------------------------------------------
-- Filter input
------------------------------------------------------------------------------

-- |
-- >>> takeWhileMWithInput f u = Unfold.map snd $ Unfold.takeWhileM (\(a,b) -> f a b) (Unfold.carry u)
{-# INLINE_NORMAL takeWhileMWithInput #-}
takeWhileMWithInput :: Monad m =>
    (a -> b -> m Bool) -> Unfold m a b -> Unfold m a b
takeWhileMWithInput f u = map snd $ takeWhileM (\(a,b) -> f a b) (carry u)
{-
takeWhileMWithInput f (Unfold step1 inject1) = Unfold step inject

    where

    inject a = do
        s <- inject1 a
        return $ Tuple' a s

    {-# INLINE_LATE step #-}
    step (Tuple' a st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f a x
                return $ if b then Yield x (Tuple' a s) else Stop
            Skip s -> return $ Skip (Tuple' a s)
            Stop   -> return Stop
-}

-- | Same as 'takeWhile' but with a monadic predicate.
--
{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
-- XXX Check if the compiler simplifies the following to the same as the custom
-- implementation below (the Tuple' should help eliminate the unused param):
--
-- takeWhileM f = takeWhileMWithInput (\_ b -> f b)
takeWhileM f (Unfold step1 inject1) = Unfold step inject1
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

-- | End the stream generated by the 'Unfold' as soon as the predicate fails
-- on an element.
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
takeWhile f = takeWhileM (return . f)

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

{-# DEPRECATED mapM2 "Use carry with mapM instead." #-}
{-# INLINE_NORMAL mapM2 #-}
mapM2 :: Monad m => (a -> b -> m c) -> Unfold m a b -> Unfold m a c
mapM2 f = mapM (uncurry f) . carry
{-
mapM2 f (Unfold ustep uinject) = Unfold step inject
    where
    inject a = do
        r <- uinject a
        return (a, r)

    {-# INLINE_LATE step #-}
    step (inp, st) = do
        r <- ustep st
        case r of
            Yield x s -> f inp x >>= \a -> return $ Yield a (inp, s)
            Skip s    -> return $ Skip (inp, s)
            Stop      -> return Stop
-}

-- | Apply a monadic function to each element of the stream and replace it
-- with the output of the resulting action.
--
{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (b -> m c) -> Unfold m a b -> Unfold m a c
-- mapM f = mapM2 (const f)
mapM f (Unfold ustep uinject) = Unfold step uinject
    where
    {-# INLINE_LATE step #-}
    step st = do
        r <- ustep st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

-- | Carry the input along with the output as the first element of the output
-- tuple.
--
-- carry = Unfold.lmap (\x -> (x,x)) . Unfold.zipRepeat
--
-- Note that the input seed may mutate (e.g. if the seed is a Handle or IORef)
-- as stream is generated from it, so we need to be careful when reusing the
-- seed while the stream is being generated from it.
--
{-# INLINE_NORMAL carry #-}
carry :: Functor m => Unfold m a b -> Unfold m a (a,b)
carry (Unfold ustep uinject) = Unfold step (\a -> (a,) <$> uinject a)

    where

    func a r =
        case r of
            Yield x s -> Yield (a, x) (a, s)
            Skip s    -> Skip (a, s)
            Stop      -> Stop

    {-# INLINE_LATE step #-}
    step (a, st) = fmap (func a) (ustep st)

{-# DEPRECATED map2 "Use carry with map instead." #-}
{-# INLINE_NORMAL map2 #-}
map2 :: Functor m => (a -> b -> c) -> Unfold m a b -> Unfold m a c
map2 f = map (uncurry f) . carry

-- | Map a function on the output of the unfold (the type @b@).
--
-- /Pre-release/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Unfold m a b -> Unfold m a c
-- map f = map2 (const f)
map f (Unfold ustep uinject) = Unfold step uinject

    where

    {-# INLINE_LATE step #-}
    step st = fmap (fmap f) (ustep st)

-- | Maps a function on the output of the unfold (the type @b@).
instance Functor m => Functor (Unfold m a) where
    {-# INLINE fmap #-}
    fmap = map

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- XXX Shouldn't this be Unfold m (m a) a ?

-- | The unfold discards its input and generates a function stream using the
-- supplied monadic action.
--
-- /Pre-release/
{-# INLINE fromEffect #-}
fromEffect :: Applicative m => m b -> Unfold m a b
fromEffect m = Unfold step inject

    where

    inject _ = pure False

    step False = (`Yield` True) <$> m
    step True = pure Stop

-- XXX Shouldn't this be Unfold m a a ? Which is identity. Should this function
-- even exist for Unfolds. Should we have applicative/Monad for unfolds?

-- | Discards the unfold input and always returns the argument of 'fromPure'.
--
-- > fromPure = fromEffect . pure
--
-- /Pre-release/
{-# INLINE fromPure #-}
fromPure :: Applicative m => b -> Unfold m a b
fromPure = fromEffect Prelude.. pure

data TupleState a = TupleBoth a a | TupleOne a | TupleNone

-- | Convert a tuple to a 'Stream'.
--
{-# INLINE_LATE fromTuple #-}
fromTuple :: Applicative m => Unfold m (a,a) a
fromTuple = Unfold step (\(x,y) -> pure $ TupleBoth x y)

    where

    {-# INLINE_LATE step #-}
    step (TupleBoth x y) = pure $ Yield x (TupleOne y)
    step (TupleOne y) = pure $ Yield y TupleNone
    step TupleNone = pure Stop

-- XXX Check if "unfold (fromList [1..10])" fuses, if it doesn't we can use
-- rewrite rules to rewrite list enumerations to unfold enumerations.

-- | Convert a list of pure values to a 'Stream'
--
{-# INLINE_LATE fromList #-}
fromList :: Applicative m => Unfold m [a] a
fromList = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step (x:xs) = pure $ Yield x xs
    step [] = pure Stop

-- | Outer product discarding the first element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL crossApplySnd #-}
crossApplySnd :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a c
crossApplySnd (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

-- | Outer product discarding the second element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL crossApplyFst #-}
crossApplyFst :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a b
crossApplyFst (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

{-
{-# ANN type Many2State Fuse #-}
data Many2State x s1 s2 = Many2Outer x s1 | Many2Inner x s1 s2
-}

{-# DEPRECATED many2 "Use carry with unfoldEach instead." #-}
{-# INLINE_NORMAL many2 #-}
many2 :: Monad m =>
    Unfold m (a, b) c -> Unfold m a b -> Unfold m a c
many2 u1 u2 = unfoldEach u1 (carry u2)
{-
unfoldEach2 (Unfold step2 inject2) (Unfold step1 inject1) = Unfold step inject

    where

    inject a = do
        s <- inject1 a
        return $ Many2Outer a s

    {-# INLINE_LATE step #-}
    step (Many2Outer a st) = do
        r <- step1 st
        case r of
            Yield b s -> do
                innerSt <- inject2 (a, b)
                return $ Skip (Many2Inner a s innerSt)
            Skip s    -> return $ Skip (Many2Outer a s)
            Stop      -> return Stop

    step (Many2Inner a ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (Many2Inner a ost s)
            Skip s    -> Skip (Many2Inner a ost s)
            Stop      -> Skip (Many2Outer a ost)
-}

data Cross a s1 b s2 = CrossOuter a s1 | CrossInner a s1 b s2

-- >> f1 f u = Unfold.mapM (\((_, c), b) -> f b c) Unfold.carry (Unfold.lmap fst u))
-- >> crossWithM f u = Unfold.unfoldEach2 (f1 f u)

-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two unfolds using a monadic combining function.
--
-- /Pre-release/
{-# INLINE_NORMAL crossWithM #-}
crossWithM :: Monad m =>
    (b -> c -> m d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
-- crossWithM f u1 u2 = many2 (mapM2 (\(_, b) c -> f b c) (lmap fst u2)) u1
crossWithM f (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject a = do
        s1 <- inject1 a
        return $ CrossOuter a s1

    {-# INLINE_LATE step #-}
    step (CrossOuter a s1) = do
        r <- step1 s1
        case r of
            Yield b s -> do
                s2 <- inject2 a
                return $ Skip (CrossInner a s b s2)
            Skip s    -> return $ Skip (CrossOuter a s)
            Stop      -> return Stop

    step (CrossInner a s1 b s2) = do
        r <- step2 s2
        case r of
            Yield c s -> f b c >>= \d -> return $ Yield d (CrossInner a s1 b s)
            Skip s    -> return $ Skip (CrossInner a s1 b s)
            Stop      -> return $ Skip (CrossOuter a s1)

data FairUnfoldState a o i =
      FairUnfoldInit a o ([i] -> [i])
    | FairUnfoldNext a o ([i] -> [i]) [i]
    | FairUnfoldDrain ([i] -> [i]) [i]

{-# INLINE_NORMAL fairCrossWithM #-}
fairCrossWithM :: Monad m =>
    (b -> c -> m d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
fairCrossWithM f (Unfold step1 inject1) (Unfold step2 inject2) =
    Unfold step inject

    where

    inject a = do
        s1 <- inject1 a
        return $ FairUnfoldInit a s1 id

    {-# INLINE_LATE step #-}
    step (FairUnfoldInit a o ls) = do
        r <- step1 o
        case r of
            Yield b o' -> do
                i <- inject2 a
                i `seq` return (Skip (FairUnfoldNext a o' id (ls [(b,i)])))
            Skip o' -> return $ Skip (FairUnfoldInit a o' ls)
            Stop -> return $ Skip (FairUnfoldDrain id (ls []))

    step (FairUnfoldNext a o ys []) =
            return $ Skip (FairUnfoldInit a o ys)

    step (FairUnfoldNext a o ys ((b,st):ls)) = do
        r <- step2 st
        case r of
            Yield c s ->
                f b c >>= \x ->
                    return $ Yield x (FairUnfoldNext a o (ys . ((b, s) :)) ls)
            Skip s    -> return $ Skip (FairUnfoldNext a o ys ((b,s) : ls))
            Stop      -> return $ Skip (FairUnfoldNext a o ys ls)

    step (FairUnfoldDrain ys []) =
        case ys [] of
            [] -> return Stop
            xs -> return $ Skip (FairUnfoldDrain id xs)

    step (FairUnfoldDrain ys ((b,st):ls)) = do
        r <- step2 st
        case r of
            Yield c s ->
                f b c >>= \x ->
                    return $ Yield x (FairUnfoldDrain (ys . ((b,s) :)) ls)
            Skip s    -> return $ Skip (FairUnfoldDrain ys ((b,s) : ls))
            Stop      -> return $ Skip (FairUnfoldDrain ys ls)

-- | Like 'crossWithM' but uses a pure combining function.
--
-- > crossWith f = crossWithM (\b c -> return $ f b c)
--
-- >>> u1 = Unfold.lmap fst Unfold.fromList
-- >>> u2 = Unfold.lmap snd Unfold.fromList
-- >>> u = Unfold.crossWith (,) u1 u2
-- >>> Unfold.fold Fold.toList u ([1,2,3], [4,5,6])
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
{-# INLINE crossWith #-}
crossWith :: Monad m =>
    (b -> c -> d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
crossWith f = crossWithM (\b c -> return $ f b c)

{-# INLINE fairCrossWith #-}
fairCrossWith :: Monad m =>
    (b -> c -> d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
fairCrossWith f = fairCrossWithM (\b c -> return $ f b c)

-- | See 'crossWith'.
--
-- Definition:
--
-- >>> cross = Unfold.crossWith (,)
--
-- To create a cross product of the streams generated from a tuple we can
-- write:
--
-- >>> :{
-- cross :: Monad m => Unfold m a b -> Unfold m c d -> Unfold m (a, c) (b, d)
-- cross u1 u2 = Unfold.cross (Unfold.lmap fst u1) (Unfold.lmap snd u2)
-- :}
--
-- /Pre-release/
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
cross = crossWith (,)

{-# INLINE_NORMAL fairCross #-}
fairCross :: Monad m => Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
fairCross = fairCrossWith (,)

{-# INLINE crossApply #-}
crossApply :: Monad m => Unfold m a (b -> c) -> Unfold m a b -> Unfold m a c
crossApply u1 u2 = fmap (\(a, b) -> a b) (cross u1 u2)

-- XXX Applicative makes sense for unfolds, but monad does not. Use streams for
-- monad.

{-
-- | Example:
--
-- >>> rlist = Unfold.lmap fst Unfold.fromList
-- >>> llist = Unfold.lmap snd Unfold.fromList
-- >>> Stream.fold Fold.toList $ Stream.unfold ((,) <$> rlist <*> llist) ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
--
instance Monad m => Applicative (Unfold m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = apply

    -- {-# INLINE (*>) #-}
    -- (*>) = apSequence

    -- {-# INLINE (<*) #-}
    -- (<*) = apDiscardSnd
-}

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

data ConcatMapState m b s1 x =
      ConcatMapOuter x s1
    | forall s2. ConcatMapInner x s1 s2 (s2 -> m (Step s2 b))

-- XXX This is experimental. We should rather use streams if concatMap like
-- functionality is needed. This is no more efficient than streams.

-- | Map an unfold generating action to each element of an unfold and
-- flatten the results into a single stream.
--
{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m
    => (b -> m (Unfold m a c)) -> Unfold m a b -> Unfold m a c
concatMapM f (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return $ ConcatMapOuter x s

    {-# INLINE_LATE step #-}
    step (ConcatMapOuter seed st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                Unfold step2 inject2 <- f x
                innerSt <- inject2 seed
                return $ Skip (ConcatMapInner seed s innerSt step2)
            Skip s    -> return $ Skip (ConcatMapOuter seed s)
            Stop      -> return Stop

    step (ConcatMapInner seed ost ist istep) = do
        r <- istep ist
        return $ case r of
            Yield x s -> Yield x (ConcatMapInner seed ost s istep)
            Skip s    -> Skip (ConcatMapInner seed ost s istep)
            Stop      -> Skip (ConcatMapOuter seed ost)

{-# INLINE concatMap #-}
concatMap :: Monad m => (b -> Unfold m a c) -> Unfold m a b -> Unfold m a c
concatMap f = concatMapM (return Prelude.. f)

infixl 1 `bind`

{-# INLINE bind #-}
bind :: Monad m => Unfold m a b -> (b -> Unfold m a c) -> Unfold m a c
bind = flip concatMap

{-
-- Note: concatMap and Monad instance for unfolds have performance comparable
-- to Stream. In fact, concatMap is slower than Stream, that may be some
-- optimization issue though.
--
-- Monad allows an unfold to depend on the output of a previous unfold.
-- However, it is probably easier to use streams in such situations.
--
-- | Example:
--
-- >>> :{
--  u = do
--   x <- Unfold.enumerateFromToIntegral 4
--   y <- Unfold.enumerateFromToIntegral x
--   return (x, y)
-- :}
-- >>> Stream.fold Fold.toList $ Stream.unfold u 1
-- [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)]
--
instance Monad m => Monad (Unfold m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    -- {-# INLINE (>>) #-}
    -- (>>) = (*>)
-}

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

-- | Lift a monadic function into an unfold. The unfold generates a singleton
-- stream.
--
{-# INLINE functionM #-}
functionM :: Applicative m => (a -> m b) -> Unfold m a b
functionM f = Unfold step inject

    where

    inject x = pure $ Just x

    {-# INLINE_LATE step #-}
    step (Just x) = (`Yield` Nothing) <$> f x
    step Nothing = pure Stop

-- | Lift a pure function into an unfold. The unfold generates a singleton
-- stream.
--
-- > function f = functionM $ return . f
--
{-# INLINE function #-}
function :: Applicative m => (a -> b) -> Unfold m a b
function f = functionM $ pure Prelude.. f

-- | Identity unfold. The unfold generates a singleton stream having the input
-- as the only element.
--
-- > identity = function Prelude.id
--
-- /Pre-release/
{-# INLINE identity #-}
identity :: Applicative m => Unfold m a a
identity = function Prelude.id

{-# ANN type ConcatState Fuse #-}
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- | Apply the first unfold to each output element of the second unfold and
-- flatten the output in a single stream.
--
{-# INLINE_NORMAL unfoldEach #-}
unfoldEach, many :: Monad m => Unfold m b c -> Unfold m a b -> Unfold m a c
-- many u1 = many2 (lmap snd u1)
unfoldEach (Unfold step2 inject2) (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return $ ConcatOuter s

    {-# INLINE_LATE step #-}
    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (ConcatInner s innerSt)
            Skip s    -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (ConcatInner ost s)
            Skip s    -> Skip (ConcatInner ost s)
            Stop      -> Skip (ConcatOuter ost)

RENAME(many,unfoldEach)

{-
-- XXX There are multiple possible ways to combine the unfolds, "many" appends
-- them, we could also have other variants of "many" e.g. manyInterleave.
-- Should we even have a category instance or just use these functions
-- directly?
--
instance Monad m => Category (Unfold m) where
    {-# INLINE id #-}
    id = identity

    {-# INLINE (.) #-}
    (.) = many
-}

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

-- XXX call the original zipWith as distribute and this one as zip? or this
-- could be called divide.
--
{-# INLINE_NORMAL zipArrowWithM #-}
zipArrowWithM :: Monad m
    => (b -> c -> m d) -> Unfold m a1 b -> Unfold m a2 c -> Unfold m (a1,a2) d
zipArrowWithM f (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject (x,y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return (s1, s2, Nothing)

    {-# INLINE_LATE step #-}
    step (s1, s2, Nothing) = do
        r <- step1 s1
        return $
          case r of
            Yield x s -> Skip (s, s2, Just x)
            Skip s    -> Skip (s, s2, Nothing)
            Stop      -> Stop

    step (s1, s2, Just x) = do
        r <- step2 s2
        case r of
            Yield y s -> do
                z <- f x y
                return $ Yield z (s1, s, Nothing)
            Skip s -> return $ Skip (s1, s, Just x)
            Stop   -> return Stop

-- | Distribute the input to two unfolds and then zip the outputs to a single
-- stream using a monadic zip function.
--
-- >>> zipWithM f u1 u2 = Unfold.lmap (\x -> (x,x)) (Unfold.zipArrowWithM f u1 u2)
--
-- Stops as soon as any of the unfolds stops.
--
-- /Pre-release/
{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (b -> c -> m d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
zipWithM f u1 u2 = lmap (\x -> (x,x)) (zipArrowWithM f u1 u2)
{-
zipWithM f (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject x = do
        s1 <- inject1 x
        s2 <- inject2 x
        return (s1, s2, Nothing)

    {-# INLINE_LATE step #-}
    step (s1, s2, Nothing) = do
        r <- step1 s1
        return $
          case r of
            Yield x s -> Skip (s, s2, Just x)
            Skip s    -> Skip (s, s2, Nothing)
            Stop      -> Stop

    step (s1, s2, Just x) = do
        r <- step2 s2
        case r of
            Yield y s -> do
                z <- f x y
                return $ Yield z (s1, s, Nothing)
            Skip s -> return $ Skip (s1, s, Just x)
            Stop   -> return Stop
-}

-- | Like 'zipWithM' but with a pure zip function.
--
-- >>> square = fmap (\x -> x * x) Unfold.fromList
-- >>> cube = fmap (\x -> x * x * x) Unfold.fromList
-- >>> u = Unfold.zipWith (,) square cube
-- >>> Unfold.fold Fold.toList u [1..5]
-- [(1,1),(4,8),(9,27),(16,64),(25,125)]
--
-- > zipWith f = zipWithM (\a b -> return $ f a b)
--
{-# INLINE zipWith #-}
zipWith :: Monad m
    => (b -> c -> d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
zipWith f = zipWithM (\a b -> return (f a b))

{-# INLINE zipArrowWith #-}
zipArrowWith :: Monad m
    => (b -> c -> d) -> Unfold m a1 b -> Unfold m a2 c -> Unfold m (a1,a2) d
zipArrowWith f = zipArrowWithM (\a b -> return (f a b))

-------------------------------------------------------------------------------
-- Arrow
-------------------------------------------------------------------------------

{-
-- XXX There are multiple ways of combining the outputs of two unfolds, we
-- could zip, merge, append and more. What is the preferred way for Arrow
-- instance? Should we even have an arrow instance or just use these functions
-- directly?
--
-- | '***' is a zip like operation, in fact it is the same as @Unfold.zipWith
-- (,)@, '&&&' is a tee like operation  i.e. distributes the input to both the
-- unfolds and then zips the output.
--
{-# ANN module "HLint: ignore Use zip" #-}
instance Monad m => Arrow (Unfold m) where
    {-# INLINE arr #-}
    arr = function

    {-# INLINE (***) #-}
    u1 *** u2 = zipWith (,) (lmap fst u1) (lmap snd u2)
-}

------------------------------------------------------------------------------
-- Interleaving
------------------------------------------------------------------------------

-- We can possibly have an "interleave" operation to interleave the streams
-- from two seeds:
--
-- interleave :: Unfold m x a -> Unfold m x a -> Unfold m (x, x) a
--
-- Alternatively, we can use a signature like zipWith:
-- interleave :: Unfold m x a -> Unfold m x a -> Unfold m x a
--
-- We can implement this in terms of manyInterleave, but that may
-- not be as efficient as a custom implementation.
--
-- Similarly we can also have other binary combining ops like append, mergeBy.
-- We already have zipWith.

data InterleaveState s1 s2 =
      InterleaveFirst s1 s2
    | InterleaveSecond s1 s2
    | InterleaveSecondOnly s2
    | InterleaveFirstOnly s1

-- | Interleave the streams generated by two unfolds.
{-# INLINE_NORMAL interleave #-}
interleave :: Monad m => Unfold m a c -> Unfold m b c -> Unfold m (a,b) c
interleave (Unfold step1 inject1) (Unfold step2 inject2) =
    Unfold step inject

    where

    inject (a,b) = do
        s1 <- inject1 a
        s2 <- inject2 b
        return (InterleaveFirst s1 s2)

    {-# INLINE_LATE step #-}
    step (InterleaveFirst st1 st2) = do
        r <- step1 st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step (InterleaveSecond st1 st2) = do
        r <- step2 st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step (InterleaveFirstOnly st1) = do
        r <- step1 st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step (InterleaveSecondOnly st2) = do
        r <- step2 st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

data ManyInterleaveState o i =
      ManyInterleaveOuter o [i]
    | ManyInterleaveInner o [i]
    | ManyInterleaveInnerL [i] [i]
    | ManyInterleaveInnerR [i] [i]

-- | See 'Streamly.Internal.Data.Stream.unfoldEachInterleave' for
-- documentation and notes.
--
-- /Internal/
{-# INLINE_NORMAL unfoldEachInterleave #-}
unfoldEachInterleave, manyInterleave :: Monad m =>
    Unfold m a b -> Unfold m c a -> Unfold m c b
unfoldEachInterleave (Unfold istep iinject) (Unfold ostep oinject) =
    Unfold step inject

    where

    inject x = do
        ost <- oinject x
        return (ManyInterleaveOuter ost [])

    {-# INLINE_LATE step #-}
    step (ManyInterleaveOuter o ls) = do
        r <- ostep o
        case r of
            Yield a o' -> do
                i <- iinject a
                i `seq` return (Skip (ManyInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ManyInterleaveOuter o' ls)
            Stop -> return $ Skip (ManyInterleaveInnerL ls [])

    step (ManyInterleaveInner _ []) = undefined
    step (ManyInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ManyInterleaveOuter o (s:ls))
            Skip s    -> Skip (ManyInterleaveInner o (s:ls))
            Stop      -> Skip (ManyInterleaveOuter o ls)

    step (ManyInterleaveInnerL [] []) = return Stop
    step (ManyInterleaveInnerL [] rs) =
        return $ Skip (ManyInterleaveInnerR [] rs)

    step (ManyInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ManyInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ManyInterleaveInnerL (s:ls) rs)
            Stop      -> Skip (ManyInterleaveInnerL ls rs)

    step (ManyInterleaveInnerR [] []) = return Stop
    step (ManyInterleaveInnerR ls []) =
        return $ Skip (ManyInterleaveInnerL ls [])

    step (ManyInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ManyInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ManyInterleaveInnerR ls (s:rs))
            Stop      -> Skip (ManyInterleaveInnerR ls rs)

RENAME(manyInterleave,unfoldEachInterleave)
