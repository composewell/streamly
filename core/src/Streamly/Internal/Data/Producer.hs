-- |
-- Module      : Streamly.Internal.Data.Producer
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators on stream transition functions of type
-- @s -> m (Step s a)@. These are shared by the @Stream@ and @Unfold@
-- step functions.

module Streamly.Internal.Data.Producer
    (
      CrossApplyState(..)
    , CrossApplyFstState(..)
    , CrossState(..)
    , FairCrossState(..)
    , TupleState(..)
    , ConcatState(..)
    , InterleaveState(..)
    , InterleaveEachState(..)
    , ZipState(..)
    , ConcatMapState(..)
    , InnerProducer(..)
    , concatMapM
    , unfoldEach
    , unfoldEachInterleave
    , interleave
    , zipWithM
    , crossApply
    , crossApplyFst
    , crossApplySnd
    , crossWithM
    , fairCrossWithM
    , fromEffect
    , fromList
    , fromTuple
    , mapM
    , mapMaybeM
    , takeWhileM
    , unfoldrM
    )
where

#include "inline.hs"

import Data.Functor ((<&>))
import Streamly.Internal.Data.Stream.Step (Step(..))

import Prelude hiding (mapM)

-- | A stream transition: given the current state, produce the next 'Step'.
-- The state type @a@ is also the type carried inside 'Step', so a 'Yield'
-- delivers a new value alongside the updated state.
type Producer m a b = a -> m (Step a b)

-- | State of a cross-apply style producer. @x@ is the seed from which the
-- inner producer's state is (re)injected for every element of the outer
-- producer. We store the first-order @x@ seed rather than the injection
-- action @m s2@ so that the inner injection stays a known, statically inlined
-- call and the loop state remains unboxable (storing @m s2@ here defeats
-- fusion and forces per-element allocation).
data CrossApplyState x s1 s2 a b =
      CrossApplyOuter x s1
    | CrossApplyInner x (a -> b) s1 s2

-- | State for 'crossApplyFst'. The inner constructor stores the outer
-- producer's value @b@ /directly/ so that it can be re-yielded for each element
-- of the inner producer as a loop-invariant value. Storing a function
-- (@const b@) here instead would force a per-element PAP application in the hot
-- inner loop and defeat the hoisting the original yielded value gets.
data CrossApplyFstState x s1 s2 b =
      CrossApplyFstOuter x s1
    | CrossApplyFstInner x b s1 s2

data TupleState a = TupleBoth a a | TupleOne a | TupleNone

-- | Build a single element 'Producer' from an effect. The 'Bool' state is
-- 'True' before the effect is run and 'False' after, when the producer stops.
{-# INLINE_LATE fromEffect #-}
fromEffect :: Applicative m => m b -> Producer m Bool b
fromEffect m True  = (`Yield` False) <$> m
fromEffect _ False = pure Stop

{-# INLINE_LATE fromTuple #-}
fromTuple :: Applicative m => Producer m (TupleState a) a
fromTuple (TupleBoth x y) = pure $ Yield x (TupleOne y)
fromTuple (TupleOne y) = pure $ Yield y TupleNone
fromTuple TupleNone = pure Stop

{-# INLINE_LATE fromList #-}
fromList :: Applicative m => Producer m [a] a
fromList (x:xs) = pure $ Yield x xs
fromList [] = pure Stop

{-# INLINE_LATE crossApply #-}
crossApply
    :: Monad m
    => (x -> m s2)
    -> Producer m s1 (a -> b)
    -> Producer m s2 a
    -> Producer m (CrossApplyState x s1 s2 a b) b
crossApply inject2 step1 _ (CrossApplyOuter seed st) = do
    r <- step1 st
    case r of
        Yield f s -> do
            s2 <- inject2 seed
            return $ Skip (CrossApplyInner seed f s s2)
        Skip s -> return $ Skip (CrossApplyOuter seed s)
        Stop -> return Stop
crossApply _ _ step2 (CrossApplyInner seed f os st) = do
    r <- step2 st
    return $ case r of
        Yield a s -> Yield (f a) (CrossApplyInner seed f os s)
        Skip s -> Skip (CrossApplyInner seed f os s)
        Stop -> Skip (CrossApplyOuter seed os)

-- | Outer product discarding the second (inner) element. For each element of
-- the first producer the entire second producer is run, yielding the first
-- producer's element each time.
{-# INLINE_LATE crossApplyFst #-}
crossApplyFst
    :: Monad m
    => (x -> m s2)
    -> Producer m s1 b
    -> Producer m s2 a
    -> Producer m (CrossApplyFstState x s1 s2 b) b
crossApplyFst inject2 step1 _ (CrossApplyFstOuter seed st) = do
    r <- step1 st
    case r of
        Yield b s -> do
            s2 <- inject2 seed
            return $ Skip (CrossApplyFstInner seed b s s2)
        Skip s -> return $ Skip (CrossApplyFstOuter seed s)
        Stop -> return Stop
crossApplyFst _ _ step2 (CrossApplyFstInner seed b os st) = do
    r <- step2 st
    return $ case r of
        Yield _ s -> Yield b (CrossApplyFstInner seed b os s)
        Skip s -> Skip (CrossApplyFstInner seed b os s)
        Stop -> Skip (CrossApplyFstOuter seed os)

-- | Outer product discarding the first (outer) element. For each element of
-- the first producer the entire second producer is run, yielding the second
-- producer's elements.
{-# INLINE_LATE crossApplySnd #-}
crossApplySnd
    :: Monad m
    => (x -> m s2)
    -> Producer m s1 a
    -> Producer m s2 b
    -> Producer m (CrossApplyState x s1 s2 b b) b
crossApplySnd inject2 step1 _ (CrossApplyOuter seed st) = do
    r <- step1 st
    case r of
        Yield _ s -> do
            s2 <- inject2 seed
            return $ Skip (CrossApplyInner seed id s s2)
        Skip s -> return $ Skip (CrossApplyOuter seed s)
        Stop -> return Stop
crossApplySnd _ _ step2 (CrossApplyInner seed f os st) = do
    r <- step2 st
    return $ case r of
        Yield a s -> Yield (f a) (CrossApplyInner seed f os s)
        Skip s -> Skip (CrossApplyInner seed f os s)
        Stop -> Skip (CrossApplyOuter seed os)

data CrossState x s1 s2 b =
      CrossOuter x s1
    | CrossInner x b s1 s2

-- | Cross product (vector or cartesian product) of two producers using a
-- monadic combining function. For each element of the first (outer) producer
-- the entire second (inner) producer is run, combining the outer element with
-- each inner element.
{-# INLINE_LATE crossWithM #-}
crossWithM
    :: Monad m
    => (b -> c -> m d)
    -> (x -> m s2)
    -> Producer m s1 b
    -> Producer m s2 c
    -> Producer m (CrossState x s1 s2 b) d
crossWithM _ inject2 step1 _ (CrossOuter seed st) = do
    r <- step1 st
    case r of
        Yield b s -> do
            s2 <- inject2 seed
            return $ Skip (CrossInner seed b s s2)
        Skip s -> return $ Skip (CrossOuter seed s)
        Stop -> return Stop
crossWithM f _ _ step2 (CrossInner seed b os st) = do
    r <- step2 st
    case r of
        Yield c s -> f b c >>= \d -> return $ Yield d (CrossInner seed b os s)
        Skip s -> return $ Skip (CrossInner seed b os s)
        Stop -> return $ Skip (CrossOuter seed os)

data FairCrossState x s1 i =
      FairCrossInit x s1 ([i] -> [i])
    | FairCrossNext x s1 ([i] -> [i]) [i]
    | FairCrossDrain ([i] -> [i]) [i]

-- | Like 'crossWithM' but interleaves the inner producers fairly: it advances
-- every live inner producer by one step in a round before injecting the next
-- outer element, instead of running each inner producer to completion.
{-# INLINE_LATE fairCrossWithM #-}
fairCrossWithM
    :: Monad m
    => (b -> c -> m d)
    -> (x -> m s2)
    -> Producer m s1 b
    -> Producer m s2 c
    -> Producer m (FairCrossState x s1 (b, s2)) d
fairCrossWithM _ inject2 step1 _ (FairCrossInit seed o ls) = do
    r <- step1 o
    case r of
        Yield b o1 -> do
            i <- inject2 seed
            i `seq` return (Skip (FairCrossNext seed o1 id (ls [(b, i)])))
        Skip o1 -> return $ Skip (FairCrossInit seed o1 ls)
        Stop -> return $ Skip (FairCrossDrain id (ls []))
fairCrossWithM _ _ _ _ (FairCrossNext seed o ys []) =
    return $ Skip (FairCrossInit seed o ys)
fairCrossWithM f _ _ step2 (FairCrossNext seed o ys ((b, st):ls)) = do
    r <- step2 st
    case r of
        Yield c s ->
            f b c >>= \d ->
                return $ Yield d (FairCrossNext seed o (ys . ((b, s) :)) ls)
        Skip s -> return $ Skip (FairCrossNext seed o ys ((b, s) : ls))
        Stop -> return $ Skip (FairCrossNext seed o ys ls)
fairCrossWithM _ _ _ _ (FairCrossDrain ys []) =
    case ys [] of
        [] -> return Stop
        xs -> return $ Skip (FairCrossDrain id xs)
fairCrossWithM f _ _ step2 (FairCrossDrain ys ((b, st):ls)) = do
    r <- step2 st
    case r of
        Yield c s ->
            f b c >>= \d ->
                return $ Yield d (FairCrossDrain (ys . ((b, s) :)) ls)
        Skip s -> return $ Skip (FairCrossDrain ys ((b, s) : ls))
        Stop -> return $ Skip (FairCrossDrain ys ls)

-- | An inner producer of a 'concatMapM' bundling its step function with its
-- current state. It is the existential package that allows the dynamically
-- generated inner stream to be stored in the loop state of 'concatMapM'.
data InnerProducer m c = forall s. InnerProducer (s -> m (Step s c)) s

-- | State of a 'concatMapM' producer. @x@ is the seed carried for the whole
-- loop (used to (re)generate the inner producer for each outer element), @s1@
-- is the outer producer's state.
data ConcatMapState m c x s1 =
      ConcatMapOuter x s1
    | ConcatMapInner x s1 (InnerProducer m c)

-- | Map an inner-producer generating action to each element of the outer
-- producer and flatten the results into a single stream. The supplied function
-- is given the loop seed @x@ along with the outer element so that the inner
-- producer can be (re)generated from the seed.
{-# INLINE_LATE concatMapM #-}
concatMapM
    :: Monad m
    => (x -> b -> m (InnerProducer m c))
    -> Producer m s1 b
    -> Producer m (ConcatMapState m c x s1) c
concatMapM f step1 (ConcatMapOuter seed st) = do
    r <- step1 st
    case r of
        Yield b s -> do
            inner <- f seed b
            return $ Skip (ConcatMapInner seed s inner)
        Skip s -> return $ Skip (ConcatMapOuter seed s)
        Stop -> return Stop
concatMapM _ _ (ConcatMapInner seed ost (InnerProducer istep ist)) = do
    r <- istep ist
    return $ case r of
        Yield x s -> Yield x (ConcatMapInner seed ost (InnerProducer istep s))
        Skip s -> Skip (ConcatMapInner seed ost (InnerProducer istep s))
        Stop -> Skip (ConcatMapOuter seed ost)

-- | State of an 'unfoldEach' producer. @s1@ is the outer producer's state and
-- @s2@ the inner producer's state which is re-injected from each outer element.
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- | Run the inner producer for every element of the outer producer and flatten
-- the results into a single stream. Unlike 'concatMapM' the inner producer is
-- the same statically known producer for every outer element; only its state is
-- (re)injected from the outer element via the supplied action.
{-# INLINE_LATE unfoldEach #-}
unfoldEach
    :: Monad m
    => (b -> m s2)
    -> Producer m s1 b
    -> Producer m s2 c
    -> Producer m (ConcatState s1 s2) c
unfoldEach inject2 step1 _ (ConcatOuter st) = do
    r <- step1 st
    case r of
        Yield b s -> do
            i <- inject2 b
            i `seq` return (Skip (ConcatInner s i))
        Skip s    -> return $ Skip (ConcatOuter s)
        Stop      -> return Stop
unfoldEach _ _ step2 (ConcatInner ost ist) = do
    r <- step2 ist
    return $ case r of
        Yield x s -> Yield x (ConcatInner ost s)
        Skip s    -> Skip (ConcatInner ost s)
        Stop      -> Skip (ConcatOuter ost)

-- | State of an 'unfoldEachInterleave' producer. @o@ is the outer producer's
-- state and @i@ an inner producer's state. The @[i]@ lists hold the live inner
-- producers; we step one inner producer per round and shuttle the survivors
-- between the two lists, reversing the traversal direction at each end (a
-- boustrophedon walk) so that no per-round reversal or difference-list closure
-- is needed.
data InterleaveEachState o i =
      InterleaveOuter o [i]
    | InterleaveInner o [i]
    | InterleaveInnerL [i] [i]
    | InterleaveInnerR [i] [i]

-- | Run the inner producer for every element of the outer producer, like
-- 'unfoldEach', but interleave the inner producers breadth-first instead of
-- concatenating them: advance each live inner producer by one step in a round
-- before moving on to the next. After the outer producer is exhausted the
-- direction of traversal is reversed at each end, alternating the order on
-- successive rounds. The same statically known inner producer is used for every
-- outer element; only its state is (re)injected from the outer element via the
-- supplied action.
{-# INLINE_LATE unfoldEachInterleave #-}
unfoldEachInterleave
    :: Monad m
    => (b -> m s2)
    -> Producer m s1 b
    -> Producer m s2 c
    -> Producer m (InterleaveEachState s1 s2) c
unfoldEachInterleave inject2 step1 _ (InterleaveOuter o ls) = do
    r <- step1 o
    case r of
        Yield a o1 -> do
            i <- inject2 a
            i `seq` return (Skip (InterleaveInner o1 (i : ls)))
        Skip o1 -> return $ Skip (InterleaveOuter o1 ls)
        Stop -> return $ Skip (InterleaveInnerL ls [])

unfoldEachInterleave _ _ _ (InterleaveInner _ []) = undefined
unfoldEachInterleave _ _ step2 (InterleaveInner o (st:ls)) = do
    r <- step2 st
    return $ case r of
        Yield x s -> Yield x (InterleaveOuter o (s:ls))
        Skip s    -> Skip (InterleaveInner o (s:ls))
        Stop      -> Skip (InterleaveOuter o ls)

unfoldEachInterleave _ _ _ (InterleaveInnerL [] []) = return Stop
unfoldEachInterleave _ _ _ (InterleaveInnerL [] rs) =
    return $ Skip (InterleaveInnerR [] rs)
unfoldEachInterleave _ _ step2 (InterleaveInnerL (st:ls) rs) = do
    r <- step2 st
    return $ case r of
        Yield x s -> Yield x (InterleaveInnerL ls (s:rs))
        Skip s    -> Skip (InterleaveInnerL (s:ls) rs)
        Stop      -> Skip (InterleaveInnerL ls rs)

unfoldEachInterleave _ _ _ (InterleaveInnerR [] []) = return Stop
unfoldEachInterleave _ _ _ (InterleaveInnerR ls []) =
    return $ Skip (InterleaveInnerL ls [])
unfoldEachInterleave _ _ step2 (InterleaveInnerR ls (st:rs)) = do
    r <- step2 st
    return $ case r of
        Yield x s -> Yield x (InterleaveInnerR (s:ls) rs)
        Skip s    -> Skip (InterleaveInnerR ls (s:rs))
        Stop      -> Skip (InterleaveInnerR ls rs)

-- | State of an 'interleave' producer. @s1@ and @s2@ are the states of the two
-- producers being interleaved. We alternate stepping the first and the second
-- producer; once one of them stops we switch to a state that drains only the
-- other.
data InterleaveState s1 s2 =
      InterleaveFirst s1 s2
    | InterleaveSecond s1 s2
    | InterleaveSecondOnly s2
    | InterleaveFirstOnly s1

-- | Interleave the elements produced by two producers, yielding one element
-- from each alternately, starting from the first. When one producer stops, the
-- remaining elements of the other are emitted. Both producers are completely
-- exhausted.
{-# INLINE_LATE interleave #-}
interleave
    :: Monad m
    => Producer m s1 c
    -> Producer m s2 c
    -> Producer m (InterleaveState s1 s2) c
interleave step1 _ (InterleaveFirst st1 st2) = do
    r <- step1 st1
    return $ case r of
        Yield a s -> Yield a (InterleaveSecond s st2)
        Skip s    -> Skip (InterleaveFirst s st2)
        Stop      -> Skip (InterleaveSecondOnly st2)
interleave _ step2 (InterleaveSecond st1 st2) = do
    r <- step2 st2
    return $ case r of
        Yield a s -> Yield a (InterleaveFirst st1 s)
        Skip s    -> Skip (InterleaveSecond st1 s)
        Stop      -> Skip (InterleaveFirstOnly st1)
interleave step1 _ (InterleaveFirstOnly st1) = do
    r <- step1 st1
    return $ case r of
        Yield a s -> Yield a (InterleaveFirstOnly s)
        Skip s    -> Skip (InterleaveFirstOnly s)
        Stop      -> Stop
interleave _ step2 (InterleaveSecondOnly st2) = do
    r <- step2 st2
    return $ case r of
        Yield a s -> Yield a (InterleaveSecondOnly s)
        Skip s    -> Skip (InterleaveSecondOnly s)
        Stop      -> Stop

-- | State of a 'zipWithM' producer. In 'ZipFirst' we pull the next element from
-- the first producer; in 'ZipSecond' we hold that buffered element @b@ and pull
-- from the second producer to zip with it.
data ZipState s1 s2 b = ZipFirst s1 s2 | ZipSecond s1 s2 b

-- | Zip the elements produced by two producers using a monadic zip function.
-- The first producer is advanced to get an element, then the second is advanced
-- and the two elements are combined. Stops as soon as either producer stops.
{-# INLINE_LATE zipWithM #-}
zipWithM
    :: Monad m
    => (b -> c -> m d)
    -> Producer m s1 b
    -> Producer m s2 c
    -> Producer m (ZipState s1 s2 b) d
zipWithM _ step1 _ (ZipFirst s1 s2) = do
    r <- step1 s1
    return $ case r of
        Yield x s -> Skip (ZipSecond s s2 x)
        Skip s    -> Skip (ZipFirst s s2)
        Stop      -> Stop
zipWithM f _ step2 (ZipSecond s1 s2 x) = do
    r <- step2 s2
    case r of
        Yield y s -> do
            z <- f x y
            return $ Yield z (ZipFirst s1 s)
        Skip s -> return $ Skip (ZipSecond s1 s x)
        Stop   -> return Stop

{-# INLINE_LATE mapM #-}
mapM :: Monad m => (b -> m c) -> Producer m s b -> Producer m s c
mapM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ Yield b s
        Skip s -> return (Skip s)
        Stop   -> return Stop

{-# INLINE_LATE mapMaybeM #-}
mapMaybeM :: Monad m
    => (b -> m (Maybe c)) -> Producer m s b -> Producer m s c
mapMaybeM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ case b of
                Just c  -> Yield c s
                Nothing -> Skip s
        Skip s -> return (Skip s)
        Stop   -> return Stop

{-# INLINE_LATE takeWhileM #-}
takeWhileM :: Monad m
    => (b -> m Bool) -> Producer m s b -> Producer m s b
takeWhileM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ if b then Yield x s else Stop
        Skip s -> return (Skip s)
        Stop   -> return Stop

-- | Build a 'Producer' from a /monadic/ step function that generates the next
-- element and the next seed value from the current seed value. It is invoked
-- until it returns 'Nothing'.
{-# INLINE_LATE unfoldrM #-}
unfoldrM :: Applicative m => (a -> m (Maybe (b, a))) -> Producer m a b
unfoldrM next a =
    next a <&> \case
        Just (b, a1) -> Yield b a1
        Nothing -> Stop
