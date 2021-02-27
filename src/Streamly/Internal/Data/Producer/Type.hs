-- |
-- Module      : Streamly.Internal.Data.Producer.Type
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Internal.Data.Producer" for introduction.
--

module Streamly.Internal.Data.Producer.Type
    (
    -- * Type
      Step (..)
    , Producer (..)
    , Unread (..)

    -- * Producers
    -- , nil
    -- , nilM
    , const
    -- , unfoldrM
    , fromList

    -- * State
    , identity -- get
    , modify
    , put

    -- * Mapping
    , translate
    , lmap

    -- * Nesting
    , cross
    , NestedLoop (..)
    , concatMapM
    , concatMap
    -- , concat
    , concat_
    )
where

#include "inline.hs"

import Debug.Trace
import Fusion.Plugin.Types (Fuse(..))
import Prelude hiding (concat, map, const, concatMap)
import qualified Prelude

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Producer as Producer
-- >>> import qualified Streamly.Prelude as Stream

------------------------------------------------------------------------------
-- Step
------------------------------------------------------------------------------

-- XXX Should we have an Error and Stop a instead? Error may indicate an
-- exceptional condition or using the source beyond the end.
--
-- When one producer stops how do we start another? How do we chain/append
-- producers?
--
-- serial/append :: Producer m a b -> Producer m a b -> Producer m a b
--
-- A Producer can yield many values, when it is done yielding it returns Stop.
-- When it stops it may or may not have a valid seed value, so it returns a
-- Maybe on Stop. If the seed finishes in the middle of a composition it should
-- be an error.
--
-- We should use either Yield or Stop, not both.
--
{-# ANN type Step Fuse #-}
data Step s a b =
      Stop         -- Stop with no state no result
    | Skip s       -- Continue with state but no result
    | Nil a        -- Stop with state but no result
    | Result b     -- Final result with no further state
    | Final b a    -- Final result with state
    | Partial b s  -- Partial result with state

instance Functor (Step s a) where
    {-# INLINE fmap #-}
    fmap _ Stop = Stop
    fmap _ (Skip s) = Skip s
    fmap _ (Nil a) = Nil a
    fmap f (Result b) = Result (f b)
    fmap f (Final b a) = Final (f b) a
    fmap f (Partial b s) = Partial (f b) s

------------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------------

-- We need the return type of extract to be "Maybe a" so that we can represent
-- a "used up" state for types that can not inherently represent that. For
-- example, we may not be able to implement "const" correctly without it.
--
-- Or we can use the input type to be Maybe instead. Check duality with the
-- type of "initial" in folds.
--
-- | A @Producer m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- /Internal/

data Producer m a b =
    -- | @Producer step inject extract@
    forall s. Producer (s -> m (Step s a b)) (a -> m s) (s -> m a)

------------------------------------------------------------------------------
-- Producers
------------------------------------------------------------------------------

{-
-- | Finish the computation.
{-# INLINE exit #-}
exit :: Monad m => Producer m a b
exit = Producer step return (\_ -> return None)

    where

    {-# INLINE_LATE step #-}
    step _ = return (Stop None)

-- | Finish the current iteration similar to 'empty' in the list monad.
{-# INLINE nilM #-}
nilM :: Monad m => (a -> m c) -> Producer m a b
nilM f = Producer step return (return . Stopping)

    where

    {-# INLINE_LATE step #-}
    step x = f x >> return (Stop None)

{-# INLINE nil #-}
nil :: Monad m => Producer m a b
nil = nilM (\_ -> return ())

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> Producer m a b
unfoldrM next = Producer step return (return . Resuming)

    where

    {-# INLINE_LATE step #-}
    step st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing -> Stop None
            -}

-- | Convert a list of pure values to a 'Stream'
--
-- /Internal/
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Producer m [a] a
fromList = Producer step return return

    where

    {-# INLINE_LATE step #-}
    step [] = return Stop
    step (x:[]) = return $ Result x
    step (x:xs) = return $ Partial x xs

------------------------------------------------------------------------------
-- Mapping
------------------------------------------------------------------------------

-- | Interconvert the producer between two interconvertible input types.
--
-- /Internal/
{-# INLINE_NORMAL translate #-}
translate :: Monad m =>
    (a -> c) -> (c -> a) -> Producer m c b -> Producer m a b
translate f g (Producer step inject extract) =
    Producer step1 (inject . f)  (fmap g . extract)

    where

    step1 st = do
        r <- step st
        return $ case r of
            Stop -> Stop
            Skip s -> Skip s
            Nil c -> Nil (g c)
            Result b -> Result b
            Final b c -> Final b (g c)
            Partial b s -> Partial b s

-- | Map the producer input to another value of the same type.
--
-- /Internal/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> a) -> Producer m a b -> Producer m a b
lmap f (Producer step inject extract) = Producer step (inject . f) extract

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

-- | Map a function on the output of the producer (the type @b@).
--
-- /Internal/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Producer m a b -> Producer m a c
map f (Producer ustep uinject uextract) = Producer step uinject uextract

    where

    {-# INLINE_LATE step #-}
    step st = fmap (fmap f) (ustep st)

-- | Maps a function on the output of the producer (the type @b@).
instance Functor m => Functor (Producer m a) where
    {-# INLINE fmap #-}
    fmap = map

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

-- | Identity unfold. Generates a singleton stream with the seed as the only
-- element in the stream.
--
-- > identity = singletonM return
--
{-# INLINE identity #-}
identity :: Applicative m => Producer m a a
identity = Producer step pure pure

    where

    step a = pure $ Final a a

{-# INLINE modify #-}
modify :: Applicative m => (a -> a) -> Producer m a ()
modify f = Producer step (pure . f) pure

    where

    step a = pure $ Nil a

{-# INLINE put #-}
put :: Applicative m => a -> Producer m a ()
put a = modify (Prelude.const a)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- XXX call it effect or valueM?
{-# INLINE const #-}
const :: Applicative m => m b -> Producer m a b
const m = Producer step pure pure

    where

    step a = (`Final` a) <$> m

data Cross s1 b s2 =
      CrossOuter s1
    | CrossInnerFinal b s2
    | CrossInnerPartial b s2

-- XXX Since we assume that when a producer is in Stopping state we can skip
-- resuming it, so any producer must not do any side effect when it is
-- stopping, otherwise when using applicative/monad the effect will not occur.
-- To be able to support that we will need the injected value to be wrapped in
-- the required state.
--
-- Two producers consuming from the same shared state in an interleaving
-- fashion and producing tuples with output from each one. For example, we can
-- interleave two Source.parseMany.
--
-- We can also have two interleaved producers running serially one after
-- another and producing elements in the same output stream:
--
-- interleave :: Monad m => Producer m a b -> Producer m a b -> Producer m a b
--
-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two producers.
--
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Producer m a b -> Producer m a c -> Producer m a (b, c)
cross (Producer step1 inject1 extract1) (Producer step2 inject2 extract2) =
    Producer step inject extract

    where

    -- XXX What if we want to stop the whole loop rather than stopping the
    -- iteration? How do we distinguish that? Perhaps we need "inject" to say
    -- Stop for that.
    inject a = do
        s1 <- inject1 a
        return $ CrossOuter s1

    {-# INLINE_LATE step #-}
    step (CrossOuter s1) = do
        r <- step1 s1
        case r of
            Stop -> return Stop
            Skip s -> return $ Skip (CrossOuter s)
            Nil a -> return $ Nil a
            -- XXX We have to discard the result here, inner loop cannot
            -- continue without a state. We should error or bakctrack.
            Result _ -> return Stop
            Final b a -> do
                s2 <- inject2 a
                return $ Skip (CrossInnerFinal b s2)
            Partial b s -> do
                res <- extract1 s
                s2 <- inject2 res
                return $ Skip (CrossInnerPartial b s2)
    step (CrossInnerFinal b s2) = do
        r <- step2 s2
        return $ case r of
            Stop -> Stop
            Skip s -> Skip (CrossInnerFinal b s)
            -- XXX We are discarding b here, it should be an error or we should
            -- backtrack.
            Nil a -> Nil a
            Result c -> Result (b, c)
            Final c a -> Final (b,c) a
            Partial c s -> Partial (b,c) (CrossInnerFinal b s)
    step (CrossInnerPartial b s2) = do
        r <- step2 s2
        case r of
            Stop -> return Stop
            Skip s -> return $ Skip (CrossInnerPartial b s)
            -- XXX We are discarding b here, it should be an error or we should
            -- backtrack.
            Nil a -> return $ Nil a
            Result c -> return $ Result (b, c)
            Final c a -> do
                s1 <- inject1 a
                return $ Partial (b,c) (CrossOuter s1)
            Partial c s -> return $ Partial (b,c) (CrossInnerPartial b s)

    extract (CrossOuter s1) = extract1 s1
    extract (CrossInnerFinal _ s2) = extract2 s2
    extract (CrossInnerPartial _ s2) = extract2 s2

-- | Example:
--
-- >>> Stream.toList $ Stream.produce ((,) <$> Producer.fromList <*> Producer.fromList) [1,2,3,4]
-- [(1,2),(1,3),(1,4)]
--
instance Monad m => Applicative (Producer m a) where
    {-# INLINE pure #-}
    pure = const Prelude.. return

    {-# INLINE (<*>) #-}
    u1 <*> u2 = fmap (\(a, b) -> a b) (cross u1 u2)

    -- {-# INLINE (*>) #-}
    -- (*>) = apSequence

    -- {-# INLINE (<*) #-}
    -- (<*) = apDiscardSnd

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

data ConcatMapState m a b s1 =
      ConcatMapOuter s1
    | forall s2. ConcatMapInnerFinal s2 (s2 -> m (Step s2 a b)) (s2 -> m a)
    | forall s2. ConcatMapInnerPartial s2 (s2 -> m (Step s2 a b)) (s2 -> m a)

-- XXX For the composition to stop and not go in an infinite loop, the source
-- must return Nothing in the end. If it returns an empty Just state then we
-- may keep going in a loop. This is similar to the infinite looping in folds
-- when we use an empty returning parser in foldMany etc.
--
-- | Map a producer generating action to each element of a producer and
-- flatten the results into a single stream. Each producer consumes from the
-- same shared state.
--
{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m
    => (b -> m (Producer m a c)) -> Producer m a b -> Producer m a c
concatMapM f (Producer step1 inject1 extract1) = Producer step inject extract

    where

    inject a = do
        s1 <- inject1 a
        return $ ConcatMapOuter s1

    {-# INLINE_LATE step #-}
    step (ConcatMapOuter s1) = do
        r <- step1 s1
        case r of
            Stop -> return Stop
            Skip s -> return $ Skip (ConcatMapOuter s)
            Nil a -> return $ Nil a
            -- XXX We have to discard the result here, inner loop cannot
            -- continue without a state. We should error or bakctrack.
            Result b -> do -- trace "discarding result" (return Stop)
                Producer step2 inject2 extract2 <- f b
                -- XXX we need to support Nothing as input state
                s2 <- inject2 undefined
                return $ Skip (ConcatMapInnerFinal s2 step2 extract2)
            Final b a -> do
                Producer step2 inject2 extract2 <- f b
                s2 <- inject2 a
                return $ Skip (ConcatMapInnerFinal s2 step2 extract2)
            Partial b s -> do
                Producer step2 inject2 extract2 <- f b
                a <- extract1 s
                s2 <- inject2 a
                return $ Skip (ConcatMapInnerPartial s2 step2 extract2)
    step (ConcatMapInnerFinal s2 step2 extract2) = do
        trace "inner final" (return ())
        r <- step2 s2
        return $ case r of
            Stop -> Stop
            Skip s -> Skip (ConcatMapInnerFinal s step2 extract2)
            -- XXX We are discarding the result of outer loop here, it should
            -- be an error or we should backtrack.
            Nil a -> Nil a
            Result c -> Result c
            Final c a -> Final c a
            Partial c s -> Partial c (ConcatMapInnerFinal s step2 extract2)
    step (ConcatMapInnerPartial s2 step2 extract2) = do
        trace "inner partial" (return ())
        r <- step2 s2
        case r of
            Stop -> return Stop
            Skip s -> return $ Skip (ConcatMapInnerPartial s step2 extract2)
            Nil a -> return $ Nil a
            Result c -> return $ Result c
            Final c a -> do
                s1 <- inject1 a
                return $ Partial c (ConcatMapOuter s1)
            Partial c s -> return $ Partial c (ConcatMapInnerPartial s step2 extract2)

    extract (ConcatMapOuter s1) = extract1 s1
    extract (ConcatMapInnerFinal s2 _ extract2) = extract2 s2
    extract (ConcatMapInnerPartial s2 _ extract2) = extract2 s2

{-# INLINE concatMap #-}
concatMap :: Monad m =>
    (b -> Producer m a c) -> Producer m a b -> Producer m a c
concatMap f = concatMapM (return Prelude.. f)

-- Note: concatMap and Monad instance for producers have performance comparable
-- to Stream. In fact, concatMap is slower than Stream, that may be some
-- optimization issue though.
--
-- | Example:
--
-- >>> u = do { x <- Producer.fromList; y <- Producer.fromList; return (x,y); }
-- >>> Stream.toList $ Stream.produce u [1,2,3,4]
-- [(1,2),(1,3),(1,4)]
--
instance Monad m => Monad (Producer m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    -- {-# INLINE (>>) #-}
    -- (>>) = (*>)

{-
-- XXX requires the type to be "Producer a m b"
instance MonadTrans Producer where
    {-# INLINE lift #-}
    lift = const
-}

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | State representing a nested loop.
{-# ANN type NestedLoop Fuse #-}
data NestedLoop s1 s2 a =
      OuterLoop s1
    | InnerLoopResult s2
    | InnerLoopFinal a s2
    | InnerLoopPartial s1 s2

            {-
-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Internal/
--
{-# INLINE_NORMAL concat #-}
concat :: Monad m =>
    Producer m a b -> Producer m b c -> Producer m (NestedLoop a b) c
concat (Producer step1 inject1 extract1) (Producer step2 inject2 extract2) =
    Producer step inject extract

    where

    inject (OuterLoop x) = do
        s <- inject1 x
        return $ OuterLoop s
    inject (InnerLoop x y) = do
        s1 <- inject1 x
        s2 <- inject2 y
        return $ InnerLoop s1 s2

    {-# INLINE_LATE step #-}
    step (OuterLoop st) = do
        r <- step1 st
        case r of
            Yield b s -> do
                s2 <- inject2 b
                return $ Skip (InnerLoop s s2)
            Skip s -> return $ Skip (OuterLoop s)
            Stop res -> return $ Stop $ fmap OuterLoop res

    step (InnerLoop s1 s2) = do
        r <- step2 s2
        return $ case r of
            Yield c s -> Yield c (InnerLoop s1 s)
            Skip s -> Skip (InnerLoop s1 s)
            Stop res ->
                case res of
                    Nothing -> Skip (OuterLoop s1)
                    -- XXX When the state is not fully consumed should we stop
                    -- or discard and continue or error out?
                    Just _ -> Skip (OuterLoop s1)

    extract (OuterLoop s1) = fmap OuterLoop <$> extract1 s1
    extract (InnerLoop s1 s2) = do
        r1 <- extract1 s1
        case r1 of
            Nothing -> return Nothing
            Just a -> do
                r2 <- extract2 s2
                return $ Just $ case r2 of
                    Nothing -> OuterLoop a
                    Just b -> InnerLoop a b
                    -}

-- XXX Should we use a single element unread or a list?
-- | Elements of type "b" can be pushed back to the type "a"
class Unread a b where
    unread :: [b] -> a -> a

-- XXX use "Unread a b"
-- XXX Leftover State of b will be dropped
{-# INLINE_NORMAL concat_ #-}
concat_ :: Monad m => Producer m a b -> Producer m b c -> Producer m a c
concat_ (Producer step1 inject1 extract1) (Producer step2 inject2 _) =
    Producer step inject extract

    where

    inject a = do
        s <- inject1 a
        return $ OuterLoop s

    {-# INLINE_LATE step #-}
    step (OuterLoop st) = do
        r <- step1 st
        case r of
            Stop -> return Stop
            Skip s -> return $ Skip (OuterLoop s)
            Nil a -> return $ Nil a
            Result b -> do
                s2 <- inject2 b
                return $ Skip (InnerLoopResult s2)
            Final b a -> do
                s2 <- inject2 b
                return $ Skip (InnerLoopFinal a s2)
            Partial b s -> do
                s2 <- inject2 b
                return $ Skip (InnerLoopPartial s s2)
    -- XXX If the state is extracted here then we cannot resume from where we
    -- left.
    step (InnerLoopResult s2) = do
        r <- step2 s2
        case r of
            Stop -> return Stop
            -- XXX discarding the inner state here
            Nil _ -> return Stop
            Result c -> return $ Result c
            -- XXX discarding the inner state here
            Final c _ -> return $ Result c
            Skip s -> return $ Skip (InnerLoopResult s)
            Partial c s -> return $ Partial c (InnerLoopResult s)
    step (InnerLoopFinal a s2) = do
        r <- step2 s2
        case r of
            Stop -> return $ Nil a
            -- XXX discarding the inner state here
            Nil _ -> return $ Nil a
            Result c -> return $ Final c a
            -- XXX discarding the inner state here
            Final c _ -> return $ Final c a
            Skip s -> return $ Skip (InnerLoopFinal a s)
            Partial c s -> return $ Partial c (InnerLoopFinal a s)
    step (InnerLoopPartial s1 s2) = do
        r <- step2 s2
        case r of
            Stop -> return $ Skip (OuterLoop s1)
            Skip s -> return $ Skip (InnerLoopPartial s1 s)
            -- XXX discarding the inner state here
            Nil _ -> return $ Skip (OuterLoop s1)
            Result c -> return $ Partial c (OuterLoop s1)
            -- XXX discarding inner state here
            Final c _ -> return $ Partial c (OuterLoop s1)
            Partial c s -> return $ Partial c (InnerLoopPartial s1 s)

    extract (OuterLoop s1) = extract1 s1
    -- Requires extract to return a Maybe type
    extract (InnerLoopResult _) = error "Not handled" -- return Nothing
    extract (InnerLoopFinal a _) = return a
    extract (InnerLoopPartial s1 _) = extract1 s1
