-- |
-- Module      : Streamly.Internal.Data.Unfold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run the examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold

module Streamly.Internal.Data.Unfold.Type
    ( Unfold (..)

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

    -- * Transformations
    , lmap
    , map

    -- * Nesting
    , ConcatState (..)
    , many

    -- Applicative
    , apSequence
    , apDiscardSnd
    , crossWithM
    , crossWith
    , cross
    , apply

    -- Monad
    , concatMapM
    , concatMap
    , bind

    , zipWithM
    , zipWith
    )
where

#include "inline.hs"

-- import Control.Arrow (Arrow(..))
-- import Control.Category (Category(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))

import Prelude hiding (const, map, concatMap, zipWith)

-- $setup
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold

------------------------------------------------------------------------------
-- Monadic Unfolds
------------------------------------------------------------------------------

-- The order of arguments allows 'Category' and 'Arrow' instances but precludes
-- contravariant and contra-applicative.
--
-- | An @Unfold m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- @since 0.7.0

data Unfold m a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

------------------------------------------------------------------------------
-- Basic constructors
------------------------------------------------------------------------------

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
--
-- | Build a stream by unfolding a /monadic/ step function starting from a seed.
-- The step function returns the next element in the stream and the next seed
-- value. When it is done it returns 'Nothing' and the stream ends.
--
-- /Since: 0.8.0/
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
-- /Since: 0.8.0/
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
-- @
-- lmap f = Unfold.many (Unfold.function f)
-- @
--
-- /Since: 0.8.0/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (uinject Prelude.. f)

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

-- | Map a function on the output of the unfold (the type @b@).
--
-- /Pre-release/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Unfold m a b -> Unfold m a c
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

-- | Discards the unfold input and always returns the argument of 'fromPure'.
--
-- prop> fromPure = fromEffect . pure
--
-- /Pre-release/
fromPure :: Applicative m => b -> Unfold m a b
fromPure = fromEffect Prelude.. pure

-- | Outer product discarding the first element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL apSequence #-}
apSequence :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a c
apSequence (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

-- | Outer product discarding the second element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL apDiscardSnd #-}
apDiscardSnd :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a b
apDiscardSnd (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

data Cross a s1 b s2 = CrossOuter a s1 | CrossInner a s1 b s2

-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two unfolds using a monadic combining function.
--
-- /Pre-release/
{-# INLINE_NORMAL crossWithM #-}
crossWithM :: Monad m =>
    (b -> c -> m d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
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

-- | Like 'crossWithM' but uses a pure combining function.
--
-- prop> crossWith f = crossWithM (\b c -> return $ f b c)
--
-- >>> u1 = Unfold.lmap fst Unfold.fromList
-- >>> u2 = Unfold.lmap snd Unfold.fromList
-- >>> u = Unfold.crossWith (,) u1 u2
-- >>> Unfold.fold Fold.toList u ([1,2,3], [4,5,6])
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
--
-- /Since: 0.8.0/
--
{-# INLINE crossWith #-}
crossWith :: Monad m =>
    (b -> c -> d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
crossWith f = crossWithM (\b c -> return $ f b c)

-- | See 'crossWith'.
--
-- prop> cross = crossWith (,)
--
-- To cross the streams from a tuple we can write:
--
-- @
-- crossProduct :: Monad m => Unfold m a b -> Unfold m c d -> Unfold m (a, c) (b, d)
-- crossProduct u1 u2 = cross (lmap fst u1) (lmap snd u2)
-- @
--
-- /Pre-release/
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
cross = crossWith (,)

apply :: Monad m => Unfold m a (b -> c) -> Unfold m a b -> Unfold m a c
apply u1 u2 = fmap (\(a, b) -> a b) (cross u1 u2)

{-
-- | Example:
--
-- >>> rlist = Unfold.lmap fst Unfold.fromList
-- >>> llist = Unfold.lmap snd Unfold.fromList
-- >>> Stream.toList $ Stream.unfold ((,) <$> rlist <*> llist) ([1,2],[3,4])
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
-- >>> Stream.toList $ Stream.unfold u 1
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
-- /Since: 0.8.0/

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
-- prop> function f = functionM $ return . f
--
-- /Since: 0.8.0/

{-# INLINE function #-}
function :: Applicative m => (a -> b) -> Unfold m a b
function f = functionM $ pure Prelude.. f

-- | Identity unfold. The unfold generates a singleton stream having the input
-- as the only element.
--
-- prop> identity = function Prelude.id
--
-- /Pre-release/
{-# INLINE identity #-}
identity :: Applicative m => Unfold m a a
identity = function Prelude.id

{-# ANN type ConcatState Fuse #-}
data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

-- | Apply the second unfold to each output element of the first unfold and
-- flatten the output in a single stream.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL many #-}
many :: Monad m => Unfold m a b -> Unfold m b c -> Unfold m a c
many (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

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
    (.) = flip many
-}

-------------------------------------------------------------------------------
-- Zipping
-------------------------------------------------------------------------------

-- | Distribute the input to two unfolds and then zip the outputs to a single
-- stream using a monadic zip function.
--
-- Stops as soon as any of the unfolds stops.
--
-- /Pre-release/
{-# INLINE_NORMAL zipWithM #-}
zipWithM :: Monad m
    => (b -> c -> m d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
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

-- | Like 'zipWithM' but with a pure zip function.
--
-- >>> square = fmap (\x -> x * x) Unfold.fromList
-- >>> cube = fmap (\x -> x * x * x) Unfold.fromList
-- >>> u = Unfold.zipWith (,) square cube
-- >>> Unfold.fold Fold.toList u [1..5]
-- [(1,1),(4,8),(9,27),(16,64),(25,125)]
--
-- prop> zipWith f = zipWithM (\a b -> return $ f a b)
--
-- /Since: 0.8.0/
--
{-# INLINE zipWith #-}
zipWith :: Monad m
    => (b -> c -> d) -> Unfold m a b -> Unfold m a c -> Unfold m a d
zipWith f = zipWithM (\a b -> return (f a b))

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
