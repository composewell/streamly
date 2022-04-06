#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run the examples in this module:
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
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
-- 'unfoldMany' operations, the latter is more efficient.  'concatMap'
-- generates a new stream object from each element in the stream by applying
-- the supplied function to the element, the stream object includes the "step"
-- function as well as the initial "state" of the stream.  Since the stream is
-- generated dynamically the compiler does not know the step function or the
-- state type statically at compile time, therefore, it cannot inline it. On
-- the other hand in case of 'unfoldMany' the compiler has visibility into
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

-- Open control flow style streams can also have two representations. StreamK
-- is a producer style representation. We can also have a consumer style
-- representation. We can use that for composable folds in StreamK
-- representation.
--

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
-- example, the 'Streamly.Prelude.concatMap' combinator which uses @a -> t m b@
-- (where @t@ is a stream type) to generate streams is much less efficient
-- compared to 'Streamly.Prelude.unfoldMany'.
--
-- On the other hand, transformation operations on stream types are as
-- efficient as transformations on 'Unfold'.
--
-- We should note that in some cases working with stream types may be more
-- convenient compared to working with the 'Unfold' type.  However, if extra
-- performance boost is important then 'Unfold' based composition should be
-- preferred compared to stream based composition when merging or concatenating
-- streams.

module Streamly.Internal.Data.Unfold
    (
    -- * Unfold Type
      Step(..)
    , Unfold

    -- * Unfolds
    -- One to one correspondence with
    -- "Streamly.Internal.Data.Stream.IsStream.Generate"
    -- ** Basic Constructors
    , mkUnfoldM
    , mkUnfoldrM
    , unfoldrM
    , unfoldr
    , functionM
    , function
    , identity
    , nilM
    , consM

    -- ** From Values
    , fromEffect
    , fromPure

    -- ** Generators
    -- | Generate a monadic stream from a seed.
    , repeatM
    , replicateM
    , fromIndicesM
    , iterateM

    -- ** Enumerations
    , Enumerable (..)

    -- ** Enumerate Num
    , enumerateFromNum
    , enumerateFromThenNum
    , enumerateFromStepNum

    -- ** Enumerating 'Bounded 'Integral' Types
    , enumerateFromIntegralBounded
    , enumerateFromThenIntegralBounded
    , enumerateFromToIntegralBounded
    , enumerateFromThenToIntegralBounded

    -- ** Enumerating 'Unounded Integral' Types
    , enumerateFromIntegral
    , enumerateFromThenIntegral
    , enumerateFromToIntegral
    , enumerateFromThenToIntegral

    -- ** Enumerating 'Small Integral' Types
    , enumerateFromSmallBounded
    , enumerateFromThenSmallBounded
    , enumerateFromToSmall
    , enumerateFromThenToSmall

    -- ** Enumerating 'Fractional' Types
    , enumerateFromFractional
    , enumerateFromThenFractional
    , enumerateFromToFractional
    , enumerateFromThenToFractional

    -- ** From Containers
    , fromList
    , fromListM

    , fromStream
    , fromStreamK
    , fromStreamD

    -- * Combinators
    -- ** Mapping on Input
    , lmap
    , lmapM
    , both
    , first
    , second
    , discardFirst
    , discardSecond
    , swap
    -- coapply
    -- comonad

    -- * Folding
    , fold

    -- ** Mapping on Output
    , map
    , mapM
    , mapMWithInput
    , postscanlM'
    , postscan
    , foldMany
    -- pipe

    -- ** Either Wrapped Input
    , either

    -- ** Filtering
    , takeWhileM
    , takeWhile
    , take
    , filter
    , filterM
    , drop
    , dropWhile
    , dropWhileM

    -- ** Zipping
    , zipWithM
    , zipWith

    -- ** Cross product
    , crossWithM
    , crossWith
    , cross
    , apply

    -- ** Nesting
    , ConcatState (..)
    , many
    , concatMapM
    , bind

    -- ** Resource Management
    , gbracket_
    , gbracket
    , before
    , after
    , after_
    , finally
    , finally_
    , bracket
    , bracket_

    -- ** Exceptions
    , onException
    , handle
    )
where

import Control.Exception (Exception, mask_)
import Control.Monad.Catch (MonadCatch)
import Data.Functor (($>))
import GHC.Types (SPEC(..))
import Streamly.Internal.Control.Concurrent (MonadRunInIO, MonadAsync, withRunInIO)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.IOFinalizer
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..), Step(..))
import Streamly.Internal.Data.SVar.Type (defState)

import qualified Control.Monad.Catch as MC
import qualified Data.Tuple as Tuple
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Unfold.Enumeration
import Streamly.Internal.Data.Unfold.Type
import Prelude
       hiding (map, mapM, takeWhile, take, filter, const, zipWith
              , drop, dropWhile, either)

-- $setup
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Prelude as Stream


-- | Convert an 'Unfold' into an unfold accepting a tuple as an argument,
-- using the argument of the original fold as the second element of tuple and
-- discarding the first element of the tuple.
--
-- @
-- discardFirst = Unfold.lmap snd
-- @
--
-- /Pre-release/
--
{-# INLINE_NORMAL discardFirst #-}
discardFirst :: Unfold m a b -> Unfold m (c, a) b
discardFirst = lmap snd

-- | Convert an 'Unfold' into an unfold accepting a tuple as an argument,
-- using the argument of the original fold as the first element of tuple and
-- discarding the second element of the tuple.
--
-- @
-- discardSecond = Unfold.lmap fst
-- @
--
-- /Pre-release/
--
{-# INLINE_NORMAL discardSecond #-}
discardSecond :: Unfold m a b -> Unfold m (a, c) b
discardSecond = lmap fst

-- | Convert an 'Unfold' that accepts a tuple as an argument into an unfold
-- that accepts a tuple with elements swapped.
--
-- @
-- swap = Unfold.lmap Tuple.swap
-- @
--
-- /Pre-release/
--
{-# INLINE_NORMAL swap #-}
swap :: Unfold m (a, c) b -> Unfold m (c, a) b
swap = lmap Tuple.swap

-------------------------------------------------------------------------------
-- Output operations
-------------------------------------------------------------------------------

-- XXX Do we need this combinator or the stream based idiom is enough?

-- | Compose an 'Unfold' and a 'Fold'. Given an @Unfold m a b@ and a
-- @Fold m b c@, returns a monadic action @a -> m c@ representing the
-- application of the fold on the unfolded stream.
--
-- >>> Unfold.fold Fold.sum Unfold.fromList [1..100]
-- 5050
--
-- >>> fold f u = Stream.fold f . Stream.unfold u
--
-- /Pre-release/
--
{-# INLINE_NORMAL fold #-}
fold :: Monad m => Fold m b c -> Unfold m a b -> a -> m c
fold (Fold fstep initial extract) (Unfold ustep inject) a = do
    res <- initial
    case res of
        FL.Partial x -> inject a >>= go SPEC x
        FL.Done b -> return b

    where

    {-# INLINE_LATE go #-}
    go !_ !fs st = do
        r <- ustep st
        case r of
            Yield x s -> do
                res <- fstep fs x
                case res of
                    FL.Partial fs1 -> go SPEC fs1 s
                    FL.Done c -> return c
            Skip s -> go SPEC fs s
            Stop -> extract fs

-- {-# ANN type FoldMany Fuse #-}
data FoldMany s fs b a
    = FoldManyStart s
    | FoldManyFirst fs s
    | FoldManyLoop s fs
    | FoldManyYield b (FoldMany s fs b a)
    | FoldManyDone

-- | Apply a fold multiple times on the output of an unfold.
--
-- /Pre-release/
{-# INLINE_NORMAL foldMany #-}
foldMany :: Monad m => Fold m b c -> Unfold m a b -> Unfold m a c
foldMany (Fold fstep initial extract) (Unfold ustep inject1) =
    Unfold step inject

    where

    inject x = do
        r <- inject1 x
        return (FoldManyStart r)

    {-# INLINE consume #-}
    consume x s fs = do
        res <- fstep fs x
        return
            $ Skip
            $ case res of
                  FL.Done b -> FoldManyYield b (FoldManyStart s)
                  FL.Partial ps -> FoldManyLoop s ps

    {-# INLINE_LATE step #-}
    step (FoldManyStart st) = do
        r <- initial
        return
            $ Skip
            $ case r of
                  FL.Done b -> FoldManyYield b (FoldManyStart st)
                  FL.Partial fs -> FoldManyFirst fs st
    step (FoldManyFirst fs st) = do
        r <- ustep st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyFirst fs s)
            Stop -> return Stop
    step (FoldManyLoop st fs) = do
        r <- ustep st
        case r of
            Yield x s -> consume x s fs
            Skip s -> return $ Skip (FoldManyLoop s fs)
            Stop -> do
                b <- extract fs
                return $ Skip (FoldManyYield b FoldManyDone)
    step (FoldManyYield b next) = return $ Yield b next
    step FoldManyDone = return Stop

-- | Apply a monadic function to each element of the stream and replace it
-- with the output of the resulting action.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (b -> m c) -> Unfold m a b -> Unfold m a c
mapM f (Unfold ustep uinject) = Unfold step uinject
    where
    {-# INLINE_LATE step #-}
    step st = do
        r <- ustep st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

{-# INLINE_NORMAL mapMWithInput #-}
mapMWithInput :: Monad m => (a -> b -> m c) -> Unfold m a b -> Unfold m a c
mapMWithInput f (Unfold ustep uinject) = Unfold step inject
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

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

-- | Make an unfold operate on values wrapped in an @Either a a@ type. 'Right
-- a' translates to 'Right b' and 'Left a' translates to 'Left b'.
--
-- /Internal/
{-# INLINE_NORMAL either #-}
either :: Applicative m => Unfold m a b -> Unfold m (Either a a) (Either b b)
either (Unfold step1 inject1) = Unfold step inject

    where

    inject (Left a) = (, Left) <$> inject1 a
    inject (Right a) = (, Right) <$> inject1 a

    {-# INLINE_LATE step #-}
    step (st, f) = do
        (\case
            Yield x s -> Yield (f x) (s, f)
            Skip s -> Skip (s, f)
            Stop -> Stop) <$> step1 st

-- | Scan the output of an 'Unfold' to change it in a stateful manner.
--
-- /Pre-release/
{-# INLINE_NORMAL postscan #-}
postscan :: Monad m => Fold m b c -> Unfold m a b -> Unfold m a c
postscan (Fold stepF initial extract) (Unfold stepU injectU) =
    Unfold step inject

    where

    inject a =  do
        r <- initial
        case r of
            FL.Partial fs -> Just . (fs,) <$> injectU a
            FL.Done _ -> return Nothing

    {-# INLINE_LATE step #-}
    step (Just (fs, us)) = do
        ru <- stepU us
        case ru of
            Yield x s -> do
                rf <- stepF fs x
                case rf of
                    FL.Done v -> return $ Yield v Nothing
                    FL.Partial fs1 -> do
                        v <- extract fs1
                        return $ Yield v (Just (fs1, s))
            Skip s -> return $ Skip (Just (fs, s))
            Stop -> return Stop

    step Nothing = return Stop

-- | Scan the output of an 'Unfold' to change it in a stateful manner.
--
-- /Pre-release/
{-# INLINE_NORMAL postscanlM' #-}
postscanlM' :: Monad m => (b -> a -> m b) -> m b -> Unfold m c a -> Unfold m c b
postscanlM' f z = postscan (FL.foldlM' f z)

-------------------------------------------------------------------------------
-- Convert streams into unfolds
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromStreamD #-}
fromStreamD :: Applicative m => Unfold m (Stream m a) a
fromStreamD = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step (UnStream step1 state1) =
        (\case
            Yield x s -> Yield x (Stream step1 s)
            Skip s    -> Skip (Stream step1 s)
            Stop      -> Stop) <$> step1 defState state1

{-# INLINE_NORMAL fromStreamK #-}
fromStreamK :: Applicative m => Unfold m (K.Stream m a) a
fromStreamK = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step stream = do
        (\case
            Just (x, xs) -> Yield x xs
            Nothing -> Stop) <$> K.uncons stream

-- XXX Using Unfold.fromStreamD seems to be faster (using cross product test
-- case) than using fromStream even if it is implemented using fromStreamD.
-- Check if StreamK to StreamD rewrite rules are working correctly when
-- implementing fromStream using fromStreamD.
--
-- | Convert a stream into an 'Unfold'. Note that a stream converted to an
-- 'Unfold' may not be as efficient as an 'Unfold' in some situations.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL fromStream #-}
fromStream :: Applicative m => Unfold m (SerialT m a) a
fromStream = lmap getSerialT fromStreamK

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Lift a monadic function into an unfold generating a nil stream with a side
-- effect.
--
{-# INLINE nilM #-}
nilM :: Applicative m => (a -> m c) -> Unfold m a b
nilM f = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step x = f x $> Stop

-- | Prepend a monadic single element generator function to an 'Unfold'. The
-- same seed is used in the action as well as the unfold.
--
-- /Pre-release/
{-# INLINE_NORMAL consM #-}
consM :: Applicative m => (a -> m b) -> Unfold m a b -> Unfold m a b
consM action unf = Unfold step inject

    where

    inject = pure . Left

    {-# INLINE_LATE step #-}
    step (Left a) = (`Yield` Right (D.unfold unf a)) <$> action a
    step (Right (UnStream step1 st)) = do
        (\case
            Yield x s -> Yield x (Right (Stream step1 s))
            Skip s -> Skip (Right (Stream step1 s))
            Stop -> Stop) <$> step1 defState st

-- XXX Check if "unfold (fromList [1..10])" fuses, if it doesn't we can use
-- rewrite rules to rewrite list enumerations to unfold enumerations.
--
-- | Convert a list of pure values to a 'Stream'
--
-- /Since: 0.8.0/
--
{-# INLINE_LATE fromList #-}
fromList :: Applicative m => Unfold m [a] a
fromList = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step (x:xs) = pure $ Yield x xs
    step [] = pure Stop

-- | Convert a list of monadic values to a 'Stream'
--
-- /Since: 0.8.0/
--
{-# INLINE_LATE fromListM #-}
fromListM :: Applicative m => Unfold m [m a] a
fromListM = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step (x:xs) = (`Yield` xs) <$> x
    step [] = pure Stop

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

-- | Generates a stream replicating the seed @n@ times.
--
-- /Since: 0.8.0/
--
{-# INLINE replicateM #-}
replicateM :: Applicative m => Int -> Unfold m (m a) a
replicateM n = Unfold step inject

    where

    inject action = pure (action, n)

    {-# INLINE_LATE step #-}
    step (action, i) =
        if i <= 0
        then pure Stop
        else (\x -> Yield x (action, i - 1)) <$> action

-- | Generates an infinite stream repeating the seed.
--
-- /Since: 0.8.0/
--
{-# INLINE repeatM #-}
repeatM :: Applicative m => Unfold m (m a) a
repeatM = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step action = (`Yield` action) <$> action

-- | Generates an infinite stream starting with the given seed and applying the
-- given function repeatedly.
--
-- /Since: 0.8.0/
--
{-# INLINE iterateM #-}
iterateM :: Applicative m => (a -> m a) -> Unfold m (m a) a
iterateM f = Unfold step id

    where

    {-# INLINE_LATE step #-}
    step x = Yield x <$> f x

-- | @fromIndicesM gen@ generates an infinite stream of values using @gen@
-- starting from the seed.
--
-- @
-- fromIndicesM f = Unfold.mapM f $ Unfold.enumerateFrom 0
-- @
--
-- /Pre-release/
--
{-# INLINE_NORMAL fromIndicesM #-}
fromIndicesM :: Applicative m => (Int -> m a) -> Unfold m Int a
fromIndicesM gen = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step i = (`Yield` (i + 1)) <$> gen i

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

-- |
-- >>> u = Unfold.take 2 Unfold.fromList
-- >>> Unfold.fold Fold.toList u [1..100]
-- [1,2]
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL take #-}
take :: Applicative m => Int -> Unfold m a b -> Unfold m a b
take n (Unfold step1 inject1) = Unfold step inject

    where

    inject x = (, 0) <$> inject1 x

    {-# INLINE_LATE step #-}
    step (st, i) | i < n = do
        (\case
            Yield x s -> Yield x (s, i + 1)
            Skip s -> Skip (s, i)
            Stop   -> Stop) <$> step1 st
    step (_, _) = pure Stop

-- | Same as 'filter' but with a monadic predicate.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
filterM f (Unfold step1 inject1) = Unfold step inject1
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Skip s
            Skip s -> return $ Skip s
            Stop   -> return Stop

-- | Include only those elements that pass a predicate.
--
-- /Since: 0.8.0/
--
{-# INLINE filter #-}
filter :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
filter f = filterM (return . f)

-- | @drop n unf@ drops @n@ elements from the stream generated by @unf@.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL drop #-}
drop :: Applicative m => Int -> Unfold m a b -> Unfold m a b
drop n (Unfold step inject) = Unfold step' inject'

    where

    inject' a = (, n) <$> inject a

    {-# INLINE_LATE step' #-}
    step' (st, i)
        | i > 0 = do
            (\case
                  Yield _ s -> Skip (s, i - 1)
                  Skip s -> Skip (s, i)
                  Stop -> Stop) <$> step st
        | otherwise = do
            (\case
                  Yield x s -> Yield x (s, 0)
                  Skip s -> Skip (s, 0)
                  Stop -> Stop) <$> step st

-- | @dropWhileM f unf@ drops elements from the stream generated by @unf@ while
-- the condition holds true. The condition function @f@ is /monadic/ in nature.
--
-- /Since: 0.8.0/
--
{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
dropWhileM f (Unfold step inject) = Unfold step' inject'

    where

    inject' a = do
        b <- inject a
        return $ Left b

    {-# INLINE_LATE step' #-}
    step' (Left st) = do
        r <- step st
        case r of
            Yield x s -> do
                b <- f x
                return
                    $ if b
                      then Skip (Left s)
                      else Yield x (Right s)
            Skip s -> return $ Skip (Left s)
            Stop -> return Stop
    step' (Right st) = do
        r <- step st
        return
            $ case r of
                  Yield x s -> Yield x (Right s)
                  Skip s -> Skip (Right s)
                  Stop -> Stop

-- | Similar to 'dropWhileM' but with a pure condition function.
--
-- /Since: 0.8.0/
--
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
dropWhile f = dropWhileM (return . f)

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Like 'gbracket' but with following differences:
--
-- * alloc action @a -> m c@ runs with async exceptions enabled
-- * cleanup action @c -> m d@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * does not require a 'MonadAsync' constraint.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE_NORMAL gbracket_ #-}
gbracket_
    :: Monad m
    => (a -> m c)                           -- ^ before
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> (c -> m d)                           -- ^ after, on normal stop
    -> Unfold m (c, e) b                    -- ^ on exception
    -> Unfold m c b                         -- ^ unfold to run
    -> Unfold m a b
gbracket_ bef exc aft (Unfold estep einject) (Unfold step1 inject1) =
    Unfold step inject

    where

    inject x = do
        r <- bef x
        s <- inject1 r
        return $ Right (s, r)

    {-# INLINE_LATE step #-}
    step (Right (st, v)) = do
        res <- exc $ step1 st
        case res of
            Right r -> case r of
                Yield x s -> return $ Yield x (Right (s, v))
                Skip s    -> return $ Skip (Right (s, v))
                Stop      -> aft v >> return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e -> do
                r <- einject (v, e)
                return $ Skip (Left r)
    step (Left st) = do
        res <- estep st
        return $ case res of
            Yield x s -> Yield x (Left s)
            Skip s    -> Skip (Left s)
            Stop      -> Stop

-- | Run the alloc action @a -> m c@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @c@ as input to @Unfold m c b@ to generate an output stream. When
-- unfolding use the supplied @try@ operation @forall s. m s -> m (Either e s)@
-- to catch synchronous exceptions. If an exception occurs run the exception
-- handling unfold @Unfold m (c, e) b@.
--
-- The cleanup action @c -> m d@, runs whenever the stream ends normally, due
-- to a sync or async exception or if it gets garbage collected after a partial
-- lazy evaluation.  See 'bracket' for the semantics of the cleanup action.
--
-- 'gbracket' can express all other exception handling combinators.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL gbracket #-}
gbracket
    :: MonadRunInIO m
    => (a -> m c)                           -- ^ before
    -> (c -> m d)                           -- ^ after, on normal stop, or GC
    -> Unfold m (c, e) b                    -- ^ on exception
    -> (forall s. m s -> m (Either e s))    -- ^ try (exception handling)
    -> Unfold m c b                         -- ^ unfold to run
    -> Unfold m a b
gbracket bef aft (Unfold estep einject) ftry (Unfold step1 inject1) =
    Unfold step inject

    where

    inject x = do
        -- Mask asynchronous exceptions to make the execution of 'bef' and
        -- the registration of 'aft' atomic. See comment in 'D.gbracketIO'.
        (r, ref) <- withRunInIO $ \run -> mask_ $ run $ do
            r <- bef x
            ref <- newIOFinalizer (aft r)
            return (r, ref)
        s <- inject1 r
        return $ Right (s, r, ref)

    {-# INLINE_LATE step #-}
    step (Right (st, v, ref)) = do
        res <- ftry $ step1 st
        case res of
            Right r -> case r of
                Yield x s -> return $ Yield x (Right (s, v, ref))
                Skip s    -> return $ Skip (Right (s, v, ref))
                Stop      -> do
                    runIOFinalizer ref
                    return Stop
            -- XXX Do not handle async exceptions, just rethrow them.
            Left e -> do
                -- Clearing of finalizer and running of exception handler must
                -- be atomic wrt async exceptions. Otherwise if we have cleared
                -- the finalizer and have not run the exception handler then we
                -- may leak the resource.
                r <- clearingIOFinalizer ref (einject (v, e))
                return $ Skip (Left r)
    step (Left st) = do
        res <- estep st
        return $ case res of
            Yield x s -> Yield x (Left s)
            Skip s    -> Skip (Left s)
            Stop      -> Stop

-- | Run a side effect @a -> m c@ on the input @a@ before unfolding it using
-- @Unfold m a b@.
--
-- > before f = lmapM (\a -> f a >> return a)
--
-- /Pre-release/
{-# INLINE_NORMAL before #-}
before :: (a -> m c) -> Unfold m a b -> Unfold m a b
before action (Unfold step inject) = Unfold step (action >> inject)

-- The custom implementation of "after_" is slightly faster (5-7%) than
-- "_after".  This is just to document and make sure that we can always use
-- gbracket to implement after_ The same applies to other combinators as well.
--
{-# INLINE_NORMAL _after #-}
_after :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
_after aft = gbracket_ return (fmap Right) aft undefined

-- | Like 'after' with following differences:
--
-- * action @a -> m c@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * Monad @m@ does not require any other constraints.
--
-- /Pre-release/
{-# INLINE_NORMAL after_ #-}
after_ :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
after_ action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> action v >> return Stop

-- | Unfold the input @a@ using @Unfold m a b@, run an action on @a@ whenever
-- the unfold stops normally, or if it is garbage collected after a partial
-- lazy evaluation.
--
-- The semantics of the action @a -> m c@ are similar to the cleanup action
-- semantics in 'bracket'.
--
-- /See also 'after_'/
--
-- /Pre-release/
{-# INLINE_NORMAL after #-}
after :: MonadRunInIO m
    => (a -> m c) -> Unfold m a b -> Unfold m a b
after action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        ref <- newIOFinalizer (action x)
        return (s, ref)

    {-# INLINE_LATE step #-}
    step (st, ref) = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x (s, ref)
            Skip s    -> return $ Skip (s, ref)
            Stop      -> do
                runIOFinalizer ref
                return Stop

{-# INLINE_NORMAL _onException #-}
_onException :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
_onException action =
    gbracket_ return MC.try
        (\_ -> return ())
        (nilM (\(a, e :: MC.SomeException) -> action a >> MC.throwM e))

-- | Unfold the input @a@ using @Unfold m a b@, run the action @a -> m c@ on
-- @a@ if the unfold aborts due to an exception.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
onException action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` action v
        return $ case res of
            Yield x s -> Yield x (s, v)
            Skip s    -> Skip (s, v)
            Stop      -> Stop

{-# INLINE_NORMAL _finally #-}
_finally :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
_finally action =
    gbracket_ return MC.try action
        (nilM (\(a, e :: MC.SomeException) -> action a >> MC.throwM e))

-- | Like 'finally' with following differences:
--
-- * action @a -> m c@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * does not require a 'MonadAsync' constraint.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL finally_ #-}
finally_ :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
finally_ action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` action v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> action v >> return Stop

-- | Unfold the input @a@ using @Unfold m a b@, run an action on @a@ whenever
-- the unfold stops normally, aborts due to an exception or if it is garbage
-- collected after a partial lazy evaluation.
--
-- The semantics of the action @a -> m c@ are similar to the cleanup action
-- semantics in 'bracket'.
--
-- @
-- finally release = bracket return release
-- @
--
-- /See also 'finally_'/
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL finally #-}
finally :: (MonadAsync m, MonadCatch m)
    => (a -> m c) -> Unfold m a b -> Unfold m a b
finally action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        ref <- newIOFinalizer (action x)
        return (s, ref)

    {-# INLINE_LATE step #-}
    step (st, ref) = do
        res <- step1 st `MC.onException` runIOFinalizer ref
        case res of
            Yield x s -> return $ Yield x (s, ref)
            Skip s    -> return $ Skip (s, ref)
            Stop      -> do
                runIOFinalizer ref
                return Stop

{-# INLINE_NORMAL _bracket #-}
_bracket :: MonadCatch m
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
_bracket bef aft =
    gbracket_ bef MC.try aft (nilM (\(a, e :: MC.SomeException) -> aft a >>
    MC.throwM e))

-- | Like 'bracket' but with following differences:
--
-- * alloc action @a -> m c@ runs with async exceptions enabled
-- * cleanup action @c -> m d@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * does not require a 'MonadAsync' constraint.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL bracket_ #-}
bracket_ :: MonadCatch m
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
bracket_ bef aft (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        r <- bef x
        s <- inject1 r
        return (s, r)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` aft v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> aft v >> return Stop

-- | Run the alloc action @a -> m c@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @c@ as input to @Unfold m c b@ to generate an output stream.
--
-- @c@ is usually a resource under the state of monad @m@, e.g. a file
-- handle, that requires a cleanup after use. The cleanup action @c -> m d@,
-- runs whenever the stream ends normally, due to a sync or async exception or
-- if it gets garbage collected after a partial lazy evaluation.
--
-- 'bracket' only guarantees that the cleanup action runs, and it runs with
-- async exceptions enabled. The action must ensure that it can successfully
-- cleanup the resource in the face of sync or async exceptions.
--
-- When the stream ends normally or on a sync exception, cleanup action runs
-- immediately in the current thread context, whereas in other cases it runs in
-- the GC context, therefore, cleanup may be delayed until the GC gets to run.
--
-- /See also: 'bracket_', 'gbracket'/
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL bracket #-}
bracket :: (MonadAsync m, MonadCatch m)
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
bracket bef aft (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        -- Mask asynchronous exceptions to make the execution of 'bef' and
        -- the registration of 'aft' atomic. See comment in 'D.gbracketIO'.
        (r, ref) <- withRunInIO $ \run -> mask_ $ run $ do
            r <- bef x
            ref <- newIOFinalizer (aft r)
            return (r, ref)
        s <- inject1 r
        return (s, ref)

    {-# INLINE_LATE step #-}
    step (st, ref) = do
        res <- step1 st `MC.onException` runIOFinalizer ref
        case res of
            Yield x s -> return $ Yield x (s, ref)
            Skip s    -> return $ Skip (s, ref)
            Stop      -> do
                runIOFinalizer ref
                return Stop

-- | When unfolding @Unfold m a b@ if an exception @e@ occurs, unfold @e@ using
-- @Unfold m e b@.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => Unfold m e b -> Unfold m a b -> Unfold m a b
handle exc =
    gbracket_ return MC.try (\_ -> return ()) (discardFirst exc)
