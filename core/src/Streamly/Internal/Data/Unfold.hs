{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Unfold
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Unfold Type
      Step(..)
    , Unfold

    -- * Unfolds
    -- One to one correspondence with
    -- "Streamly.Internal.Data.Stream.StreamD.Generate"
    -- ** Basic Constructors
    , mkUnfoldM
    , mkUnfoldrM
    , unfoldrM
    , unfoldr
    , functionM
    , function
    , identity
    , nilM
    , nil
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
    , module Streamly.Internal.Data.Unfold.Enumeration

    -- ** From Containers
    , fromList
    , fromListM

    -- ** From Memory
    , fromPtr

    -- ** From Stream
    , fromStreamK
    , fromStreamD
    , fromStream

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

    -- XXX Add "WithInput" versions of all these, map2, postscan2, takeWhile2,
    -- filter2 etc.  Alternatively, we can use the default operations with
    -- input, but that might make the common case more inconvenient.

    -- ** Mapping on Output
    , map
    , map2
    , mapM
    , mapM2

    , postscanlM'
    , postscan
    , scan
    , scanMany
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
    , joinInnerGeneric
    , crossApply

    -- ** Nesting
    , ConcatState (..)
    , many
    , many2
    , concatMapM
    , bind

    -- ** Resource Management
    -- | 'bracket' is the most general resource management operation, all other
    -- operations can be expressed using it. These functions have IO suffix
    -- because the allocation and cleanup functions are IO actions. For
    -- generalized allocation and cleanup functions see the functions without
    -- the IO suffix in the "streamly" package.
    , gbracket_
    , gbracketIO
    , before
    , afterIO
    , after_
    , finallyIO
    , finally_
    , bracketIO
    , bracket_

    -- ** Exceptions
    -- | Most of these combinators inhibit stream fusion, therefore, when
    -- possible, they should be called in an outer loop to mitigate the cost.
    -- For example, instead of calling them on a stream of chars call them on a
    -- stream of arrays before flattening it to a stream of chars.
    , onException
    , handle
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Exception (Exception, mask_)
import Control.Monad.Catch (MonadCatch)
import Data.Functor (($>))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.IOFinalizer
    (newIOFinalizer, runIOFinalizer, clearingIOFinalizer)
import Streamly.Internal.Data.Stream.StreamD.Type (Stream(..), Step(..))
import Streamly.Internal.Data.SVar.Type (defState)

import qualified Control.Monad.Catch as MC
import qualified Data.Tuple as Tuple
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Prelude

import Streamly.Internal.Data.Unfold.Enumeration
import Streamly.Internal.Data.Unfold.Type
import Prelude
       hiding (map, mapM, takeWhile, take, filter, const, zipWith
              , drop, dropWhile, either)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Foreign (Storable, peek, sizeOf)
import Foreign.Ptr

#include "DocTestDataUnfold.hs"

-------------------------------------------------------------------------------
-- Input operations
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

-- | Choose left or right unfold based on an either input.
--
-- /Pre-release/
{-# INLINE_NORMAL either #-}
either :: Applicative m =>
    Unfold m a c -> Unfold m b c -> Unfold m (Either a b) c
either (Unfold stepL injectL) (Unfold stepR injectR) = Unfold step inject

    where

    inject (Left x) = Left <$> injectL x
    inject (Right x) = Right <$> injectR x

    {-# INLINE_LATE step #-}
    step (Left st) = do
        (\case
            Yield x s -> Yield x (Left s)
            Skip s -> Skip (Left s)
            Stop -> Stop) <$> stepL st
    step (Right st) = do
        (\case
            Yield x s -> Yield x (Right s)
            Skip s -> Skip (Right s)
            Stop -> Stop) <$> stepR st

-- postscan2 :: Monad m => Refold m a b c -> Unfold m a b -> Unfold m a c

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

data ScanState s f = ScanInit s | ScanDo s !f | ScanDone

{-# INLINE_NORMAL scanWith #-}
scanWith :: Monad m => Bool -> Fold m b c -> Unfold m a b -> Unfold m a c
scanWith restart (Fold fstep initial extract) (Unfold stepU injectU) =
    Unfold step inject

    where

    inject a = ScanInit <$> injectU a

    {-# INLINE runStep #-}
    runStep us action = do
        r <- action
        case r of
            FL.Partial fs -> do
                !b <- extract fs
                return $ Yield b (ScanDo us fs)
            FL.Done b ->
                let next = if restart then ScanInit us else ScanDone
                 in return $ Yield b next

    {-# INLINE_LATE step #-}
    step (ScanInit us) = runStep us initial
    step (ScanDo us fs) = do
        res <- stepU us
        case res of
            Yield x s -> runStep s (fstep fs x)
            Skip s -> return $ Skip $ ScanDo s fs
            Stop -> return Stop
    step ScanDone = return Stop

-- | Scan the output of an 'Unfold' to change it in a stateful manner.
-- Once fold is done it will restart from its initial state.
--
-- >>> u = Unfold.scanMany (Fold.take 2 Fold.sum) Unfold.fromList
-- >>> Unfold.fold Fold.toList u [1,2,3,4,5]
-- [0,1,3,0,3,7,0,5]
--
-- /Pre-release/
{-# INLINE_NORMAL scanMany #-}
scanMany :: Monad m => Fold m b c -> Unfold m a b -> Unfold m a c
scanMany = scanWith True

-- scan2 :: Monad m => Refold m a b c -> Unfold m a b -> Unfold m a c

-- | Scan the output of an 'Unfold' to change it in a stateful manner.
-- Once fold is done it will stop.
--
-- >>> u = Unfold.scan (Fold.take 2 Fold.sum) Unfold.fromList
-- >>> Unfold.fold Fold.toList u [1,2,3,4,5]
-- [0,1,3]
--
-- /Pre-release/
{-# INLINE_NORMAL scan #-}
scan :: Monad m => Fold m b c -> Unfold m a b -> Unfold m a c
scan = scanWith False

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
fromStreamK :: Applicative m => Unfold m (K.StreamK m a) a
fromStreamK = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step stream = do
        (\case
            Just (x, xs) -> Yield x xs
            Nothing -> Stop) <$> K.uncons stream

{-# INLINE fromStream #-}
fromStream :: Applicative m => Unfold m (Stream m a) a
fromStream = fromStreamD

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

-- | An empty stream.
{-# INLINE nil #-}
nil :: Applicative m => Unfold m a b
nil = Unfold (Prelude.const (pure Stop)) pure

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

-- | Convert a list of monadic values to a 'Stream'
--
{-# INLINE_LATE fromListM #-}
fromListM :: Applicative m => Unfold m [m a] a
fromListM = Unfold step pure

    where

    {-# INLINE_LATE step #-}
    step (x:xs) = (`Yield` xs) <$> x
    step [] = pure Stop

{-# INLINE fromPtr #-}
fromPtr :: forall m a. (MonadIO m, Storable a) => Unfold m (Ptr a) a
fromPtr = Unfold step return

    where

    {-# INLINE_LATE step #-}
    step p = do
        x <- liftIO $ peek p
        return $ Yield x (PTR_NEXT(p, a))

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

-- | Given a seed @(n, action)@, generates a stream replicating the @action@ @n@
-- times.
--
{-# INLINE replicateM #-}
replicateM :: Applicative m => Unfold m (Int, m a) a
replicateM = Unfold step inject

    where

    inject seed = pure seed

    {-# INLINE_LATE step #-}
    step (i, action) =
        if i <= 0
        then pure Stop
        else (\x -> Yield x (i - 1, action)) <$> action

-- | Generates an infinite stream repeating the seed.
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
{-# INLINE filter #-}
filter :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
filter f = filterM (return . f)

-- | @drop n unf@ drops @n@ elements from the stream generated by @unf@.
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
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
dropWhile f = dropWhileM (return . f)

{-# INLINE_NORMAL joinInnerGeneric #-}
joinInnerGeneric :: Monad m =>
    (b -> c -> Bool) -> Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
joinInnerGeneric eq s1 s2 = filter (\(a, b) -> a `eq` b) $ cross s1 s2

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Like 'gbracketIO' but with following differences:
--
-- * alloc action @a -> m c@ runs with async exceptions enabled
-- * cleanup action @c -> m d@ won't run if the stream is garbage collected
--   after partial evaluation.
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
{-# INLINE_NORMAL gbracketIO #-}
gbracketIO
    :: MonadIO m
    => (a -> IO c)                           -- ^ before
    -> (c -> IO d)                           -- ^ after, on normal stop, or GC
    -> (c -> IO ())                          -- ^ action on exception
    -> Unfold m e b                          -- ^ stream on exception
    -> (forall s. m s -> IO (Either e s))    -- ^ try (exception handling)
    -> Unfold m c b                         -- ^ unfold to run
    -> Unfold m a b
gbracketIO bef aft onExc (Unfold estep einject) ftry (Unfold step1 inject1) =
    Unfold step inject

    where

    inject x = do
        -- Mask asynchronous exceptions to make the execution of 'bef' and
        -- the registration of 'aft' atomic. See comment in 'D.gbracketIO'.
        (r, ref) <- liftIO $ mask_ $ do
            r <- bef x
            ref <- newIOFinalizer (aft r)
            return (r, ref)
        s <- inject1 r
        return $ Right (s, r, ref)

    {-# INLINE_LATE step #-}
    step (Right (st, v, ref)) = do
        res <- liftIO $ ftry $ step1 st
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
                liftIO $ clearingIOFinalizer ref (onExc v)
                r <- einject e
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
{-# INLINE_NORMAL afterIO #-}
afterIO :: MonadIO m
    => (a -> IO c) -> Unfold m a b -> Unfold m a b
afterIO action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        ref <- liftIO $ newIOFinalizer (action x)
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

-- | Like 'finallyIO' with following differences:
--
-- * action @a -> m c@ won't run if the stream is garbage collected
--   after partial evaluation.
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
{-# INLINE_NORMAL finallyIO #-}
finallyIO :: (MonadIO m, MonadCatch m)
    => (a -> IO c) -> Unfold m a b -> Unfold m a b
finallyIO action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        ref <- liftIO $ newIOFinalizer (action x)
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

-- | Like 'bracketIO' but with following differences:
--
-- * alloc action @a -> m c@ runs with async exceptions enabled
-- * cleanup action @c -> m d@ won't run if the stream is garbage collected
--   after partial evaluation.
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
{-# INLINE_NORMAL bracketIO #-}
bracketIO :: (MonadIO m, MonadCatch m)
    => (a -> IO c) -> (c -> IO d) -> Unfold m c b -> Unfold m a b
bracketIO bef aft (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        -- Mask asynchronous exceptions to make the execution of 'bef' and
        -- the registration of 'aft' atomic. See comment in 'D.gbracketIO'.
        (r, ref) <- liftIO $ mask_ $ do
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
