{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.StreamK.Type
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- Continuation passing style (CPS) stream implementation. The symbol 'K' below
-- denotes a function as well as a Kontinuation.
--
module Streamly.Streams.StreamK.Type
    (
    -- * A class for streams
      IsStream (..)
    , adapt

    -- * The stream type
    , Stream ()

    -- * Construction
    , mkStream
    , fromStopK
    , fromYieldK
    , consK

    -- * Elimination
    , foldStream
    , foldStreamShared
    , foldStreamSVar

    -- instances
    , consMSerial

    , nil
    , serial
    , map
    , yieldM

    , Streaming   -- deprecated
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Semigroup (Semigroup(..))
import Prelude hiding (map)

import Streamly.SVar

------------------------------------------------------------------------------
-- Basic stream type
------------------------------------------------------------------------------

-- | The type @Stream m a@ represents a monadic stream of values of type 'a'
-- constructed using actions in monad 'm'. It uses stop, singleton and yield
-- continuations equivalent to the following direct style type:
--
-- @
-- data Stream m a = Stop | Singleton a | Yield a (Stream m a)
-- @
--
-- To facilitate parallel composition we maintain a local state in an 'SVar'
-- that is shared across and is used for synchronization of the streams being
-- composed.
--
-- The singleton case can be expressed in terms of stop and yield but we have
-- it as a separate case to optimize composition operations for streams with
-- single element.  We build singleton streams in the implementation of 'pure'
-- for Applicative and Monad, and in 'lift' for MonadTrans.
--
-- XXX remove the Stream type parameter from State as it is always constant.
-- We can remove it from SVar as well
--
newtype Stream m a =
    MkStream (forall r.
               State Stream m a         -- state
            -> (a -> Stream m a -> m r) -- yield
            -> (a -> m r)               -- singleton
            -> m r                      -- stop
            -> m r
            )

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

infixr 5 `consM`
infixr 5 |:

-- XXX Use a different SVar based on the stream type. But we need to make sure
-- that we do not lose performance due to polymorphism.
--
-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
--
-- @since 0.2.0
class IsStream t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a
    -- | Constructs a stream by adding a monadic action at the head of an
    -- existing stream. For example:
    --
    -- @
    -- > toList $ getLine \`consM` getLine \`consM` nil
    -- hello
    -- world
    -- ["hello","world"]
    -- @
    --
    -- /Concurrent (do not use 'parallely' to construct infinite streams)/
    --
    -- @since 0.2.0
    consM :: MonadAsync m => m a -> t m a -> t m a
    -- | Operator equivalent of 'consM'. We can read it as "@parallel colon@"
    -- to remember that @|@ comes before ':'.
    --
    -- @
    -- > toList $ getLine |: getLine |: nil
    -- hello
    -- world
    -- ["hello","world"]
    -- @
    --
    -- @
    -- let delay = threadDelay 1000000 >> print 1
    -- runStream $ serially  $ delay |: delay |: delay |: nil
    -- runStream $ parallely $ delay |: delay |: delay |: nil
    -- @
    --
    -- /Concurrent (do not use 'parallely' to construct infinite streams)/
    --
    -- @since 0.2.0
    (|:) :: MonadAsync m => m a -> t m a -> t m a
    -- We can define (|:) just as 'consM' but it is defined explicitly for each
    -- type because we want to use SPECIALIZE pragma on the definition.

-- | Same as 'IsStream'.
--
-- @since 0.1.0
{-# DEPRECATED Streaming "Please use IsStream instead." #-}
type Streaming = IsStream

-------------------------------------------------------------------------------
-- Type adapting combinators
-------------------------------------------------------------------------------

-- XXX Move/reset the State here by reconstructing the stream with cleared
-- state. Can we make sure we do not do that when t1 = t2? If we do this then
-- we do not need to do that explicitly using svarStyle.  It would act as
-- unShare when the stream type is the same.
--
-- | Adapt any specific stream type to any other specific stream type.
--
-- @since 0.1.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

------------------------------------------------------------------------------
-- Building a stream
------------------------------------------------------------------------------

-- XXX The State is always parameterized by "Stream" which means State is not
-- different for different stream types. So we have to manually make sure that
-- when converting from one stream to another we migrate the state correctly.
-- This can be fixed if we use a different SVar type for different streams.
-- Currently we always use "SVar Stream" and therefore a different State type
-- parameterized by that stream.
--
-- | Build a stream from an 'SVar', a stop continuation, a singleton stream
-- continuation and a yield continuation.
mkStream:: IsStream t
    => (forall r. State Stream m a
        -> (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
mkStream k = fromStream $ MkStream $ \st yld sng stp ->
    let yieldk a r = yld a (toStream r)
     in k st yieldk sng stp

-- | A terminal function that has no continuation to follow.
type StopK m = forall r. m r -> m r

-- | A monadic continuation, it is a function that yields a value of type "a"
-- and calls the argument (a -> m r) as a continuation with that value. We can
-- also think of it as a callback with a handler (a -> m r).  Category
-- theorists call it a codensity type, a special type of right kan extension.
type YieldK m a = forall r. (a -> m r) -> m r

_wrapM :: Monad m => m a -> YieldK m a
_wrapM m = \k -> m >>= k

-- | Make an empty stream from a stop function.
fromStopK :: IsStream t => StopK m -> t m a
fromStopK k = mkStream $ \_ _ _ stp -> k stp

-- | Make a singleton stream from a yield function.
fromYieldK :: IsStream t => YieldK m a -> t m a
fromYieldK k = mkStream $ \_ _ sng _ -> k sng

-- | Add a yield function at the head of the stream.
consK :: IsStream t => YieldK m a -> t m a -> t m a
consK k r = mkStream $ \_ yld _ _ -> k (\x -> yld x r)

-- XXX Build a stream from a repeating callback function.

------------------------------------------------------------------------------
-- Folding a stream
------------------------------------------------------------------------------

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation. The stream would share the current
-- SVar passed via the State.
{-# INLINE foldStreamShared #-}
foldStreamShared
    :: IsStream t
    => State Stream m a
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStreamShared st yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        MkStream k = toStream m
     in k st yieldk sng stp

-- | Fold a stream by providing a State, stop continuation, a singleton
-- continuation and a yield continuation. The stream will not use the SVar
-- passed via State.
{-# INLINE foldStream #-}
foldStream
    :: IsStream t
    => State Stream m a
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
foldStream st yld sng stp m =
    let yieldk a x = yld a (fromStream x)
        MkStream k = toStream m
     in k (adaptState st) yieldk sng stp

-- Run the stream using a run function associated with the SVar that runs the
-- streams with a captured snapshot of the monadic state.
{-# INLINE foldStreamSVar #-}
foldStreamSVar
    :: (IsStream t, MonadIO m)
    => SVar Stream m a
    -> State Stream m a          -- state
    -> (a -> t m a -> m r)       -- yield
    -> (a -> m r)                -- singleton
    -> m r                       -- stop
    -> t m a
    -> m ()
foldStreamSVar sv st yld sng stp m =
    let mrun = runInIO $ svarMrun sv
    in void $ liftIO $ mrun $ foldStreamShared st yld sng stp m

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

{-# INLINE consMSerial #-}
consMSerial :: (IsStream t, Monad m) => m a -> t m a -> t m a
consMSerial m r = mkStream $ \_ yld _ _ -> m >>= \a -> yld a r

-------------------------------------------------------------------------------
-- IsStream Stream
-------------------------------------------------------------------------------

instance IsStream Stream where
    toStream = id
    fromStream = id

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
    consM :: Monad m => m a -> Stream m a -> Stream m a
    consM = consMSerial

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> Stream IO a -> Stream IO a #-}
    (|:) :: Monad m => m a -> Stream m a -> Stream m a
    (|:) = consMSerial

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'SerialT'.
-- Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- @since 0.2.0
{-# INLINE serial #-}
serial :: IsStream t => t m a -> t m a -> t m a
serial m1 m2 = go m1
    where
    go m = mkStream $ \st yld sng stp ->
               let stop       = foldStream st yld sng stp m2
                   single a   = yld a m2
                   yieldk a r = yld a (go r)
               in foldStream st yieldk single stop m

instance Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

-- | An empty stream.
--
-- @
-- > toList nil
-- []
-- @
--
-- @since 0.1.0
nil :: IsStream t => t m a
nil = mkStream $ \_ _ _ stp -> stp

instance Monoid (Stream m a) where
    mempty = nil
    mappend = (<>)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------


{-# INLINE map #-}
map :: (IsStream t, Monad m) => (a -> b) -> t m a -> t m b
map f m = mkStream $ \st yld sng stp ->
    let single     = sng . f
        yieldk a r = yld (f a) (map f r)
    in foldStream (adaptState st) yieldk single stp m

instance Monad m => Functor (Stream m) where
    fmap = map

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

{-# INLINE yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM m = fromStream $ mkStream $ \_ _ single _ -> m >>= single

instance MonadTrans Stream where
    lift = yieldM
