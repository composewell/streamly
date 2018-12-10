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
-- Module      : Streamly.Streams.StreamKType
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
module Streamly.Streams.StreamKType
    (
    -- * A class for streams
      IsStream (..)
    , adapt

    -- * The stream type
    , Stream ()
    , Streaming

    -- * Construction
    , consMSerial
    , mkStream

    -- * Elimination
    , unStream
    , unStreamShared
    , foldStream
    , runStreamSVar

    , nil
    , serial
    , map
    , yieldM
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Semigroup (Semigroup(..))
import Prelude hiding (map)

import Streamly.SVar

------------------------------------------------------------------------------
-- The basic stream type
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
newtype Stream m a =
    MkStream (forall r.
               State Stream m a          -- state
            -> m r                       -- stop
            -> (a -> m r)                -- singleton
            -> (a -> Stream m a -> m r)  -- yield
            -> m r
            )

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

infixr 5 `consM`
infixr 5 |:

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

-- | Adapt any specific stream type to any other specific stream type.
--
-- @since 0.1.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

{-# INLINE consMSerial #-}
consMSerial :: (Monad m) => m a -> Stream m a -> Stream m a
consMSerial m r = MkStream $ \_ _ _ yld -> m >>= \a -> yld a r

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
-- Building a stream
------------------------------------------------------------------------------

-- | Build a stream from an 'SVar', a stop continuation, a singleton stream
-- continuation and a yield continuation.
mkStream:: IsStream t
    => (forall r. State Stream m a
        -> m r
        -> (a -> m r)
        -> (a -> t m a -> m r)
        -> m r)
    -> t m a
mkStream k = fromStream $ MkStream $ \st stp sng yld ->
    let yieldk a r = yld a (toStream r)
     in k st stp sng yieldk

-- | An empty stream.
--
-- @
-- > toList nil
-- []
-- @
--
-- @since 0.1.0
nil :: IsStream t => t m a
nil = fromStream $ mkStream $ \_ stp _ _ -> stp

------------------------------------------------------------------------------
-- Folding a stream
------------------------------------------------------------------------------

-- Run a stream in detached mode i.e. not joining an SVar
{-# INLINE unStream #-}
unStream ::
       Stream m a
    -> State Stream m b          -- state
    -> m r                       -- stop
    -> (a -> m r)                -- singleton
    -> (a -> Stream m a -> m r)  -- yield
    -> m r
unStream (MkStream runner) st = runner (adaptState st)

-- | Like unstream, but passes a shared SVar across continuations.
{-# INLINE unStreamShared #-}
unStreamShared ::
       Stream m a
    -> State Stream m a          -- state
    -> m r                       -- stop
    -> (a -> m r)                -- singleton
    -> (a -> Stream m a -> m r)  -- yield
    -> m r
unStreamShared (MkStream runner) st stp sng yld = runner st stp sng yld

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation.
foldStream
    :: IsStream t
    => State Stream m a
    -> m r
    -> (a -> m r)
    -> (a -> t m a -> m r)
    -> t m a
    -> m r
foldStream st blank single step m =
    let yieldk a x = step a (fromStream x)
     in unStreamShared (toStream m) st blank single yieldk

-- Run the stream using a run function associated with the SVar that runs the
-- streams with a captured snapshot of the monadic state.
{-# INLINE runStreamSVar #-}
runStreamSVar
    :: MonadIO m
    => SVar Stream m a
    -> Stream m a
    -> State Stream m a          -- state
    -> m r                       -- stop
    -> (a -> m r)                -- singleton
    -> (a -> Stream m a -> m r)  -- yield
    -> m ()
runStreamSVar sv m st stp sng yld =
    let mrun = runInIO $ svarMrun sv
    in void $ liftIO $ mrun $ unStreamShared m st stp sng yld

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
{-# INLINE serial #-}
serial :: Stream m a -> Stream m a -> Stream m a
serial m1 m2 = go m1
    where
    go m = mkStream $ \st stp sng yld ->
               let stop       = unStream m2 st stp sng yld
                   single a   = yld a m2
                   yieldk a r = yld a (go r)
               in unStream m st stop single yieldk

instance Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (Stream m a) where
    mempty = nil
    mappend = (<>)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------


{-# INLINE map #-}
map :: (IsStream t, Monad m) => (a -> b) -> t m a -> t m b
map f m = fromStream $ mkStream $ \st stp sng yld ->
    let single     = sng . f
        yieldk a r = yld (f a) (fmap f r)
    in unStream (toStream m) st stp single yieldk

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
