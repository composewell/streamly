{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.StreamK
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.StreamK
    (
    -- * Streams
      IsStream (..)
    , adapt
    , Streaming         -- deprecated
    , Stream (..)
    , run

    -- * Construction
    , nil
    , cons
    , (.:)
    , consMSerial

    -- * Generation
    , singleton
    , once
    , repeat

    -- * Semigroup Style Composition
    , serial

    -- * Utilities
    , bindWith
    , withLocal
    )
where

import Control.Monad.Reader.Class  (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Semigroup (Semigroup(..))
import Prelude hiding (repeat)

import Streamly.SVar

------------------------------------------------------------------------------
-- The basic stream type
------------------------------------------------------------------------------

-- | The type 'Stream m a' represents a monadic stream of values of type 'a'
-- constructed using actions in monad 'm'. It uses stop, singleton and yield
-- continuations equivalent to the following direct style type:
--
-- data Stream m a = Stop | Singleton a | Yield a (Stream m a)
--
-- To facilitate parallel composition we maintain a local state in an SVar that
-- is shared across and is used for synchronization of the streams being
-- composed.
--
-- The singleton case can be expressed in terms of stop and yield but we have
-- it as a separate case to optimize composition operations for streams with
-- single element.  We build singleton streams in the implementation of 'pure'
-- for Applicative and Monad, and in 'lift' for MonadTrans.
--
newtype Stream m a =
    Stream {
        runStream :: forall r.
               Maybe (SVar Stream m a)   -- local state
            -> m r                       -- stop
            -> (a -> m r)                -- singleton
            -> (a -> Stream m a -> m r)  -- yield
            -> m r
    }

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

------------------------------------------------------------------------------
-- Construction
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
nil = fromStream $ Stream $ \_ stp _ _ -> stp

infixr 5 `cons`

-- faster than consM because there is no bind.
-- | Construct a stream by adding a pure value at the head of an existing
-- stream. For pure values it can be faster than 'consM'. For example:
--
-- @
-- > toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- @since 0.1.0
cons :: IsStream t => a -> t m a -> t m a
cons a r = fromStream $ Stream $ \_ _ _ yld -> yld a (toStream r)

infixr 5 .:

-- | Operator equivalent of 'cons'.
--
-- @
-- > toList $ 1 .: 2 .: 3 .: nil
-- [1,2,3]
-- @
--
-- @since 0.1.1
(.:) :: IsStream t => a -> t m a -> t m a
(.:) = cons

-- | Constructs a stream by adding a monadic action at the head of an existing
-- stream. For example:
--
-- @
-- > toList $ getLine \`consM` getLine \`consM` nil
-- hello
-- world
-- ["hello","world"]
-- @
--
-- @since 0.2.0
{-# INLINE consMSerial #-}
consMSerial :: (Monad m) => m a -> Stream m a -> Stream m a
consMSerial m r = Stream $ \_ _ _ yld -> m >>= \a -> yld a r

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

run :: (Monad m, IsStream t) => t m a -> m ()
run m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yield _ r = go (toStream r)
         in (runStream m1) Nothing stop single yield

-------------------------------------------------------------------------------
-- Special generation
-------------------------------------------------------------------------------

-- | Same as @once . return@ but may be faster because there is no bind
singleton :: IsStream t => a -> t m a
singleton a = fromStream $ Stream $ \_ _ single _ -> single a

-- | Create a singleton stream by executing a monadic action once. Same as
-- @m \`consM` nil@ but more efficient.
--
-- @
-- > toList $ once getLine
-- hello
-- ["hello"]
-- @
--
-- @since 0.2.0
{-# INLINE once #-}
once :: (Monad m, IsStream t) => m a -> t m a
once m = fromStream $ Stream $ \_ _ single _ -> m >>= single

repeat :: IsStream t => a -> t m a
repeat a = let x = cons a x in x

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
{-# INLINE serial #-}
serial :: Stream m a -> Stream m a -> Stream m a
serial m1 m2 = go m1
    where
    go (Stream m) = Stream $ \_ stp sng yld ->
            let stop      = (runStream m2) Nothing stp sng yld
                single a  = yld a m2
                yield a r = yld a (go r)
            in m Nothing stop single yield

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

instance Monad m => Functor (Stream m) where
    fmap f m = Stream $ \_ stp sng yld ->
        let single    = sng . f
            yield a r = yld (f a) (fmap f r)
        in (runStream m) Nothing stp single yield

-------------------------------------------------------------------------------
-- Bind utility
-------------------------------------------------------------------------------

{-# INLINE bindWith #-}
bindWith
    :: (forall c. Stream m c -> Stream m c -> Stream m c)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
bindWith par m f = go m
    where
        go (Stream g) =
            Stream $ \ctx stp sng yld ->
            let runIt x = (runStream x) ctx stp sng yld
                single a  = runIt $ f a
                yield a r = runIt $ f a `par` go r
            in g Nothing stp single yield

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = Stream $ \_ stp sng yld ->
    let stop  = runStream m2 Nothing stp sng yld
    in runStream m1 Nothing stop sng yld

------------------------------------------------------------------------------
-- MonadReader
------------------------------------------------------------------------------

withLocal :: MonadReader r m => (r -> r) -> Stream m a -> Stream m a
withLocal f m =
    Stream $ \_ stp sng yld ->
        let single = local f . sng
            yield a r = local f $ yld a (withLocal f r)
        in (runStream m) Nothing (local f stp) single yield

------------------------------------------------------------------------------
-- MonadError
------------------------------------------------------------------------------

{-
-- XXX handle and test cross thread state transfer
withCatchError
    :: MonadError e m
    => Stream m a -> (e -> Stream m a) -> Stream m a
withCatchError m h =
    Stream $ \_ stp sng yld ->
        let run x = runStream x Nothing stp sng yield
            handle r = r `catchError` \e -> run $ h e
            yield a r = yld a (withCatchError r h)
        in handle $ run m
-}

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    lift = once
