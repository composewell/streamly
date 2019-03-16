{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Streamly.Internal.MonadLazy
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.MonadLazy
    (
      MonadLazy (..)
    , MyIO
    , myIOtoIO
    , runMyIO
    )
where

import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative (liftA2)
import GHC.IO (IO(..), unIO, failIO)
import GHC.Prim
import Data.Functor.Identity (Identity)
import Control.Monad.StrictIdentity -- (StrictIdentity)

-------------------------------------------------------------------------------
-- An IO implementation for performance experimentation
-------------------------------------------------------------------------------

newtype MyIO a = MyIO (State# RealWorld -> (# State# RealWorld, a #))

{-# INLINE runMyIO #-}
runMyIO (MyIO m) = case m realWorld# of (# _, res #) -> res

{-# INLINE returnIO #-}
returnIO :: a -> MyIO a
returnIO x = MyIO (\ s -> (# s, x #))

{-
{-# INLINE bindIO #-}
bindIO :: MyIO a -> (a -> MyIO b) -> MyIO b
bindIO (MyIO m) k = MyIO (\ s -> case m s of (# new_s, a #) -> myUnIO (k a) new_s)

{-# INLINE thenIO #-}
thenIO :: MyIO a -> MyIO b -> MyIO b
thenIO (MyIO m) k = MyIO (\ s -> case m s of (# new_s, _ #) -> myUnIO k new_s)
-}

{-# INLINE myUnIO #-}
myUnIO :: MyIO a -> (State# RealWorld -> (# State# RealWorld, a #))
myUnIO (MyIO a) = a

{-# INLINE myIOtoIO #-}
myIOtoIO :: MyIO a -> IO a
myIOtoIO (MyIO a) = IO a

-- | @since 2.01
instance  Functor MyIO where
   {-# INLINE fmap #-}
   fmap f x = x >>= (pure . f)

-- | @since 2.01
instance Applicative MyIO where
    {-# INLINE pure #-}
    pure  = returnIO

    -- {-# INLINE (*>) #-}
    --   (*>)  = thenIO

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE liftA2 #-}
    liftA2 = liftM2

-- | @since 2.01
instance  Monad MyIO  where
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    (>>)      = (*>)
--    (>>=)     = bindIO
    (>>=) (MyIO m) k = MyIO ( \ s ->
            let r = case m s of (# _, res #) -> res
            in myUnIO (k r) s)
    -- fail s    = failIO s

instance MonadIO MyIO where
    -- liftIO (IO a) = MyIO a

instance MonadThrow MyIO where
instance MonadBase IO MyIO where
instance MonadBaseControl IO MyIO where

instance MonadLazy MyIO where
    {-# INLINE lazyBind #-}
    lazyBind = (>>=)

-------------------------------------------------------------------------------
-- Lazy bind for strict monads
-------------------------------------------------------------------------------

-- Strict Monads (read IO) can make the foldr run to completion due to the
-- strictness. We need a lazy bind to keep the composition lazy enough. When
-- implementing a left fold we need to make lazy values strict. That is easy to
-- do using explicit evaluation or strictness notations. In contrast, here in
-- foldr we need to make strict functions lazy (e.g. the IO bind) and there
-- seem to be no good way of doing that.
--
-- Note that this is only need to keep the Applicative composition of folds in
-- IO lazy and not to keep the IO itself lazy.
-- XXX Need to investigate if this can introduce any kind of badness/insanity?
--
class Monad m => MonadLazy m where
    lazyBind :: forall a b. m a -> (a -> m b) -> m b

instance MonadLazy IO where
    {-# INLINE lazyBind #-}
    lazyBind (IO m) k = IO ( \ s ->
            let r = case m s of (# _, res #) -> res
            in unIO (k r) s)

-- For lazy monads bind is lazy bind
instance MonadLazy Identity where
    {-# INLINE lazyBind #-}
    lazyBind = (>>=)

-------------------------------------------------------------------------------
-- For performance comparison to see how much strictness matters in Identity
-------------------------------------------------------------------------------

instance MonadLazy StrictIdentity where
    {-# INLINE lazyBind #-}
    lazyBind m k  = k (runStrictIdentity m)
