{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE FlexibleContexts                   #-}

-- |
-- Module      : Streamly.Fold.Types
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Fold.Types
    ( Fold (..)
    , lmap
    , lmapM
    , lfilter
    , lfilterM
    , lcatMaybes
    , ltake
    , ltakeWhile
    , lsessionsOf
    , lchunksOf
    , duplicate
    )
where

import Control.Applicative (liftA2)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (SomeException(..), catch, mask)
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (control)
import Data.Maybe (isJust, fromJust)
import Data.Semigroup (Semigroup(..))
import Streamly.Strict (Tuple'(..), Tuple3'(..), Either'(..))
import Streamly.SVar (MonadAsync)

------------------------------------------------------------------------------
-- Monadic left folds
------------------------------------------------------------------------------

-- | Represents a left fold over an input stream of values of type @a@ to a
-- single value of type @b@ in 'Monad' @m@.
--
-- @since 0.7.0

-- The fold uses an intermediate type @x@ as accumulator. The fold accumulator
-- is initialized by calling the @init@ function and is then driven by calling
-- the step function repeatedly. When the fold is done the @extract@ function
-- is used to map the intermediate type @x@ to the final type @b@. This allows
-- the state of the fold to be embedded in an arbitrary type @x@.
data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall x. Fold (x -> a -> m x) (m x) (x -> m b)

-- | Maps a function on the output of the fold (the type @b@).
instance Applicative m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step start done) = Fold step start done'
        where
        done' x = fmap f $! done x

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

-- | The fold resulting from '<*>' distributes its input to both the argument
-- folds and combines their output using the supplied function.
instance Applicative m => Applicative (Fold m a) where
    {-# INLINE pure #-}
    pure b = Fold (\() _ -> pure ()) (pure ()) (\() -> pure b)

    {-# INLINE (<*>) #-}
    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Tuple' xL xR) a = Tuple' <$> stepL xL a <*> stepR xR a
            begin = Tuple' <$> beginL <*> beginR
            done (Tuple' xL xR) = doneL xL <*> doneR xR
        in  Fold step begin done

    {-# INLINE (<*) #-}
    (<*) m = \_ -> m

    {-# INLINE (*>) #-}
    _ *> m = m

-- | Combines the outputs of the folds (the type @b@) using their 'Semigroup'
-- instances.
instance (Semigroup b, Monad m) => Semigroup (Fold m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

-- | Combines the outputs of the folds (the type @b@) using their 'Monoid'
-- instances.
instance (Semigroup b, Monoid b, Monad m) => Monoid (Fold m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

-- | Combines the fold outputs (type @b@) using their 'Num' instances.
instance (Monad m, Num b) => Num (Fold m a b) where
    {-# INLINE fromInteger #-}
    fromInteger = pure . fromInteger

    {-# INLINE negate #-}
    negate = fmap negate

    {-# INLINE abs #-}
    abs = fmap abs

    {-# INLINE signum #-}
    signum = fmap signum

    {-# INLINE (+) #-}
    (+) = liftA2 (+)

    {-# INLINE (*) #-}
    (*) = liftA2 (*)

    {-# INLINE (-) #-}
    (-) = liftA2 (-)

-- | Combines the fold outputs (type @b@) using their 'Fractional' instances.
instance (Monad m, Fractional b) => Fractional (Fold m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

-- | Combines the fold outputs using their 'Floating' instances.
instance (Monad m, Floating b) => Floating (Fold m a b) where
    {-# INLINE pi #-}
    pi = pure pi

    {-# INLINE exp #-}
    exp = fmap exp

    {-# INLINE sqrt #-}
    sqrt = fmap sqrt

    {-# INLINE log #-}
    log = fmap log

    {-# INLINE sin #-}
    sin = fmap sin

    {-# INLINE tan #-}
    tan = fmap tan

    {-# INLINE cos #-}
    cos = fmap cos

    {-# INLINE asin #-}
    asin = fmap asin

    {-# INLINE atan #-}
    atan = fmap atan

    {-# INLINE acos #-}
    acos = fmap acos

    {-# INLINE sinh #-}
    sinh = fmap sinh

    {-# INLINE tanh #-}
    tanh = fmap tanh

    {-# INLINE cosh #-}
    cosh = fmap cosh

    {-# INLINE asinh #-}
    asinh = fmap asinh

    {-# INLINE atanh #-}
    atanh = fmap atanh

    {-# INLINE acosh #-}
    acosh = fmap acosh

    {-# INLINE (**) #-}
    (**) = liftA2 (**)

    {-# INLINE logBase #-}
    logBase = liftA2 logBase

------------------------------------------------------------------------------
-- Internal APIs
------------------------------------------------------------------------------

-- | @(lmap f fold)@ maps the function @f@ on the input of the fold.
--
-- >>> S.runFold (FL.lmap (\x -> x * x) FL.sum) (S.enumerateFromTo 1 100)
-- 338350
--
-- @since 0.7.0
{-# INLINABLE lmap #-}
lmap :: (a -> b) -> Fold m b r -> Fold m a r
lmap f (Fold step begin done) = Fold step' begin done
  where
    step' x a = step x (f a)

-- | @(lmapM f fold)@ maps the monadic function @f@ on the input of the fold.
--
-- @since 0.7.0
{-# INLINABLE lmapM #-}
lmapM :: Monad m => (a -> m b) -> Fold m b r -> Fold m a r
lmapM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = f a >>= step x

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- >>> S.runFold (lfilter (> 5) FL.sum) [1..10]
-- 40
--
-- @since 0.7.0
{-# INLINABLE lfilter #-}
lfilter :: Monad m => (a -> Bool) -> Fold m a r -> Fold m a r
lfilter f (Fold step begin done) = Fold step' begin done
  where
    step' x a = if f a then step x a else return x

-- | Like 'lfilter' but with a monadic predicate.
--
-- @since 0.7.0
{-# INLINABLE lfilterM #-}
lfilterM :: Monad m => (a -> m Bool) -> Fold m a r -> Fold m a r
lfilterM f (Fold step begin done) = Fold step' begin done
  where
    step' x a = do
      use <- f a
      if use then step x a else return x

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
{-# INLINE lcatMaybes #-}
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = lfilter isJust . lmap fromJust

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.7.0
{-# INLINABLE ltake #-}
ltake :: Monad m => Int -> Fold m a b -> Fold m a b
ltake n (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap (Tuple' 0) initial
    step' (Tuple' i r) a = do
        if i < n
        then do
            res <- step r a
            return $ Tuple' (i + 1) res
        else return $ Tuple' i r
    done' (Tuple' _ r) = done r

-- | Takes elements from the input as long as the predicate succeeds.
--
-- @since 0.7.0
{-# INLINABLE ltakeWhile #-}
ltakeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a b
ltakeWhile predicate (Fold step initial done) = Fold step' initial' done'
    where
    initial' = fmap Left' initial
    step' (Left' r) a = do
        if predicate a
        then fmap Left' $ step r a
        else return (Right' r)
    step' r _ = return r
    done' (Left' r) = done r
    done' (Right' r) = done r

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------
--
-- | Modify the fold such that when the fold is done, instead of returning the
-- accumulator, it returns a fold. The returned fold starts from where we left
-- i.e. it uses the last accumulator value as the initial value of the
-- accumulator. Thus we can resume the fold later and feed it more input.
--
-- >> do
-- >    more <- S.runFold (FL.duplicate FL.sum) (S.enumerateFromTo 1 10)
-- >    evenMore <- S.runFold (FL.duplicate more) (S.enumerateFromTo 11 20)
-- >    S.runFold evenMore (S.enumerateFromTo 21 30)
-- > 465
--
-- @since 0.7.0
{-# INLINABLE duplicate #-}
duplicate :: Applicative m => Fold m a b -> Fold m a (Fold m a b)
duplicate (Fold step begin done) =
    Fold step begin (\x -> pure (Fold step (pure x) done))

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
--
{-# INLINE lchunksOf #-}
lchunksOf :: Monad m => Int -> Fold m a b -> Fold m b c -> Fold m a c
lchunksOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    initial' = (Tuple3' 0) <$> initial1 <*> initial2
    step' (Tuple3' i r1 r2) a = do
        if i < n
        then do
            res <- step1 r1 a
            return $ Tuple3' (i + 1) res r2
        else do
            res <- extract1 r1
            acc2 <- step2 r2 res

            i1 <- initial1
            acc1 <- step1 i1 a
            return $ Tuple3' 1 acc1 acc2
    extract' (Tuple3' _ _ r) = extract2 r

-- | Group the input stream into windows of n second each and then fold each
-- group using the provided fold function.
--
-- For example, we can copy and distribute a stream to multiple folds where
-- each fold can group the input differently e.g. by one second, one minute and
-- one hour windows respectively and fold each resulting stream of folds.
--
-- @
--
-- -----Fold m a b----|-Fold n a c-|-Fold n a c-|-...-|----Fold m a c
--
-- @
{-# INLINE lsessionsOf #-}
lsessionsOf :: MonadAsync m => Double -> Fold m a b -> Fold m b c -> Fold m a c
lsessionsOf n (Fold step1 initial1 extract1) (Fold step2 initial2 extract2) =
    Fold step' initial' extract'

    where

    -- XXX MVar may be expensive we need a cheaper synch mechanism here
    initial' = do
        i1 <- initial1
        i2 <- initial2
        mv1 <- liftIO $ newMVar i1
        mv2 <- liftIO $ newMVar (Right i2)
        t <- control $ \run ->
            mask $ \restore -> do
                tid <- forkIO $ catch (restore $ void $ run (timerThread mv1 mv2))
                                      (handleChildException mv2)
                run (return tid)
        return $ Tuple3' t mv1 mv2
    step' acc@(Tuple3' _ mv1 _) a = do
            r1 <- liftIO $ takeMVar mv1
            res <- step1 r1 a
            liftIO $ putMVar mv1 res
            return acc
    extract' (Tuple3' tid _ mv2) = do
        r2 <- liftIO $ takeMVar mv2
        liftIO $ killThread tid
        case r2 of
            Left e -> throwM e
            Right x -> extract2 x

    timerThread mv1 mv2 = do
        liftIO $ threadDelay (round $ n * 1000000)

        r1 <- liftIO $ takeMVar mv1
        i1 <- initial1
        liftIO $ putMVar mv1 i1

        res1 <- extract1 r1
        r2 <- liftIO $ takeMVar mv2
        res <- case r2 of
                    Left _ -> return r2
                    Right x -> fmap Right $ step2 x res1
        liftIO $ putMVar mv2 res
        timerThread mv1 mv2

    handleChildException ::
        MVar (Either SomeException a) -> SomeException -> IO ()
    handleChildException mv2 e = do
        r2 <- takeMVar mv2
        let r = case r2 of
                    Left _ -> r2
                    Right _ -> Left e
        putMVar mv2 r
