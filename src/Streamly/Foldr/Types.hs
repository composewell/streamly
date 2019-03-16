{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE BangPatterns          #-}

-- |
-- Module      : Streamly.Foldr.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Foldr.Types
    (
      Foldr (..)
    )
where

import Control.Applicative (liftA2)
import Streamly.Internal.MonadLazy (MonadLazy(..))
import Streamly.Foldl.Types (Pair'(..))

------------------------------------------------------------------------------
-- Comonadic right folds
------------------------------------------------------------------------------

-- Foldl is a push based fold i.e. the producer calls the step function which in
-- turn calls the step functions of the composed folds and so on. Producer
-- has the control and pushes the whole stream inside the fold. The fold can
-- choose to use it or ignore it.
--
-- Foldr is a pull based fold i.e. the end consumers call their step functions
-- and demand the next value from the producer. As the end fold demands it in
-- turn will demand the values from the composed folds and so on, finally
-- demanding a value from the origin producer.
--
-- With Foldr we can branch out a stream into multiple different branches each
-- of which can pull lazily and terminate early. For example, we can try
-- different parses on a stream and then applicatively combine the results.
-- That way we can get all successful parses. Or we can compose differently and
-- select the first successful parse. If a parse fails we will not be consuming
-- the whole stream before we return, we can return as soon as it fails and try
-- the next one. Also, we will have the inputs consumed till now buffered so
-- that we can start the next fold from the beginning.
--
-- All parses are right folds in general. For example even "all/or/and"
-- combinators are acutally parses. Any early termination is a parse because it
-- can succeed or fail. Left folds always succeed, they do not have a failure.
-- For example the "length" combinator cannot fail.

-- Right folds are lazy puller computations. A composed right fold consumer or
-- comonadic right fold would pull from a common source and distribute the
-- input to multiple right folds in the composition. In comonadic left folds
-- the source pushes into each fold and therefore each fold recieves full input
-- irrespective of whether it uses it or not. In right folds we have pulling
-- computations and therefore they can terminate early and stop pulling. If all
-- the computations in a composed right fold stop pulling then the whole
-- computation terminates. The source needs to copy and distribute the input,
-- therefore it needs to retain an input until it has been pulled by all the
-- folds in a composition.
--
-- Now we can compose the consuming folds in different ways. One, all the
-- results are to be used ultimately (applicative). Two, only one of the
-- results is chosen (alternative). In a monoidal composition the outputs of
-- all the folds can be merged into a single lazy stream.
--
-- To implement composition of right folds we probably need a puller that pulls
-- and distributes the input to individual folds. there are multiple ways to
-- have multiple consumers pulling:
--
-- * when an element is pulled/demanded by any of the folds we queue it to all
-- the folds, each fold has its own buffer. This way we need a buffer but
-- different folds can run at different speeds.
--
-- * When a fold demands an element it is made to wait until the element has
-- been consumed by all folds, therefore all folds run at the speed of the
-- slowest fold. This is in fact a special case of the buffered case, in this
-- case we have a single element buffer.



-- | Represents a right fold from a container of values of type @a@ to a single
-- value of type @b@ in 'Monad' @m@. Each step of the fold can be applied
-- incrementally by explicitly calling the @step@ function and the accumulated
-- value can be extracted at any point by calling the @extract@ function.

-- Right fold via left fold
-- One of the models to implement a partial fold is to push values to a left
-- fold and examine the state after that. We stop pushing if we have got the
-- result. This essentially means that we force a certain structure to the
-- accumulator of the left fold. However, this would not be general. To have a
-- general right fold equivalent we would perhaps have to make the accumulator
-- a functor.
data Foldr m a b =
  -- | @Foldr@ @step@ @final@ @project@
  forall x. Foldr (a -> m x -> m x) (m x) (m x -> m b)

-- In an alternative composition all folds can receive the same type of input
-- and one of them is chosen.

instance MonadLazy m => Functor (Foldr m a) where
    {-# INLINE fmap #-}
    fmap f (Foldr step final project) = Foldr step final (fmap f . project)

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

-- Run two right folds in parallel sharing the same input and composing the
-- output using a function.
--
instance MonadLazy m => Applicative (Foldr m a) where
    {-# INLINE pure #-}
    -- XXX run the action instead of ignoring it??
    pure b = Foldr (\_ _ -> pure ()) (pure ()) (\_ -> pure b)

    -- Strict Pair makes a big difference with Identity monad benchmark but
    -- makes no difference in IO.
    --
    -- Performance needs to be improved for this. Observations till now:
    -- IO Monad any/all composed: 9 ms
    -- Identity monad any/all: 1 ms
    -- StrictIdentity monad any/all: 1 ms
    -- MyIO with lazyBind (ignoring the token) everywhere: 9 ms
    -- MyIO with boxed tuples, ignoring the token : 1 ms
    -- Identity monad any/any: 94 us
    --
    -- We need to strive for the last figure for all cases.
    --
    {-# INLINE (<*>) #-}
    Foldr stepL finalL projectL <*> Foldr stepR finalR projectR =
        let step a xs = do
                lazyBind xs (\ ~(Pair' xL xR) -> do
                    return $ Pair' (stepL a xL) (stepR a xR))

            final = return $ Pair' finalL finalR
            -- pass a continuation to consume the lazy values
            project x = do
                (Pair' xL xR) <- x
                projectL xL <*> projectR xR
                -- x `lazyBind` \ ~(Pair' xL xR) -> projectL xL `lazyBind` \l -> projectR xR `lazyBind` \r -> return (l r)
        in Foldr step final project

{-
lazyBind (xs) (\ ~(Pair' xL xR) -> do
    return $ Pair' (if predicate x then return True else xL)
                   (if predicate x then return True else xR))

    stepL x xs = if predicate x then return True else xs
    stepR x xs = if predicate x then xs else return False

lazyBind (IO m) k = IO ( \ s ->
        let r = case m s of (# _, res #) -> res
        in unIO (k r) s)

((\ ~xs@(Pair' xL xR) -> do
    return $ Pair' (if predicate x then return True else xL)
                   (if predicate x then return True else xR)) lazyXS token)

    inside the any/all steps the xL/xR are extractions from Pair' constructor
    so the evaluation of the Pair' to whnf will lead to the next lazy pair
    constructor.

    step x xs = if predicate x then xs else return False
    -}

instance (Semigroup b, MonadLazy m) => Semigroup (Foldr m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

instance (Monoid b, MonadLazy m) => Monoid (Foldr m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

instance (MonadLazy m, Num b) => Num (Foldr m a b) where
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

instance (MonadLazy m, Fractional b) => Fractional (Foldr m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

instance (MonadLazy m, Floating b) => Floating (Foldr m a b) where
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
