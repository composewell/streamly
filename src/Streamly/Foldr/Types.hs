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

import Control.Applicative (liftA2, Alternative(..))
import Streamly.Internal.MonadLazy (MonadLazy(..))
import Streamly.Foldl.Types (Pair'(..))

------------------------------------------------------------------------------
-- Left vs right folds
------------------------------------------------------------------------------

-- A strict foldl is a push based fold i.e. the producer calls the step
-- function which in turn calls the step functions of the composed folds and so
-- on. Producer has the control and pushes the whole stream inside the fold.
-- The fold can choose to use it or ignore it.
--
-- A lazy foldr is a pull based fold i.e. the end consumers call their step
-- functions and demand the next value from the producer. As the consumer fold
-- gets evaluated, it in turn demands the values from the composed folds and so
-- on, finally demanding a value from the origin producer.
--
-- A foldl pushes the full stream to all folds in a composition. Whereas a
-- foldr has the control to terminate early. However, we can wrap a left fold in
-- a functor and check the result after each push to terminate or to continue.
-- Similarly, we can convert a left fold in a scan which can be evaluated
-- lazily. Therefore, it is possible to implement the functionality of foldr
-- using foldl and vice-versa.
--
------------------------------------------------------------------------------
-- Applicative reconstructions
------------------------------------------------------------------------------
--
-- Right folds work well for reconstructing a new structure from an input
-- structure. We can reconstruct a structure applicatively i.e. applicatively
-- combine the results of multiple folds on each element and construct a lazy
-- output structure from the results.
--
-- Each fold in the composition can either run in parallel or they can run
-- serially.
--
------------------------------------------------------------------------------
-- Alternative reconstructions
------------------------------------------------------------------------------
--
-- We can apply multiple right folds on an input stream and reconstruct the
-- output choosing one of the results.


------------------------------------------------------------------------------
-- Parses are right Folds
------------------------------------------------------------------------------
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
-- can succeed or fail. Non short-circuiting left folds always succeed, they do
-- not have a failure.  For example the "length" combinator cannot fail.

------------------------------------------------------------------------------
-- Composing right Folds
------------------------------------------------------------------------------

-- Right folds are lazy puller computations. A composed right fold consumer or
-- comonadic right fold would pull from a common source and distribute the
-- input to multiple right folds in the composition.  In right folds we have
-- lazy pulling computations and therefore they can terminate early and stop
-- pulling. If all the computations in a composed right fold stop pulling then
-- the whole computation terminates. The source needs to copy and distribute
-- the input, therefore it needs to retain an input until it has been pulled by
-- all the folds in a composition.
--
-- Now we can compose the consuming folds in different ways. One, use the
-- results of all folds applicatively.  Two, only one of the results is chosen
-- (alternative). In a monoidal composition the outputs of all the folds can be
-- merged into a single lazy stream.
--
------------------------------------------------------------------------------
-- Applicative composition
------------------------------------------------------------------------------

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
--
--
-- * We can do one fold and then buffer the entire stream for the next fold.
--
-- * If we compose a lazy computation from the results of two folds then how it
-- works may depend on the evaluation of the composed result. For example, if
-- it evaluates the first fold result completely first then we will end up
-- buffering the entire stream for the next fold. In another model the two
-- pullers can work in tandem such that both of them consume one items at a
-- time, one of them consumes and then yields for the other one.
--
-- * Or we can have an applicative scan and then use a left fold on that.
--
-- * In an alternative composition, there are two ways:
--  * we first try the first fold and buffer the entire stream for the next
--  fold. This is how backtracking parsers (attoparsec) work.
--  * Use a commutative style, where we run all the folds fairly and then
--  take the one that finishes first.


-- | Represents a right fold from a container of values of type @a@ to a single
-- value of type @b@ in 'Monad' @m@. Each step of the fold can be applied
-- incrementally by explicitly calling the @step@ function and the accumulated
-- value can be extracted at any point by calling the @extract@ function.

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
-- It seems pretty difficult to compose two lazy right folds in IO
-- efficiently such that the input is given to both the folds because the
-- two folds have to be evaluated in tandem. If we represent both the folds
-- as lazy values then evaluating one of them must evaluate the other as
-- well. This means that we need to evaluate both of them as a combined
-- value rather than independently evaluatable values. The combined fold
-- should terminate when both of them terminate. We consume the result only
-- when both of them are evaluated.
--
-- However, it should be possible to implement a parallel composition using
-- early terminating/short circuiting left folds, by representing the result of
-- a left fold as a functor with "Done" or "More" constructors. Then we can
-- drive the left folds until both result in a "Done".
--
instance MonadLazy m => Applicative (Foldr m a) where
    {-# INLINE pure #-}
    -- XXX run the action instead of ignoring it??
    pure b = Foldr (\_ _ -> pure ()) (pure ()) (\_ -> pure b)

    -- Performance needs to be improved. Observations for 100,000 elements:
    --  Identity monad any/any: ~100 us (1x) rss: 22MB GCCopy: 0
    --  Identity monad any/all: ~1   ms (10x) rss: 22MB GCCopy: 10K
    --  IO Monad any/all      : ~10  ms (1000x) rss: 22MB GCCopy: 12MB
    --
    -- One conjecture is that the Identity monad can evaluate the folds in
    -- parallel because one fold need not depend on another, both can evaluate
    -- independently, that can make it much faster. However, the IO monad needs
    -- to evaluate in sequence therefore both the folds alternate the
    -- evaluation.
    --
    -- This implementation does not scale, the memory used is proportional to
    -- the size of the stream. Because Pair is allocated in each recursive
    -- step and held until the end. We need to have a truly lazy/streaming
    -- implementation that uses continuations rather than holding data.
    --
    {-# INLINE (<*>) #-}
    Foldr stepL finalL projectL <*> Foldr stepR finalR projectR =
        let step a xs = do
                lazyBind xs (\ ~(Pair' xL xR) -> do
                    return $ Pair' (stepL a xL) (stepR a xR))

            final = return $ Pair' finalL finalR
            project x = do
                (Pair' xL xR) <- x
                projectL xL <*> projectR xR
        in Foldr step final project

-- XXX We should perhaps have just "Alt" implementation instead of
-- "Alternative".  Because we do not have a sensible identity for Alternative.
--
-- If the first fold fails we run the second fold.
instance MonadLazy m => Alternative (Foldr m a) where
    {-# INLINE empty #-}
    empty = Foldr (\_ _ -> fail "step empty")
                  (fail "begin empty")
                  (\_ -> fail "extract empty")

    {-# INLINE (<|>) #-}
    Foldr stepL finalL projectL <|> Foldr stepR finalR projectR = undefined

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
