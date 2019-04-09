{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor          #-}

-- |
-- Module      : Streamly.Parse.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Parse.Types
    (
      Parse (..)
    , Status (..)
    , fromResult
    )
where

import Control.Applicative (liftA2, Alternative(..))
import Streamly.Internal.MonadLazy (MonadLazy(..))
import Streamly.Fold.Types (Pair'(..))
import Streamly.Foldr.Types

{-
-- Parse result. Failure gives the partial state of the accumulator at failure.
-- Or should we just use "Failure"? Or should we encode the reason of failure
-- i.e. a failure string or some other type?
data Result a =
      Partial !a
    | Success !a
    | Failure !a
-}

-- When the fold is done we return a value with the completion flag (Success).
-- We can instead return a Partial and return a "Stop" without a value on the
-- next iteration, however that means the driver would lose one extra value in
-- driving the fold one more extra time. The driver can always buffer the
-- previous input so that it does not lose it. That way we can also implement
-- combinators like "takeWhile" which only decide to Stop after looking at the
-- next input. However, it may be more efficient for the fold to return the
-- value instead of the driver storing it, because this is not a common case.
--
-- XXX we need to have a contructor for returning an unconsumed value as well.
-- Otherwise we cannot implement takeWhile correctly. An alternative may be for
-- the driver to remember the last value and we just return a status whether we
-- consumed it or not.
--
-- takeWhile is actually a special case of backtracking. For example in case of
-- "string" parser which matches a string we may have to return a lot more
-- unconsumed input if the matching fails at the end of the string. However,
-- for failing parsers we can use the Alternative instance to do the
-- backtracking. For takeWhile case the parser succeeds but it still returns an
-- unconsumed value.
--
-- Return a x
--
data Status a = Partial !a | Success !a

instance Functor Status where
    fmap f (Success a) = Success (f a)
    fmap f (Partial a) = Partial (f a)

{-
instance Applicative Result where
   pure = More
   Done f <*> Done a = Done (f a)
   Done f <*> More a = More (f a)
   More f <*> Done a = More (f a)
   More f <*> More a = More (f a)
   -}

-- XXX rename to fromStatus
fromResult :: Status a -> a
fromResult res =
    case res of
        Success a -> a
        Partial a -> a

data Parse m a b =
  -- | @Parse @ @ step @ @ initial @ @ extract@
  forall x. Parse (x -> a -> m (Status x)) (m (Status x)) (x -> m b)

instance Monad m => Functor (Parse m a) where
    {-# INLINE fmap #-}
    fmap f (Parse step initial done) = Parse step initial done'
        where
        done' x = fmap f $! done x

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

-- For folds/parses the following types of applicatives are possible:
--
-- 1) Parallel applicative feeding the input to all folds (fold type) (Zip)
-- 2) Serial chained applicative feeding remaining input to the next fold
-- (parse type) (DFS)
-- 3) Distribute one input element to each fold in a round-robin fashion (BFS)
--
-- Fold
-- WFold
-- ZFold
--
{-
instance Monad m => Applicative (Parse m a) where
    {-# INLINE pure #-}
    -- XXX run the action instead of ignoring it??
    pure b = Parse (\_ _ -> pure $ Success ()) (pure $ Success ()) (\_ -> pure b)

    {-# INLINE (<*>) #-}
    Parse stepL initialL doneL <*> Parse stepR initialR doneR =
        let step x@(Pair' xL xR) a =
                    -- XXX we can keep xL and xR without the Result wrapper
                    case xL of
                        Success _ ->
                            case xR of
                                -- XXX should not occur
                                Success _ -> return (Success x)
                                Partial r -> do
                                    resR <- stepR r a
                                    return $ case resR of
                                        Success _ -> Success $ Pair' xL resR
                                        Partial _ -> Partial $ Pair' xL resR
                        Partial l ->
                            case xR of
                                Success _ -> do
                                    resL <- stepL l a
                                    return $ case resL of
                                        Success _ -> Success $ Pair' resL xR
                                        Partial _ -> Partial $ Pair' resL xR
                                Partial r -> do
                                    resL <- stepL l a
                                    resR <- stepR r a
                                    return $ case (resL, resR) of
                                        (Success _, Success _) -> Success $ Pair' resL resR
                                        (_, _)           -> Partial $ Pair' resL resR

            initial = do
                resL <- initialL
                resR <- initialR
                return $ case (resL, resR) of
                    (Success _, Success _) -> Success $ Pair' resL resR
                    (_, _)           -> Partial $ Pair' resL resR

            done (Pair' xL xR) =
                doneL (fromResult xL) <*> doneR (fromResult xR)

        in  Parse step initial done
        -}

data ChainState x1 f x = ParseL x1 | ParseR f x | ParseDone f x

instance Monad m => Applicative (Parse m a) where
    {-# INLINE pure #-}
    -- XXX run the action instead of ignoring it??
    pure b = Parse (\_ _ -> pure $ Success ()) (pure $ Success ()) (\_ -> pure b)

    {-# INLINE (<*>) #-}
    (Parse stepL initialL doneL) <*> (Parse stepR initialR doneR) =
        let step (ParseDone f x) _ = return $ Success (ParseDone f x)
            step (ParseR f x) a = do
                resR <- stepR x a
                case resR of
                    Success r -> return $ Success $ ParseDone f r
                    Partial r -> return $ Partial $ ParseR f r

            step b@(ParseL f) a = do
                    resL <- stepL f a
                    case resL of
                        Success x -> do
                            f <- doneL x
                            resR <- initialR
                            case resR of
                                Success r -> return $ Success $ ParseDone f r
                                Partial r -> return $ Partial $ ParseR f r
                        Partial x -> return $ Partial $ ParseL x

            initial = do
                resL <- initialL
                case resL of
                    Success x -> do
                        f <- doneL x
                        resR <- initialR
                        case resR of
                            Success r -> return $ Success $ ParseDone f r
                            Partial r -> return $ Partial $ ParseR f r
                    Partial x -> return $ Partial (ParseL x)

            done (ParseDone f x) = do
                r <- doneR x
                return $ f r
            done _ = error "incomplete or failed parse"
        in Parse step initial done

{-
instance Monad m => Monad (Parse m a) where
    return = pure
    Parse step initial done >>= f =
        let step' = undefined
            initial' = undefined
            done' = undefined
        in  Parse step' initial' done'
        -}

-- There are two Alternative instances possible:
-- 1) Get first succeeding fold
-- 2) Get all succeeding folds (to get all possible parses)
{-
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
    -}
