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
    , Result (..)
    , fromResult
    )
where

import Control.Applicative (liftA2, Alternative(..))
import Streamly.Internal.MonadLazy (MonadLazy(..))
import Streamly.Foldl.Types (Pair'(..))
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

data Result a = Done !a | More !a

{-
instance Functor Result where
    fmap f (Done a) = Done (f a)
    fmap f (More a) = More (f a)

instance Applicative Result where
   pure = More
   Done f <*> Done a = Done (f a)
   Done f <*> More a = More (f a)
   More f <*> Done a = More (f a)
   More f <*> More a = More (f a)
   -}

fromResult :: Result a -> a
fromResult res =
    case res of
        Done a -> a
        More a -> a

-- Folds that return a Maybe are parsers.
data Parse m a b =
  -- | @Foldl @ @ step @ @ initial @ @ extract@
  forall x. Parse (x -> a -> m (Result x)) (m (Result x)) (x -> m b)

instance Monad m => Functor (Parse m a) where
    {-# INLINE fmap #-}
    fmap f (Parse step initial done) = Parse step initial done'
        where
        done' x = fmap f $! done x

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

instance Monad m => Applicative (Parse m a) where
    {-# INLINE pure #-}
    -- XXX run the action instead of ignoring it??
    pure b = Parse (\_ _ -> pure $ Done ()) (pure $ Done ()) (\_ -> pure b)

    {-# INLINE (<*>) #-}
    Parse stepL initialL doneL <*> Parse stepR initialR doneR =
        let step x@(Pair' xL xR) a =
                    -- XXX we can keep xL and xR without the Result wrapper
                    case xL of
                        Done _ ->
                            case xR of
                                -- XXX should not occur
                                Done _ -> return (Done x)
                                More r -> do
                                    resR <- stepR r a
                                    return $ case resR of
                                        Done _ -> Done $ Pair' xL resR
                                        More _ -> More $ Pair' xL resR
                        More l ->
                            case xR of
                                Done _ -> do
                                    resL <- stepL l a
                                    return $ case resL of
                                        Done _ -> Done $ Pair' resL xR
                                        More _ -> More $ Pair' resL xR
                                More r -> do
                                    resL <- stepL l a
                                    resR <- stepR r a
                                    return $ case (resL, resR) of
                                        (Done _, Done _) -> Done $ Pair' resL resR
                                        (_, _)           -> More $ Pair' resL resR

            initial = do
                resL <- initialL
                resR <- initialR
                return $ case (resL, resR) of
                    (Done _, Done _) -> Done $ Pair' resL resR
                    (_, _)           -> More $ Pair' resL resR

            done (Pair' xL xR) =
                doneL (fromResult xL) <*> doneR (fromResult xR)

        in  Parse step initial done

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
