#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Pipe.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Pipe.Type
    ( Step (..)
    , Pipe (..)
    , PipeState (..)
    , zipWith
    , tee
    , map
    , compose
    )
where

import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Data.Maybe (isJust)
import Prelude hiding (zipWith, map, id, unzip, null)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Prelude

------------------------------------------------------------------------------
-- Pipes
------------------------------------------------------------------------------

-- A scan is a much simpler version of pipes. A scan always produces an output
-- on an input whereas a pipe does not necessarily produce an output on an
-- input, it might consume multiple inputs before producing an output. That way
-- it can implement filtering. Similarly, it can produce more than one output
-- on an single input.
--
-- Therefore when two pipes are composed in parallel formation, one may run
-- slower or faster than the other. If all of them are being fed from the same
-- source, we may have to buffer the input to match the speeds. In case of
-- scans we do not have that problem.
--
-- We may also need a "Stop" constructor to indicate that we are not generating
-- any more values and we can have a "Done" constructor to indicate that we are
-- not consuming any more values. Similarly we can have a stop with error or
-- exception and a done with error or leftover values.
--
-- In generator mode, Continue means no output/continue. In fold mode Continue means
-- need more input to produce result. we can perhaps call it Continue instead.
--
data Step s a =
      Yield a s
    | Continue s

-- | Represents a stateful transformation over an input stream of values of
-- type @a@ to outputs of type @b@ in 'Monad' @m@.

-- A pipe uses a consume function and a produce function. It can switch from
-- consume/fold mode to a produce/source mode. The first step function is a
-- fold function while the seocnd one is a stream generator function.
--
-- We can upgrade a stream or a fold into a pipe. However, streams are more
-- efficient in generation and folds are more efficient in consumption.
--
-- For pure transformation we can have a 'Scan' type. A Scan would be more
-- efficient in zipping whereas pipes are useful for merging and zipping where
-- we know buffering can occur. A Scan type can be upgraded to a pipe.
--
-- XXX In general the starting state could either be for generation or for
-- consumption. Currently we are only starting with a consumption state.
--
-- An explicit either type for better readability of the code
data PipeState s1 s2 = Consume s1 | Produce s2

isProduce :: PipeState s1 s2 -> Bool
isProduce s =
    case s of
        Produce _ -> True
        Consume _ -> False

data Pipe m a b =
  forall s1 s2. Pipe (s1 -> a -> m (Step (PipeState s1 s2) b))
                     (s2 -> m (Step (PipeState s1 s2) b)) s1

instance Monad m => Functor (Pipe m a) where
    {-# INLINE_NORMAL fmap #-}
    fmap f (Pipe consume produce initial) = Pipe consume' produce' initial
        where
        {-# INLINE_LATE consume' #-}
        consume' st a = do
            r <- consume st a
            return $ case r of
                Yield x s -> Yield (f x) s
                Continue s -> Continue s

        {-# INLINE_LATE produce' #-}
        produce' st = do
            r <- produce st
            return $ case r of
                Yield x s -> Yield (f x) s
                Continue s -> Continue s

-- XXX move this to a separate module
data Deque a = Deque [a] [a]

{-# INLINE null #-}
null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

{-# INLINE snoc #-}
snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) = Deque (a : snocList) consList

{-# INLINE uncons #-}
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque snocList consList) =
  case consList of
    h : t -> Just (h, Deque snocList t)
    _ ->
      case Prelude.reverse snocList of
        h : t -> Just (h, Deque [] t)
        _ -> Nothing

-- | The composed pipe distributes the input to both the constituent pipes and
-- zips the output of the two using a supplied zipping function.
--
-- @since 0.7.0
{-# INLINE_NORMAL zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Pipe m i a -> Pipe m i b -> Pipe m i c
zipWith f (Pipe consumeL produceL stateL) (Pipe consumeR produceR stateR) =
                    Pipe consume produce state
        where

        -- Left state means we need to consume input from the source. A Right
        -- state means we either have buffered input or we are in generation
        -- mode so we do not need input from source in either case.
        --
        state = Tuple' (Consume stateL, Nothing, Nothing)
                       (Consume stateR, Nothing, Nothing)

        -- XXX for heavy buffering we need to have the (ring) buffer in pinned
        -- memory using the Storable instance.
        {-# INLINE_LATE consume #-}
        consume (Tuple' (sL, resL, lq) (sR, resR, rq)) a = do
            s1 <- drive sL resL lq consumeL produceL a
            s2 <- drive sR resR rq consumeR produceR a
            yieldOutput s1 s2

            where

            {-# INLINE drive #-}
            drive st res queue fConsume fProduce val =
                case res of
                    Nothing -> goConsume st queue val fConsume fProduce
                    Just x -> return $
                        case queue of
                            Nothing -> (st, Just x, Just $ Deque [val] [])
                            Just q  -> (st, Just x, Just $ snoc val q)

            {-# INLINE goConsume #-}
            goConsume stt queue val fConsume stp2 =
                case stt of
                    Consume st ->
                        case queue of
                            Nothing -> do
                                r <- fConsume st val
                                return $ case r of
                                    Yield x s  -> (s, Just x, Nothing)
                                    Continue s -> (s, Nothing, Nothing)
                            Just queue' ->
                                case uncons queue' of
                                    Just (v, q) -> do
                                        r <- fConsume st v
                                        let q' = snoc val q
                                        return $ case r of
                                            Yield x s  -> (s, Just x, Just q')
                                            Continue s -> (s, Nothing, Just q')
                                    Nothing -> undefined -- never occurs
                    Produce st -> do
                        r <- stp2 st
                        return $ case r of
                            Yield x s  -> (s, Just x, queue)
                            Continue s -> (s, Nothing, queue)

        {-# INLINE_LATE produce #-}
        produce (Tuple' (sL, resL, lq) (sR, resR, rq)) = do
            s1 <- drive sL resL lq consumeL produceL
            s2 <- drive sR resR rq consumeR produceR
            yieldOutput s1 s2

            where

            {-# INLINE drive #-}
            drive stt res q fConsume fProduce =
                case res of
                    Nothing -> goProduce stt q fConsume fProduce
                    Just x -> return (stt, Just x, q)

            {-# INLINE goProduce #-}
            goProduce stt queue fConsume fProduce =
                case stt of
                    Consume st ->
                        case queue of
                            -- See yieldOutput. We enter produce mode only when
                            -- each pipe is either in Produce state or the
                            -- queue is non-empty. So this case cannot occur.
                            Nothing -> undefined
                            Just queue' ->
                                case uncons queue' of
                                    Just (v, q) -> do
                                        r <- fConsume st v
                                        -- We provide a guarantee that if the
                                        -- queue is "Just" it is always
                                        -- non-empty. yieldOutput and goConsume
                                        -- depend on it.
                                        let q' = if null q
                                                 then Nothing
                                                 else Just q
                                        return $ case r of
                                            Yield x s  -> (s, Just x, q')
                                            Continue s -> (s, Nothing, q')
                                    Nothing -> return (stt, Nothing, Nothing)
                    Produce st -> do
                        r <- fProduce st
                        return $ case r of
                            Yield x s  -> (s, Just x, queue)
                            Continue s -> (s, Nothing, queue)

        {-# INLINE yieldOutput #-}
        yieldOutput s1@(sL', resL', lq') s2@(sR', resR', rq') = return $
            -- switch to produce mode if we do not need input
            if (isProduce sL' || isJust lq') && (isProduce sR' || isJust rq')
            then
                case (resL', resR') of
                    (Just xL, Just xR) ->
                        Yield (f xL xR) (Produce (Tuple' (clear s1) (clear s2)))
                    _ -> Continue (Produce (Tuple' s1 s2))
            else
                case (resL', resR') of
                    (Just xL, Just xR) ->
                        Yield (f xL xR) (Consume (Tuple' (clear s1) (clear s2)))
                    _ -> Continue (Consume (Tuple' s1 s2))
            where clear (s, _, q) = (s, Nothing, q)

instance Monad m => Applicative (Pipe m a) where
    {-# INLINE pure #-}
    pure b = Pipe (\_ _ -> pure $ Yield b (Consume ())) undefined ()

    (<*>) = zipWith id

-- | The composed pipe distributes the input to both the constituent pipes and
-- merges the outputs of the two.
--
-- @since 0.7.0
{-# INLINE_NORMAL tee #-}
tee :: Monad m => Pipe m a b -> Pipe m a b -> Pipe m a b
tee (Pipe consumeL produceL stateL) (Pipe consumeR produceR stateR) =
        Pipe consume produce state
    where

    state = Tuple' (Consume stateL) (Consume stateR)

    consume (Tuple' sL sR) a =
        case sL of
            Consume st -> do
                r <- consumeL st a
                return $ case r of
                    Yield x s -> Yield x (Produce (Tuple3' (Just a) s sR))
                    Continue s -> Continue (Produce (Tuple3' (Just a) s sR))
            -- XXX we should never come here unless the initial state of the
            -- first pipe is set to "Right".
            Produce _st -> undefined -- do
            {-
                r <- produceL st
                return $ case r of
                    Yield x s -> Yield x (Right (Tuple3' (Just a) s sR))
                    Continue s -> Continue (Right (Tuple3' (Just a) s sR))
                -}

    produce (Tuple3' (Just a) sL sR) =
        case sL of
            Consume _ ->
                case sR of
                    Consume st -> do
                        r <- consumeR st a
                        let nextL s = Consume (Tuple' sL s)
                        let nextR s = Produce (Tuple3' Nothing sL s)
                        return $ case r of
                            Yield x s@(Consume _) -> Yield x (nextL s)
                            Yield x s@(Produce _) -> Yield x (nextR s)
                            Continue s@(Consume _) -> Continue (nextL s)
                            Continue s@(Produce _) -> Continue (nextR s)
                    -- We will never come here unless the initial state of
                    -- second pipe is set to "Right".
                    Produce _ -> undefined
            Produce st -> do
                r <- produceL st
                let next s = Produce (Tuple3' (Just a) s sR)
                return $ case r of
                    Yield x s -> Yield x (next s)
                    Continue s -> Continue (next s)

    produce (Tuple3' Nothing sL sR) =
        case sR of
            Consume _ -> undefined -- should never occur
            Produce st -> do
                r <- produceR st
                return $ case r of
                    Yield x s@(Consume _) ->
                        Yield x (Consume (Tuple' sL s))
                    Yield x s@(Produce _) ->
                        Yield x (Produce (Tuple3' Nothing sL s))
                    Continue s@(Consume _) ->
                        Continue (Consume (Tuple' sL s))
                    Continue s@(Produce _) ->
                        Continue (Produce (Tuple3' Nothing sL s))

instance Monad m => Semigroup (Pipe m a b) where
    {-# INLINE (<>) #-}
    (<>) = tee

-- | Lift a pure function to a 'Pipe'.
--
-- @since 0.7.0
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Pipe m a b
map f = Pipe consume undefined ()
    where
    consume _ a = return $ Yield (f a) (Consume ())

{-
-- | A hollow or identity 'Pipe' passes through everything that comes in.
--
-- @since 0.7.0
{-# INLINE id #-}
id :: Monad m => Pipe m a a
id = map Prelude.id
-}

-- | Compose two pipes such that the output of the second pipe is attached to
-- the input of the first pipe.
--
-- @since 0.7.0
{-# INLINE_NORMAL compose #-}
compose :: Monad m => Pipe m b c -> Pipe m a b -> Pipe m a c
compose (Pipe consumeL produceL stateL) (Pipe consumeR produceR stateR) =
    Pipe consume produce state

    where

    state = Tuple' (Consume stateL) (Consume stateR)

    consume (Tuple' sL sR) a =
        case sL of
            Consume stt ->
                case sR of
                    Consume st -> do
                        rres <- consumeR st a
                        case rres of
                            Yield x sR' -> do
                                let next s =
                                        if isProduce sR'
                                        then Produce s
                                        else Consume s
                                lres <- consumeL stt x
                                return $ case lres of
                                    Yield y s1@(Consume _) ->
                                        Yield y (next $ Tuple' s1 sR')
                                    Yield y s1@(Produce _) ->
                                        Yield y (Produce $ Tuple' s1 sR')
                                    Continue s1@(Consume _) ->
                                        Continue (next $ Tuple' s1 sR')
                                    Continue s1@(Produce _) ->
                                        Continue (Produce $ Tuple' s1 sR')
                            Continue s1@(Consume _) ->
                                return $ Continue (Consume $ Tuple' sL s1)
                            Continue s1@(Produce _) ->
                                return $ Continue (Produce $ Tuple' sL s1)
                    Produce _ -> undefined
            -- XXX we should never come here unless the initial state of the
            -- first pipe is set to "Right".
            Produce _ -> undefined

    -- XXX we need to write the code in mor optimized fashion. Use Continue
    -- more and less yield points.
    produce (Tuple' sL sR) =
        case sL of
            Produce st -> do
                r <- produceL st
                let next s = if isProduce sR then Produce s else Consume s
                return $ case r of
                    Yield x s@(Consume _) -> Yield x (next $ Tuple' s sR)
                    Yield x s@(Produce _) -> Yield x (Produce $ Tuple' s sR)
                    Continue s@(Consume _) -> Continue (next $ Tuple' s sR)
                    Continue s@(Produce _) -> Continue (Produce $ Tuple' s sR)
            Consume stt ->
                case sR of
                    Produce st -> do
                        rR <- produceR st
                        case rR of
                            Yield x sR' -> do
                                let next s =
                                        if isProduce sR'
                                        then Produce s
                                        else Consume s
                                rL <- consumeL stt x
                                return $ case rL of
                                    Yield y s1@(Consume _) ->
                                        Yield y (next $ Tuple' s1 sR')
                                    Yield y s1@(Produce _) ->
                                        Yield y (Produce $ Tuple' s1 sR')
                                    Continue s1@(Consume _) ->
                                        Continue (next $ Tuple' s1 sR')
                                    Continue s1@(Produce _) ->
                                        Continue (Produce $ Tuple' s1 sR')
                            Continue s1@(Consume _) ->
                                return $ Continue (Consume $ Tuple' sL s1)
                            Continue s1@(Produce _) ->
                                return $ Continue (Produce $ Tuple' sL s1)
                    Consume _ -> return $ Continue (Consume $ Tuple' sL sR)

instance Monad m => Category (Pipe m) where
    {-# INLINE id #-}
    id = map Prelude.id

    {-# INLINE (.) #-}
    (.) = compose

unzip :: Pipe m a x -> Pipe m b y -> Pipe m (a, b) (x, y)
unzip = undefined

instance Monad m => Arrow (Pipe m) where
    {-# INLINE arr #-}
    arr = map

    {-# INLINE (***) #-}
    (***) = unzip

    {-# INLINE (&&&) #-}
    (&&&) = zipWith (,)
