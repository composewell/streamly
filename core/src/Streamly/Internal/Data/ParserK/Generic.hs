-- |
-- Module      : Streamly.Internal.Data.ParserK.Generic
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unconstrained and single element version of
-- "Streamly.Internal.Data.ChunkParserK" module.
--
-- See the "Streamly.Internal.Data.ChunkParserK" module for documentation.
--
--
module Streamly.Internal.Data.ParserK.Generic
    (
      Step (..)
    , Input (..)
    , ParseResult (..)
    , ParserK (..)
    , fromParser
    -- , toParser
    , fromPure
    , fromEffect
    , die
    )
where

#include "assert.hs"
#include "inline.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Types (SPEC(..))

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Parser.ParserD.Type as ParserD

data Input a = None | Single a

-- | Similar to "Streamly.Internal.Data.ChunkParserK.Step".
--
data Step a m r =
    -- The Int is the relative stream position to move to.
    -- 0 means try the current element again
    -- 1 means try the next element
    -- -1 backtrack by 1 element
      Done !Int r
    | Partial !Int (Input a -> m (Step a m r))
    | Continue !Int (Input a -> m (Step a m r))
    | Error String

instance Functor m => Functor (Step a m) where
    fmap f (Done n r) = Done n (f r)
    fmap f (Partial n k) = Partial n (fmap (fmap f) . k)
    fmap f (Continue n k) = Continue n (fmap (fmap f) . k)
    fmap _ (Error e) = Error e

-- | Similar to "Streamly.Internal.Data.ChunkParserK.ParseResult".
--
data ParseResult b =
      Success !Int !b     -- Relative stream position, result
    | Failure !String     -- Error

-- | Map a function over 'Success'.
instance Functor ParseResult where
    fmap f (Success n b) = Success n (f b)
    fmap _ (Failure e) = Failure e

-- | Similar to "Streamly.Internal.Data.ChunkParserK.ChunkParserK".
--
newtype ParserK a m b = MkParser
    { runParser :: forall r.
           (ParseResult b -> Int -> Input a -> m (Step a m r))
        -> Int
        -> Int
        -> Input a
        -> m (Step a m r)
    }

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

instance Functor m => Functor (ParserK a m) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \k n st arr ->
        let k1 res = k (fmap f res)
         in runParser parser k1 n st arr

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- | Similar to "Streamly.Internal.Data.ChunkParserK.fromPure".
--
{-# INLINE fromPure #-}
fromPure :: b -> ParserK a m b
fromPure b = MkParser $ \k n st arr -> k (Success n b) st arr

-- | Similar to "Streamly.Internal.Data.ChunkParserK.fromEffect".
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ParserK a m b
fromEffect eff =
    MkParser $ \k n st arr -> eff >>= \b -> k (Success n b) st arr

instance Monad m => Applicative (ParserK a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    p1 *> p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 _) s input = runParser p2 k n1 s input
            k1 (Failure e) s input = k (Failure e) s input
        in runParser p1 k1 n st arr

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 input =
                let k2 (Success n2 _) s2 input2  = k (Success n2 b) s2 input2
                    k2 (Failure e) s2 input2  = k (Failure e) s2 input2
                in runParser p2 k2 n1 s1 input
            k1 (Failure e) s1 input = k (Failure e) s1 input
        in runParser p1 k1 n st arr

    {-# INLINE liftA2 #-}
    liftA2 f p = (<*>) (fmap f p)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- | Similar to "Streamly.Internal.Data.ChunkParserK.die".
--
{-# INLINE die #-}
die :: String -> ParserK a m b
die err = MkParser (\k _ st arr -> k (Failure err) st arr)

instance Monad m => Monad (ParserK a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    p >>= f = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 inp = runParser (f b) k n1 s1 inp
            k1 (Failure e) s1 inp = k (Failure e) s1 inp
         in runParser p k1 n st arr

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif
instance Monad m => Fail.MonadFail (ParserK a m) where
    {-# INLINE fail #-}
    fail = die

instance MonadIO m => MonadIO (ParserK a m) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO

-------------------------------------------------------------------------------
-- Alternative
-------------------------------------------------------------------------------

instance Monad m => Alternative (ParserK a m) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    p1 <|> p2 = MkParser $ \k n _ arr ->
        let
            k1 (Failure _) used input = runParser p2 k (- used) 0 input
            k1 success _ input = k success 0 input
        in runParser p1 k1 n 0 arr

    -- some and many are implemented here instead of using default definitions
    -- so that we can use INLINE on them. It gives 50% performance improvement.

    {-# INLINE many #-}
    many v = many_v

        where

        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

    {-# INLINE some #-}
    some v = some_v

        where

        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

instance Monad m => MonadPlus (ParserK a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)

-------------------------------------------------------------------------------
-- Convert ParserD to ParserK
-------------------------------------------------------------------------------

{-# INLINE parseDToK #-}
parseDToK
    :: forall m a s b r. (Monad m)
    => (s -> a -> m (ParserD.Step s b))
    -> m (ParserD.Initial s b)
    -> (s -> m (ParserD.Step s b))
    -> (ParseResult b -> Int -> Input a -> m (Step a m r))
    -> Int
    -> Int
    -> Input a
    -> m (Step a m r)
parseDToK pstep initial extract cont !relPos !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            if relPos == 0
            then
                case input of
                    Single x -> parseContSingle usedCount pst x
                    None -> parseContNothing usedCount pst
            else pure $ Partial relPos (parseCont usedCount pst)
        ParserD.IDone b -> cont (Success 0 b) usedCount input
        ParserD.IError err -> cont (Failure err) usedCount input

    where

    {-# NOINLINE parseContSingle #-}
    parseContSingle !count !state !x =
        go SPEC state

        where

        go !_ !pst = do
            pRes <- pstep pst x
            case pRes of
                ParserD.Done 0 b ->
                    cont (Success 1 b) (count + 1) (Single x)
                ParserD.Done 1 b ->
                    cont (Success 0 b) count (Single x)
                ParserD.Done n b ->
                    cont (Success (1 - n) b) (count + 1 - n) (Single x)
                ParserD.Partial 0 pst1 ->
                    pure $ Partial 1 (parseCont (count + 1) pst1)
                ParserD.Partial 1 pst1 ->
                    go SPEC pst1
                ParserD.Partial n pst1 ->
                    pure $ Partial (1 - n) (parseCont (count + 1 - n) pst1)
                ParserD.Continue 0 pst1 ->
                    pure $ Continue 1 (parseCont (count + 1) pst1)
                ParserD.Continue 1 pst1 ->
                    go SPEC pst1
                ParserD.Continue n pst1 ->
                    pure $ Continue (1 - n) (parseCont (count + 1 - n) pst1)
                ParserD.Error err ->
                    cont (Failure err) count (Single x)

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the relative stream
            -- position.
            ParserD.Done n b ->
                assert (n >= 0)
                    (cont (Success (- n) b) (count - n) None)
            ParserD.Continue n pst1 ->
                assert (n >= 0)
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                cont (Failure err) count None
            ParserD.Partial _ _ -> error "Bug: parseDToK Partial unreachable"

    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Single x) = parseContSingle cnt pst x
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Similar to "Streamly.Internal.Data.ChunkParserK.fromParser".
--
{-# INLINE_LATE fromParser #-}
fromParser :: Monad m => ParserD.Parser a m b -> ParserK a m b
fromParser (ParserD.Parser step initial extract) =
    MkParser $ parseDToK step initial extract
