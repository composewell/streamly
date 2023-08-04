-- |
-- Module      : Streamly.Internal.Data.ChunkParserK.Generic
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unconstrained version of "Streamly.Internal.Data.ChunkParserK" module.
--
-- See the "Streamly.Internal.Data.ChunkParserK" module for documentation.
--
--
module Streamly.Internal.Data.ChunkParserK.Generic
    (
      Step (..)
    , Input (..)
    , ParseResult (..)
    , ChunkParserK (..)
    , fromParser
    -- , toParser
    , fromPure
    , fromEffect
    , die
    )
where

#include "ArrayMacros.h"
#include "assert.hs"
#include "inline.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Generic (Array(..))
import Streamly.Internal.Data.MutArray.Generic (getIndexUnsafeWith)
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Control.Monad.Fail as Fail
import qualified Streamly.Internal.Data.Array.Generic as Array
import qualified Streamly.Internal.Data.Parser.ParserD.Type as ParserD

data Input a = None | Chunk (Array a)

-- | See "Streamly.Internal.Data.ChunkParserK.Step" for documentation.
--
data Step a m r =
    -- The Int is the current stream position index wrt to the start of the
    -- array.
      Done !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
      -- Array a -> m (Step a m r), m (Step a m r)
    | Partial !Int (Input a -> m (Step a m r))
    | Continue !Int (Input a -> m (Step a m r))
    | Error !Int String

instance Functor m => Functor (Step a m) where
    fmap f (Done n r) = Done n (f r)
    fmap f (Partial n k) = Partial n (fmap (fmap f) . k)
    fmap f (Continue n k) = Continue n (fmap (fmap f) . k)
    fmap _ (Error n e) = Error n e

data ParseResult b =
      Success !Int !b      -- Position index, result
    | Failure !Int !String -- Position index, error

instance Functor ParseResult where
    fmap f (Success n b) = Success n (f b)
    fmap _ (Failure n e) = Failure n e

newtype ChunkParserK a m b = MkParser
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

instance Functor m => Functor (ChunkParserK a m) where
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
fromPure :: b -> ChunkParserK a m b
fromPure b = MkParser $ \k n st arr -> k (Success n b) st arr

-- | Similar to "Streamly.Internal.Data.ChunkParserK.fromEffect".
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> ChunkParserK a m b
fromEffect eff =
    MkParser $ \k n st arr -> eff >>= \b -> k (Success n b) st arr

instance Monad m => Applicative (ChunkParserK a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    p1 *> p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 _) s input = runParser p2 k n1 s input
            k1 (Failure n1 e) s input = k (Failure n1 e) s input
        in runParser p1 k1 n st arr

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 input =
                let k2 (Success n2 _) s2 input2  = k (Success n2 b) s2 input2
                    k2 (Failure n2 e) s2 input2  = k (Failure n2 e) s2 input2
                in runParser p2 k2 n1 s1 input
            k1 (Failure n1 e) s1 input = k (Failure n1 e) s1 input
        in runParser p1 k1 n st arr

    {-# INLINE liftA2 #-}
    liftA2 f p = (<*>) (fmap f p)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

{-# INLINE die #-}
die :: String -> ChunkParserK a m b
die err = MkParser (\k n st arr -> k (Failure n err) st arr)

instance Monad m => Monad (ChunkParserK a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    p >>= f = MkParser $ \k n st arr ->
        let k1 (Success n1 b) s1 inp = runParser (f b) k n1 s1 inp
            k1 (Failure n1 e) s1 inp = k (Failure n1 e) s1 inp
         in runParser p k1 n st arr

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif
instance Monad m => Fail.MonadFail (ChunkParserK a m) where
    {-# INLINE fail #-}
    fail = die

instance MonadIO m => MonadIO (ChunkParserK a m) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO

-------------------------------------------------------------------------------
-- Alternative
-------------------------------------------------------------------------------

instance Monad m => Alternative (ChunkParserK a m) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    p1 <|> p2 = MkParser $ \k n _ arr ->
        let
            k1 (Failure pos _) used input = runParser p2 k (pos - used) 0 input
            k1 success _ input = k success 0 input
        in runParser p1 k1 n 0 arr

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

instance Monad m => MonadPlus (ChunkParserK a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)

-------------------------------------------------------------------------------
-- Convert ParserD to ChunkParserK
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
parseDToK pstep initial extract cont !offset0 !usedCount !input = do
    res <- initial
    case res of
        ParserD.IPartial pst -> do
            case input of
                Chunk arr -> parseContChunk usedCount offset0 pst arr
                None -> parseContNothing usedCount pst
        ParserD.IDone b -> cont (Success offset0 b) usedCount input
        ParserD.IError err -> cont (Failure offset0 err) usedCount input

    where

    {-# NOINLINE parseContChunk #-}
    parseContChunk !count !offset !state arr@(Array contents start len) = do
         if offset >= 0
         then go SPEC (start + offset) state
         else return $ Continue offset (parseCont count state)

        where

        {-# INLINE end #-}
        end = start + len

        {-# INLINE onDone #-}
        onDone n b =
            assert (n <= Array.length arr)
                (cont (Success n b) (count + n - offset) (Chunk arr))

        {-# INLINE callParseCont #-}
        callParseCont constr n pst1 =
            assert (n < 0 || n >= Array.length arr)
                (return $ constr n (parseCont (count + n - offset) pst1))

        {-# INLINE onPartial #-}
        onPartial = callParseCont Partial

        {-# INLINE onContinue #-}
        onContinue = callParseCont Continue

        {-# INLINE onError #-}
        onError n err =
            cont (Failure n err) (count + n - offset) (Chunk arr)

        {-# INLINE onBack #-}
        onBack offset1 constr pst = do
            let pos = offset1 - start
             in if pos >= 0
                then go SPEC offset1 pst
                else constr pos pst

        go !_ !cur !pst | cur >= end =
            onContinue len  pst
        go !_ !cur !pst = do
            let !x = unsafeInlineIO $ getIndexUnsafeWith contents cur
            pRes <- pstep pst x
            let next = cur + 1
                back n = next - n
                curOff = cur - start
                nextOff = next - start
            -- The "n" here is stream position index wrt the array start, and
            -- not the backtrack count as returned by byte stream parsers.
            case pRes of
                ParserD.Done 0 b ->
                    onDone nextOff b
                ParserD.Done 1 b ->
                    onDone curOff b
                ParserD.Done n b ->
                    onDone (back n - start) b
                ParserD.Partial 0 pst1 ->
                    go SPEC next pst1
                ParserD.Partial 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Partial n pst1 ->
                    onBack (back n) onPartial pst1
                ParserD.Continue 0 pst1 ->
                    go SPEC next pst1
                ParserD.Continue 1 pst1 ->
                    go SPEC cur pst1
                ParserD.Continue n pst1 ->
                    onBack (back n) onContinue pst1
                ParserD.Error err ->
                    onError curOff err

    {-# NOINLINE parseContNothing #-}
    parseContNothing !count !pst = do
        r <- extract pst
        case r of
            -- IMPORTANT: the n here is from the byte stream parser, that means
            -- it is the backtrack element count and not the stream position
            -- index into the current input array.
            ParserD.Done n b ->
                assert (n >= 0)
                    (cont (Success (- n) b) (count - n) None)
            ParserD.Continue n pst1 ->
                assert (n >= 0)
                    (return $ Continue (- n) (parseCont (count - n) pst1))
            ParserD.Error err ->
                -- XXX It is called only when there is no input arr. So using 0
                -- as the position is correct?
                cont (Failure 0 err) count None
            ParserD.Partial _ _ -> error "Bug: parseDToK Partial unreachable"

    {-# INLINE parseCont #-}
    parseCont !cnt !pst (Chunk arr) = parseContChunk cnt 0 pst arr
    parseCont !cnt !pst None = parseContNothing cnt pst

-- | Similar to "Streamly.Internal.Data.ChunkParserK.fromParser".
--
{-# INLINE_LATE fromParser #-}
fromParser :: Monad m => ParserD.Parser a m b -> ChunkParserK a m b
fromParser (ParserD.Parser step initial extract) =
    MkParser $ parseDToK step initial extract
