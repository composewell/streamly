-- |
-- Module      : Streamly.Internal.Data.Parser.ParserK.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- CPS style implementation of parsers.
--
-- The CPS representation allows linear performance for Applicative, sequenceA,
-- Monad, sequence, and Alternative, choice operations compared to the
-- quadratic complexity of the corresponding direct style operations. However,
-- direct style operations allow fusion with ~10x better performance than CPS.
--
-- The direct style representation does not allow for recursive definitions of
-- "some" and "many" whereas CPS allows that.

module Streamly.Internal.Data.Parser.ParserK.Type
    (
      Step (..)
    , Parse (..)
    , Parser (..)
    , fromPure
    , fromEffect
    , die
    )
where

#include "inline.hs"

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Fail as Fail

-- | The intermediate result of running a parser step. The parser driver may
-- stop with a final result, pause with a continuation to resume, or fail with
-- an error.
--
-- See ParserD docs. This is the same as the ParserD Step except that it uses a
-- continuation in Partial and Continue constructors instead of a state in case
-- of ParserD.
--
-- /Pre-release/
--
data Step m a r =
      Done !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
    | Partial !Int (Maybe a -> m (Step m a r))
    | Continue !Int (Maybe a -> m (Step m a r))
    | Error String

instance Functor m => Functor (Step m a) where
    fmap f (Done n r) = Done n (f r)
    fmap f (Partial n k) = Partial n (fmap (fmap f) . k)
    fmap f (Continue n k) = Continue n (fmap (fmap f) . k)
    fmap _ (Error e) = Error e

-- | The parser's result.
--
-- /Pre-release/
--
data Parse b =
      Success !Int !b     -- Leftover count, result
    | Failure !String     -- Error

-- | Map a function over 'Success'.
instance Functor Parse where
    fmap f (Success n b) = Success n (f b)
    fmap _ (Failure e) = Failure e

-- | A continuation passing style parser representation. A continuation of
-- 'Step's, each step passes a state and a parse result to the next 'Step'. The
-- resulting 'Step' may carry a continuation that consumes input 'a' and
-- results in another 'Step'. Essentially, the continuation may either consume
-- input without a result or return a result with no further input to be
-- consumed.
--
newtype Parser a m b = MkParser
    { runParser :: forall r.
           -- leftover: the number of elements that were not used by the
           -- previous consumer and should be carried forward.
           Int
           -- (alt nesting level, alt used elem count). Nesting level is
           -- increased whenever we enter an Alternative composition and
           -- decreased when it is done. The used element count is a count of
           -- elements consumed by the Alternative. If the Alternative fails we
           -- need to backtrack by this amount.
           --
           -- The nesting level is used in parseDToK to optimize the case when
           -- we are not in an alternative, in that case we do not need to
           -- maintain the element count for backtracking.
        -> (Int, Int)
           -- The first argument is the (nest level, used count) tuple as
           -- described above. The leftover element count is carried as part of
           -- 'Success' constructor of 'Parse'.
        -> ((Int, Int) -> Parse b -> m (Step m a r))
        -> m (Step m a r)
    }

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- XXX rewrite this using ParserD, expose rmapM from ParserD.
-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (Parser a m) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \n st k ->
        let k1 s res = k s (fmap f res)
         in runParser parser n st k1

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- This is the dual of stream "fromPure".
--
-- | A parser that always yields a pure value without consuming any input.
--
-- /Pre-release/
--
{-# INLINE fromPure #-}
fromPure :: b -> Parser a m b
fromPure b = MkParser $ \n st k -> k st (Success n b)

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Parser a m b
fromEffect eff = MkParser $ \n st k -> eff >>= \b -> k st (Success n b)

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.splitWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.splitWith'
-- when fusion is important.
--
instance Monad m => Applicative (Parser a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    p1 *> p2 = MkParser $ \n st k ->
        let k1 s (Success n1 _) = runParser p2 n1 s k
            k1 s (Failure e) = k s (Failure e)
        in runParser p1 n st k1

    {-# INLINE (<*) #-}
    p1 <* p2 = MkParser $ \n st k ->
        let k1 s1 (Success n1 b) =
                let k2 s2 (Success n2 _) = k s2 (Success n2 b)
                    k2 s2 (Failure e) = k s2 (Failure e)
                in runParser p2 n1 s1 k2
            k1 s1 (Failure e) = k s1 (Failure e)
        in runParser p1 n st k1

    {-# INLINE liftA2 #-}
    liftA2 f p = (<*>) (fmap f p)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- This is the dual of "nil".
--
-- | A parser that always fails with an error message without consuming
-- any input.
--
-- /Pre-release/
--
{-# INLINE die #-}
die :: String -> Parser a m b
die err = MkParser (\_ st k -> k st (Failure err))

-- | Monad composition can be used for lookbehind parsers, we can make the
-- future parses depend on the previously parsed values.
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following parser:
--
-- @
-- backtracking :: MonadCatch m => PR.Parser Char m String
-- backtracking =
--     sequence [PR.satisfy isDigit, PR.satisfy isAlpha]
--     '<|>'
--     sequence [PR.satisfy isAlpha, PR.satisfy isDigit]
-- @
--
-- We know that if the first parse resulted in a digit at the first place then
-- the second parse is going to fail.  However, we waste that information and
-- parse the first character again in the second parse only to know that it is
-- not an alphabetic char.  By using lookbehind in a 'Monad' composition we can
-- avoid redundant work:
--
-- @
-- data DigitOrAlpha = Digit Char | Alpha Char
--
-- lookbehind :: MonadCatch m => PR.Parser Char m String
-- lookbehind = do
--     x1 \<-    Digit '<$>' PR.satisfy isDigit
--          '<|>' Alpha '<$>' PR.satisfy isAlpha
--
--     -- Note: the parse depends on what we parsed already
--     x2 <- case x1 of
--         Digit _ -> PR.satisfy isAlpha
--         Alpha _ -> PR.satisfy isDigit
--
--     return $ case x1 of
--         Digit x -> [x,x2]
--         Alpha x -> [x,x2]
-- @
--
-- See also 'Streamly.Internal.Data.Parser.concatMap'. This monad instance
-- does not fuse, use 'Streamly.Internal.Data.Parser.concatMap' when you need
-- fusion.
--
instance Monad m => Monad (Parser a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    p >>= f = MkParser $ \n st k ->
        let k1 s1 (Success n1 b) = runParser (f b) n1 s1 k
            k1 s1 (Failure e) = k s1 (Failure e)
         in runParser p n st k1

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif
instance Monad m => Fail.MonadFail (Parser a m) where
    {-# INLINE fail #-}
    fail = die

instance MonadIO m => MonadIO (Parser a m) where
    {-# INLINE liftIO #-}
    liftIO = fromEffect . liftIO

-------------------------------------------------------------------------------
-- Alternative
-------------------------------------------------------------------------------

-- | 'Alternative' form of 'Streamly.Internal.Data.Parser.alt'. Backtrack and
-- run the second parser if the first one fails.
--
-- The "some" and "many" operations of alternative accumulate results in a pure
-- list which is not scalable and streaming. Instead use
-- 'Streamly.Internal.Data.Parser.some' and
-- 'Streamly.Internal.Data.Parser.many' for fusible operations with composable
-- accumulation of results.
--
-- See also 'Streamly.Internal.Data.Parser.alt'. This 'Alternative' instance
-- does not fuse, use 'Streamly.Internal.Data.Parser.alt' when you need
-- fusion.
--
instance Monad m => Alternative (Parser a m) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    p1 <|> p2 = MkParser $ \n (level, _) k ->
        let k1 (0, _) _ = error "Bug: 0 nest level in Alternative"
            k1 (l1, n1) (Failure _) = runParser p2 n1 (l1 - 1, 0) k
            k1 (l1, _) success = k (l1 - 1, 0) success
        in runParser p1 n (level + 1, 0) k1

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

-- | 'mzero' is same as 'empty', it aborts the parser. 'mplus' is same as
-- '<|>', it selects the first succeeding parser.
--
instance Monad m => MonadPlus (Parser a m) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)

{-
instance MonadTrans (Parser a) where
    {-# INLINE lift #-}
    lift = fromEffect
-}
