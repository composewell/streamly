{-# LANGUAGE UndecidableInstances #-}
#include "inline.hs"

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
      Driver (..)
    , Parse (..)
    , Parser (..)
    , fromPure
    , fromEffect
    , die
    )
where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Catch (MonadCatch, MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import qualified Control.Monad.Fail as Fail


-- | The parse driver result. The driver may stop with a final result, pause
-- with a continuation to resume, or fail with an error.
--
-- /Pre-release/
--
data Driver m a r =
      Stop !Int r
      -- XXX we can use a "resume" and a "stop" continuations instead of Maybe.
      -- measure if that works any better.
    | Partial !Int (Maybe a -> m (Driver m a r))
    | Continue !Int (Maybe a -> m (Driver m a r))
    | Failed String

instance Functor m => Functor (Driver m a) where
    fmap f (Stop n r) = Stop n (f r)
    fmap f (Partial n yld) = Partial n (fmap (fmap f) . yld)
    fmap f (Continue n yld) = Continue n (fmap (fmap f) . yld)
    fmap _ (Failed e) = Failed e

-- The parser's result.
--
-- /Pre-release/
--
data Parse b =
      Done !Int !b      -- Done, no more input needed
    | Error !String     -- Failed

instance Functor Parse where
    fmap f (Done n b) = Done n (f b)
    fmap _ (Error e) = Error e

-- | A continuation passing style parser representation.
newtype Parser m a b = MkParser
    { runParser :: forall r.
           -- The number of elements that were not used by the previous
           -- consumer and should be carried forward.
           Int
           -- (nesting level, used elem count). Nesting level is increased
           -- whenever we enter an Alternative composition and decreased when
           -- it is done. The used element count is a count of elements
           -- consumed by the Alternative. If the Alternative fails we need to
           -- backtrack by this amount.
           --
           -- The nesting level is used in parseDToK to optimize the case when
           -- we are not in an alternative, in that case we do not need to
           -- maintain the element count for backtracking.
        -> (Int, Int)
           -- The first argument is the (nest level, used count) tuple as
           -- described above. The leftover element count is carried as part of
           -- 'Done' constructor of 'Parse'.
        -> ((Int, Int) -> Parse b -> m (Driver m a r))
        -> m (Driver m a r)
    }

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

-- | Maps a function over the output of the parser.
--
instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f parser = MkParser $ \lo st yieldk ->
        let yld s res = yieldk s (fmap f res)
         in runParser parser lo st yld

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
fromPure :: b -> Parser m a b
fromPure b = MkParser $ \lo st yieldk -> yieldk st (Done lo b)

-- | See 'Streamly.Internal.Data.Parser.fromEffect'.
--
-- /Pre-release/
--
{-# INLINE fromEffect #-}
fromEffect :: Monad m => m b -> Parser m a b
fromEffect eff = MkParser $ \lo st yieldk -> eff >>= \b -> yieldk st (Done lo b)

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.serialWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.serialWith'
-- when fusion is important.
--
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    m1 *> m2 = MkParser $ \lo st yieldk ->
        let yield1 s (Done n _) = runParser m2 n s yieldk
            yield1 s (Error e) = yieldk s (Error e)
        in runParser m1 lo st yield1

    {-# INLINE (<*) #-}
    m1 <* m2 = MkParser $ \lo st yieldk ->
        let yield1 s (Done n b) =
                let yield2 s1 (Done n1 _) = yieldk s1 (Done n1 b)
                    yield2 s1 (Error e) = yieldk s1 (Error e)
                in runParser m2 n s yield2
            yield1 s (Error e) = yieldk s (Error e)
        in runParser m1 lo st yield1

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

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
die :: String -> Parser m a b
die err = MkParser (\_ st yieldk -> yieldk st (Error err))

-- | Monad composition can be used for lookbehind parsers, we can make the
-- future parses depend on the previously parsed values.
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following parser:
--
-- @
-- backtracking :: MonadCatch m => PR.Parser m Char String
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
-- lookbehind :: MonadCatch m => PR.Parser m Char String
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
instance Monad m => Monad (Parser m a) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    m >>= k = MkParser $ \lo st yieldk ->
        let yield1 s (Done n b) = runParser (k b) n s yieldk
            yield1 s (Error e) = yieldk s (Error e)
         in runParser m lo st yield1

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif
instance Monad m => Fail.MonadFail (Parser m a) where
    {-# INLINE fail #-}
    fail = die

instance (MonadThrow m, MonadReader r m, MonadCatch m) =>
    MonadReader r (Parser m a) where

    {-# INLINE ask #-}
    ask = fromEffect ask

    {-# INLINE local #-}
    local f dp =
        MkParser $ \lo st yieldk ->
        local f $ runParser dp lo st yieldk

instance (MonadThrow m, MonadState s m) => MonadState s (Parser m a) where
    {-# INLINE get #-}
    get = fromEffect get

    {-# INLINE put #-}
    put = fromEffect . put

instance (MonadThrow m, MonadIO m) => MonadIO (Parser m a) where
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
instance Monad m => Alternative (Parser m a) where
    {-# INLINE empty #-}
    empty = die "empty"

    {-# INLINE (<|>) #-}
    m1 <|> m2 = MkParser $ \lo (level, _) yieldk ->
        let yield1 (0, _) _ = error "0 nest level in Alternative"
            yield1 (lvl, _) (Done n b) = yieldk (lvl - 1, 0) (Done n b)
            yield1 (lvl, cnt) (Error _) = runParser m2 cnt (lvl - 1, 0) yieldk
        in runParser m1 lo (level + 1, 0) yield1

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
-- /Pre-release/
--
instance Monad m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)
