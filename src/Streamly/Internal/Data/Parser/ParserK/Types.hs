#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Parser.ParserK.Types
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

module Streamly.Internal.Data.Parser.ParserK.Types
    (
      Parser (..)
    , yield
    , die
    )
where

import Control.Monad (MonadPlus(..), ap)
import Control.Applicative (Alternative(..))
import Streamly.Internal.Data.Zipper (Zipper)
import qualified Streamly.Internal.Data.Zipper as Z

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

newtype Parser m a b =
    MkParser { runParser :: forall r.
        Zipper m a -> ((Zipper m a, Either String b) -> m r) -> m r }

-- | Maps a function over the output of the fold.
--
instance Functor m => Functor (Parser m a) where
    {-# INLINE fmap #-}
    fmap f parser =
        MkParser $ \inp yieldk -> runParser parser inp (yieldk . fmap (fmap f))

-- | See 'Streamly.Internal.Data.Parser.yield'.
--
-- /Internal/
--
{-# INLINE yield #-}
yield :: b -> Parser m a b
yield b = MkParser (\inp yieldk -> yieldk (inp, Right b))

-------------------------------------------------------------------------------
-- Sequential applicative
-------------------------------------------------------------------------------

-- | 'Applicative' form of 'Streamly.Internal.Data.Parser.splitWith'. Note that
-- this operation does not fuse, use 'Streamly.Internal.Data.Parser.splitWith'
-- when fusion is important.
--
instance Monad m => Applicative (Parser m a) where
    {-# INLINE pure #-}
    pure = yield

    {-# INLINE (<*>) #-}
    (<*>) = ap

    {-# INLINE (*>) #-}
    m1 *> m2 = MkParser $ \inp yieldk ->
        let yield1 (z, b) = case b of
                Right _ -> runParser m2 z yieldk
                Left err -> runParser (die err) z yieldk
        in runParser m1 inp yield1

    {-# INLINE (<*) #-}
    m1 <* m2 = MkParser $ \inp yieldk ->
        let yield1 (z, b) = case b of
                Right _ -> runParser m2 z (\(z1, _) -> yieldk (z1, b))
                Left err -> runParser (die err) z yieldk
        in runParser m1 inp yield1

-- | See 'Streamly.Internal.Data.Parser.die'.
--
-- /Internal/
--
{-# INLINE die #-}
die :: String -> Parser m a b
die err = MkParser (\z yieldk -> yieldk (z, Left err))

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
    m >>= k = MkParser $ \inp yieldk ->
        let yield1 (z, b) = case b of
                Right x -> runParser (k x) z yieldk
                Left err -> runParser (die err) z yieldk
        in runParser m inp yield1

#if !(MIN_VERSION_base(4,13,0))
    -- This is redefined instead of just being Fail.fail to be
    -- compatible with base 4.8.
    {-# INLINE fail #-}
    fail = die
#endif

#if MIN_VERSION_base(4,9,0)
instance Monad m => Fail.MonadFail (Parser m a) where
    {-# INLINE fail #-}
    fail = die
#endif

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
    m1 <|> m2 = MkParser $ \inp yieldk ->
        let yield1 (z, b) = case b of
                Right _ -> yieldk (Z.release z, b)
                Left _ -> runParser m2 (Z.restore z) yieldk
        in runParser m1 (Z.checkpoint inp) yield1

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
-- /Internal/
--
instance Monad m => MonadPlus (Parser m a) where
    {-# INLINE mzero #-}
    mzero = die "mzero"

    {-# INLINE mplus #-}
    mplus = (<|>)
