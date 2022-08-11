-- |
-- Module      : Streamly.Internal.Data.Parser.ParserD.NonFailing
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Parsers that can backtrack but never fail. Because they cannot fail they
-- cannot have an alternative instance. This enables us to write more
-- efficient sequential parsers, because we do not need buffering for the
-- failure case.
--
-- These parsers lie between parsers that can fail and folds. They are more
-- powerful than folds because they add the backtracking capability to folds.
-- However, they are less powerful than parsers that can fail.

module Streamly.Internal.Data.Parser.ParserD.NonFailing
    (
      noErrorUnsafeSplit_
    , noErrorUnsafeSplitWith
    , noErrorUnsafeConcatMap
    )
where

import Control.Monad.Catch (throwM, MonadThrow)
import Streamly.Internal.Data.Parser.ParserD.Type
    ( Initial(..), Step(..), Parser(..), SeqParseState(..), SeqAState(..)
    , ConcatParseState(..), ParseError(..)
    )

import Prelude hiding (concatMap, filter)
--
-- $setup
-- >>> :m
-- >>> :set -package streamly
-- >>> import Control.Applicative ((<|>))
-- >>> import Prelude hiding (concatMap)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (parse)
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.Data.Parser.ParserD as ParserD

-- | Works correctly only if the first parser is guaranteed to never fail.
{-# INLINE noErrorUnsafeSplitWith #-}
noErrorUnsafeSplitWith :: Monad m
    => (a -> b -> c) -> Parser m x a -> Parser m x b -> Parser m x c
noErrorUnsafeSplitWith func (Parser stepL initialL extractL)
               (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqParseL sl
            IDone bl -> do
                resR <- initialR
                return $ case resR of
                    IPartial sr -> IPartial $ SeqParseR (func bl) sr
                    IDone br -> IDone (func bl br)
                    IError err -> IError err
            IError err -> return $ IError err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqParseL st) a = do
        r <- stepL st a
        case r of
            -- Assume that the first parser can never fail, therefore we do not
            -- need to keep the input for backtracking.
            Partial n s -> return $ Partial n (SeqParseL s)
            Continue n s -> return $ Continue n (SeqParseL s)
            Done n b -> do
                res <- initialR
                return
                    $ case res of
                          IPartial sr -> Partial n $ SeqParseR (func b) sr
                          IDone br -> Done n (func b br)
                          IError err -> Error err
            Error err -> return $ Error err

    step (SeqParseR f st) a = do
        r <- stepR st a
        return $ case r of
            Partial n s -> Partial n (SeqParseR f s)
            Continue n s -> Continue n (SeqParseR f s)
            Done n b -> Done n (f b)
            Error err -> Error err

    extract (SeqParseR f sR) = fmap f (extractR sR)
    extract (SeqParseL sL) = do
        rL <- extractL sL
        res <- initialR
        case res of
            IPartial sR -> do
                rR <- extractR sR
                return $ func rL rR
            IDone rR -> return $ func rL rR
            IError err -> error $ "noErrorUnsafeSplitWith: cannot use a "
                ++ "failing parser. Parser failed with: " ++ err

{-# INLINE noErrorUnsafeSplit_ #-}
noErrorUnsafeSplit_ :: MonadThrow m => Parser m x a -> Parser m x b -> Parser m x b
noErrorUnsafeSplit_ (Parser stepL initialL extractL) (Parser stepR initialR extractR) =
    Parser step initial extract

    where

    initial = do
        resL <- initialL
        case resL of
            IPartial sl -> return $ IPartial $ SeqAL sl
            IDone _ -> do
                resR <- initialR
                return $ case resR of
                    IPartial sr -> IPartial $ SeqAR sr
                    IDone br -> IDone br
                    IError err -> IError err
            IError err -> return $ IError err

    -- Note: For the composed parse to terminate, the left parser has to be
    -- a terminating parser returning a Done at some point.
    step (SeqAL st) a = do
        -- Important: Please do not use Applicative here. Applicative somehow
        -- caused the next action to run many times in the "tar" parsing code,
        -- not sure why though.
        resL <- stepL st a
        case resL of
            Partial n s -> return $ Partial n (SeqAL s)
            Continue n s -> return $ Continue n (SeqAL s)
            Done n _ -> do
                initR <- initialR
                return $ case initR of
                    IPartial s -> Partial n (SeqAR s)
                    IDone b -> Done n b
                    IError err -> Error err
            Error err -> return $ Error err

    step (SeqAR st) a =
        (\case
            Partial n s -> Partial n (SeqAR s)
            Continue n s -> Continue n (SeqAR s)
            Done n b -> Done n b
            Error err -> Error err) <$> stepR st a

    extract (SeqAR sR) = extractR sR
    extract (SeqAL sL) = do
        _ <- extractL sL
        res <- initialR
        case res of
            IPartial sR -> extractR sR
            IDone rR -> return rR
            IError err -> throwM $ ParseError err

{-# INLINE noErrorUnsafeConcatMap #-}
noErrorUnsafeConcatMap :: MonadThrow m =>
    (b -> Parser m a c) -> Parser m a b -> Parser m a c
noErrorUnsafeConcatMap func (Parser stepL initialL extractL) =
    Parser step initial extract

    where

    {-# INLINE initializeR #-}
    initializeR (Parser stepR initialR extractR) = do
        resR <- initialR
        return $ case resR of
            IPartial sr -> IPartial $ ConcatParseR stepR sr extractR
            IDone br -> IDone br
            IError err -> IError err

    initial = do
        res <- initialL
        case res of
            IPartial s -> return $ IPartial $ ConcatParseL s
            IDone b -> initializeR (func b)
            IError err -> return $ IError err

    {-# INLINE initializeRL #-}
    initializeRL n (Parser stepR initialR extractR) = do
        resR <- initialR
        return $ case resR of
            IPartial sr -> Partial n $ ConcatParseR stepR sr extractR
            IDone br -> Done n br
            IError err -> Error err

    step (ConcatParseL st) a = do
        r <- stepL st a
        case r of
            Partial n s -> return $ Partial n (ConcatParseL s)
            Continue n s -> return $ Continue n (ConcatParseL s)
            Done n b -> initializeRL n (func b)
            Error err -> return $ Error err

    step (ConcatParseR stepR st extractR) a = do
        r <- stepR st a
        return $ case r of
            Partial n s -> Partial n $ ConcatParseR stepR s extractR
            Continue n s -> Continue n $ ConcatParseR stepR s extractR
            Done n b -> Done n b
            Error err -> Error err

    {-# INLINE extractP #-}
    extractP (Parser _ initialR extractR) = do
        res <- initialR
        case res of
            IPartial s -> extractR s
            IDone b -> return b
            IError err -> throwM $ ParseError err

    extract (ConcatParseR _ s extractR) = extractR s
    extract (ConcatParseL sL) = extractL sL >>= extractP . func
