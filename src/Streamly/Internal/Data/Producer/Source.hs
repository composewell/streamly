-- |
-- Module      : Streamly.Internal.Data.Producer.Source
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A 'Source' is a seed that can be unfolded to a stream with a buffer.  Allows
-- to 'unread' data i.e.  push unused data back to the source buffer. This is
-- useful in parsing applications with backtracking.
--

module Streamly.Internal.Data.Producer.Source
    ( Source

    -- * Creation
    , source

    -- * Transformation
    , unread

    -- * Consumption
    , isEmpty
    , producer

    -- * Parsing
    , parse
    , parseMany
    , parseManyD
    , parseOnceD
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.Catch (MonadThrow, throwM)
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser.ParserD (ParseError(..))
import Streamly.Internal.Data.Producer.Type (Producer(..), Step(..), Inject(..))
-- import Streamly.Internal.Data.Array.Foreign.Types (Array)

import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Parser.ParserK.Types as ParserK

import Prelude hiding (read)

-- | A seed with a buffer. It allows us to 'unread' or return some data
-- after reading it. Useful in backtracked parsing.
--
data Source a b = Source [b] (Maybe a)

-- | Make a source from a seed value. The buffer would start as empty.
--
-- /Internal/
source :: Maybe a -> Source a b
source = Source []

-- | Return some unused data back to the source. The data is prepended (or
-- consed) to the source.
--
-- /Internal/
unread :: [b] -> Source a b -> Source a b
unread xs (Source ys seed) = Source (xs ++ ys) seed

-- | Determine if the source is empty.
isEmpty :: Source a b -> Bool
isEmpty (Source [] Nothing) = True
isEmpty _ = False

data ProducerState s a b =
      ProducerStream s
    | ProducerBuffer [b] a
    | ProducerFinal b a
    | ProducerBufferOnly [b]

-- | Convert a producer to a producer from a buffered source. Any buffered data
-- is read first and then the seed is unfolded.
--
-- /Internal/
{-# INLINE_NORMAL producer #-}
producer :: Monad m => Producer m a b -> Producer m (Source a b) b
producer (Producer step1 inject1 extract1) = Producer step inject extract

    where

    inject (Source [] (Just a)) = do
        r <- inject1 a
        return $ case r of
            INil x -> INil (Source [] (Just x))
            IFinal b x -> IFinal b (Source [] (Just x))
            ISkip s -> ISkip (ProducerStream s)
    inject src@(Source [] Nothing) = return $ INil src
    inject (Source xs Nothing) = return $ ISkip $ ProducerBufferOnly xs
    inject (Source xs (Just a)) = return $ ISkip $ ProducerBuffer xs a

    {-# INLINE_LATE step #-}
    step (ProducerStream s) = do
        r <- step1 s
        return $ case r of
            Stop -> Stop
            Skip s1 -> Skip (ProducerStream s1)
            Nil a -> Nil (Source [] (Just a))
            Result b -> Result b
            Final b a -> Final b (Source [] (Just a))
            Partial b s1 -> Partial b (ProducerStream s1)

    step (ProducerBuffer [] a) = return $ Nil (Source [] (Just a))
    step (ProducerBuffer (x:[]) a) = do
        r <- inject1 a
        return $ case r of
            INil a1 -> Final x (Source [] (Just a1))
            IFinal b a1 -> Partial x (ProducerFinal b a1)
            ISkip s -> Partial x (ProducerStream s)
    step (ProducerBuffer (x:xs) a) = return $ Partial x (ProducerBuffer xs a)
    step (ProducerFinal b a) = return $ Final b (Source [] (Just a))
    step (ProducerBufferOnly []) = return Stop
    step (ProducerBufferOnly (x:[])) = return $ Result x
    step (ProducerBufferOnly (x:xs)) = return $ Partial x (ProducerBufferOnly xs)

    extract (ProducerStream s) = Source [] . Just <$> extract1 s
    extract (ProducerBuffer xs a) = return $ Source xs (Just a)
    extract (ProducerBufferOnly xs) = return $ Source xs Nothing
    extract (ProducerFinal b a) = return $ Source [b] (Just a)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

{-# INLINE_NORMAL parseD #-}
parseD
    :: MonadThrow m
    => ParserD.Parser m a b
    -> Producer m (Source s a) a
    -> Source s a
    -> m (b, Source s a)
parseD
    (ParserD.Parser pstep initial extract)
    (Producer ustep uinject uextract)
    seed = do

    r <- uinject seed
    case r of
        INil src -> do
            pst <- initial
            b <- extract pst
            return (b, src)
        IFinal a src -> initial >>= gobufFinal SPEC src (List []) (List [a])
        ISkip state -> initial >>= go SPEC state (List [])

    where

    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    {-# INLINE go #-}
    go !_ st buf !pst = do
        r <- ustep st
        case r of
            Partial x s -> do
                pRes <- pstep pst x
                case pRes of
                    ParserD.Partial 0 pst1 -> go SPEC s (List []) pst1
                    ParserD.Partial n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1
                    ParserD.Continue 0 pst1 -> go SPEC s (List (x:getList buf)) pst1
                    ParserD.Continue n pst1 -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1
                    ParserD.Done n b -> do
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        s1 <- uextract s
                        return (b, unread src s1)
                    ParserD.Error err -> throwM $ ParseError err
            Skip s -> go SPEC s buf pst
            Nil src -> do
                b <- extract pst
                return (b, unread (reverse $ getList buf) src)
            Stop -> do
                b <- extract pst
                return (b, Source [] Nothing)
            Result a -> gobufFinal SPEC (Source [] Nothing) buf (List [a]) pst
            Final a src -> gobufFinal SPEC src buf (List [a]) pst

    -- gobuf SPEC stream-state backtrack-buffer forward-buffer parser-state
    gobuf !_ s buf (List []) !pst = go SPEC s buf pst
    gobuf !_ s buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            ParserD.Partial 0 pst1 ->
                gobuf SPEC s (List []) (List xs) pst1
            ParserD.Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List []) (List src) pst1
            ParserD.Continue 0 pst1 ->
                gobuf SPEC s (List (x:getList buf)) (List xs) pst1
            ParserD.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1
            ParserD.Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                s1 <- uextract s
                return (b, unread src s1)
            ParserD.Error err -> throwM $ ParseError err

    gobufFinal !_ s _ (List []) !pst = do
        b <- extract pst
        return (b, s)
    gobufFinal !_ s buf (List (x:xs)) !pst = do
        pRes <- pstep pst x
        case pRes of
            ParserD.Partial 0 pst1 ->
                gobufFinal SPEC s (List []) (List xs) pst1
            ParserD.Partial n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobufFinal SPEC s (List []) (List src) pst1
            ParserD.Continue 0 pst1 ->
                gobufFinal SPEC s (List (x:getList buf)) (List xs) pst1
            ParserD.Continue n pst1 -> do
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobufFinal SPEC s (List buf1) (List src) pst1
            ParserD.Done n b -> do
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0
                return (b, unread src s)
            ParserD.Error err -> throwM $ ParseError err

-- | Parse a buffered source using a parser, returning the parsed value and the
-- remaining source.
--
-- /Internal/
{-# INLINE [3] parse #-}
parse
    :: MonadThrow m
    => ParserK.Parser m a b
    -> Producer m (Source s a) a
    -> Source s a
    -> m (b, Source s a)
parse = parseD . ParserK.fromParserK

-------------------------------------------------------------------------------
-- Nested parsing
-------------------------------------------------------------------------------

{-# INLINE parseManyD #-}
parseManyD :: MonadThrow m =>
       ParserD.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseManyD parser reader = Producer step (return . ISkip) return

    where

    {-# INLINE_LATE step #-}
    step src = do
        if isEmpty src
        then return Stop
        else do
            (b, s1) <- parseD parser reader src
            return $ Partial b s1

-- | Apply a parser repeatedly on a buffered source producer to generate a
-- producer of parsed values.
--
-- /Internal/
{-# INLINE parseMany #-}
parseMany :: MonadThrow m =>
       ParserK.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseMany parser = parseManyD (ParserK.fromParserK parser)

-- XXX We should not have parseMany, just parseOnce should be enough and we can
-- compose those to parseMany. We do not need isEmpty now.
-- We can make this independent of the Source type. Instead of implementing
-- parseD we can directly implement the producer, without having to return the
-- Source.
{-# INLINE parseOnceD #-}
parseOnceD :: MonadThrow m =>
       ParserD.Parser m a b
    -> Producer m (Source x a) a
    -> Producer m (Source x a) b
parseOnceD parser reader = Producer step (return . ISkip) return

    where

    {-# INLINE_LATE step #-}
    step src = do
        (b, s1) <- parseD parser reader src
        return $ Final b s1

{-
{-# INLINE parseArrayD #-}
parseArrayD :: MonadThrow m =>
       ParserD.Parser m a b
    -> Producer m (Source x (Array a)) (Array a)
    -> Producer m (Source x (Array a)) b
parseArrayD parser reader = Producer step (return . Left) extract

    where

    {-# INLINE_LATE step #-}
    step (Left src) = do
        (b, s1) <- parseD parser reader src
        return $ Yield b (Right s1)
    step (Right src) = do
        return $ Stop $ Just src

    extract (Left src) = return $ Just src
    extract (Right src) = return $ Just src

-}
