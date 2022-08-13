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
-- However, they are less powerful than regular parsers because these cannot
-- fail.

module Streamly.Internal.Data.Parser.ParserD.NonFailing
    (
      noErrorUnsafeSplit_
    , noErrorUnsafeSplitWith
    , noErrorUnsafeConcatMap

    , fromFold
    , next
    , takeWhile
    , wordBy
    , groupBy
    , groupByRolling
    , groupByRollingEither
    , span
    , spanBy
    , spanByRolling
    )
where

import Control.Monad.Catch (throwM, MonadThrow)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Parser.ParserD.Type
    ( Initial(..), Step(..), Parser(..), SeqParseState(..), SeqAState(..)
    , ConcatParseState(..), ParseError(..)
    )
import qualified Streamly.Internal.Data.Fold.Type as FL

import Prelude hiding (concatMap, filter, takeWhile, span)

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

--
-- | See 'Streamly.Internal.Data.Parser.fromFold'.
--
-- /Pre-release/
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Parser m a b
fromFold (Fold fstep finitial fextract) = Parser step initial fextract

    where

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s1 -> IPartial s1
                  FL.Done b -> IDone b

    step s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 s1
                  FL.Done b -> Done 0 b

-- | See 'Streamly.Internal.Data.Parser.next'.
--
-- /Pre-release/
--
{-# INLINE next #-}
next :: Monad m => Parser m a (Maybe a)
next = Parser step initial extract
  where
  initial = pure $ IPartial ()
  step _ a = pure $ Done 0 (Just a)
  extract _ = pure Nothing

-- | See 'Streamly.Internal.Data.Parser.takeWhile'.
--
-- /Pre-release/
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
takeWhile predicate (Fold fstep finitial fextract) =
    Parser step initial fextract

    where

    initial = do
        res <- finitial
        return $ case res of
            FL.Partial s -> IPartial s
            FL.Done b -> IDone b

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      FL.Partial s1 -> Partial 0 s1
                      FL.Done b -> Done 0 b
        else Done 1 <$> fextract s

data WordByState s b = WBLeft !s | WBWord !s | WBRight !b

-- | See 'Streamly.Internal.Data.Parser.wordBy'.
--
--
{-# INLINE wordBy #-}
wordBy :: Monad m => (a -> Bool) -> Fold m a b -> Parser m a b
wordBy predicate (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE worder #-}
    worder s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Partial s1 -> Partial 0 $ WBWord s1
                  FL.Done b -> Done 0 b

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ WBLeft s
                  FL.Done b -> IDone b

    step (WBLeft s) a =
        if not (predicate a)
        then worder s a
        else return $ Partial 0 $ WBLeft s
    step (WBWord s) a =
        if not (predicate a)
        then worder s a
        else do
            b <- fextract s
            return $ Partial 0 $ WBRight b
    step (WBRight b) a =
        return
            $ if not (predicate a)
              then Done 1 b
              else Partial 0 $ WBRight b

    extract (WBLeft s) = fextract s
    extract (WBWord s) = fextract s
    extract (WBRight b) = return b

{-# ANN type GroupByState Fuse #-}
data GroupByState a s
    = GroupByInit !s
    | GroupByGrouping !a !s

-- | See 'Streamly.Internal.Data.Parser.groupBy'.
--
{-# INLINE groupBy #-}
groupBy :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupBy eq (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a0 a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a0 s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ GroupByInit s
                  FL.Done b -> IDone b

    step (GroupByInit s) a = grouper s a a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a0 a
        else Done 1 <$> fextract s

    extract (GroupByInit s) = fextract s
    extract (GroupByGrouping _ s) = fextract s

-- | See 'Streamly.Internal.Data.Parser.groupByRolling'.
--
{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => (a -> a -> Bool) -> Fold m a b -> Parser m a b
groupByRolling eq (Fold fstep finitial fextract) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s a = do
        res <- fstep s a
        return
            $ case res of
                  FL.Done b -> Done 0 b
                  FL.Partial s1 -> Partial 0 (GroupByGrouping a s1)

    initial = do
        res <- finitial
        return
            $ case res of
                  FL.Partial s -> IPartial $ GroupByInit s
                  FL.Done b -> IDone b

    step (GroupByInit s) a = grouper s a
    step (GroupByGrouping a0 s) a =
        if eq a0 a
        then grouper s a
        else Done 1 <$> fextract s

    extract (GroupByInit s) = fextract s
    extract (GroupByGrouping _ s) = fextract s

{-# ANN type GroupByStatePair Fuse #-}
data GroupByStatePair a s1 s2
    = GroupByInitPair !s1 !s2
    | GroupByGroupingPair !a !s1 !s2
    | GroupByGroupingPairL !a !s1 !s2
    | GroupByGroupingPairR !a !s1 !s2

{-# INLINE groupByRollingEither #-}
groupByRollingEither :: Monad m =>
    (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (Either b c)
groupByRollingEither
    eq
    (Fold fstep1 finitial1 fextract1)
    (Fold fstep2 finitial2 fextract2) = Parser step initial extract

    where

    {-# INLINE grouper #-}
    grouper s1 s2 a = do
        return $ Continue 0 (GroupByGroupingPair a s1 s2)

    {-# INLINE grouperL2 #-}
    grouperL2 s1 s2 a = do
        res <- fstep1 s1 a
        return
            $ case res of
                FL.Done b -> Done 0 (Left b)
                FL.Partial s11 -> Partial 0 (GroupByGroupingPairL a s11 s2)

    {-# INLINE grouperL #-}
    grouperL s1 s2 a0 a = do
        res <- fstep1 s1 a0
        case res of
            FL.Done b -> return $ Done 0 (Left b)
            FL.Partial s11 -> grouperL2 s11 s2 a

    {-# INLINE grouperR2 #-}
    grouperR2 s1 s2 a = do
        res <- fstep2 s2 a
        return
            $ case res of
                FL.Done b -> Done 0 (Right b)
                FL.Partial s21 -> Partial 0 (GroupByGroupingPairR a s1 s21)

    {-# INLINE grouperR #-}
    grouperR s1 s2 a0 a = do
        res <- fstep2 s2 a0
        case res of
            FL.Done b -> return $ Done 0 (Right b)
            FL.Partial s21 -> grouperR2 s1 s21 a

    initial = do
        res1 <- finitial1
        res2 <- finitial2
        return
            $ case res1 of
                FL.Partial s1 ->
                    case res2 of
                        FL.Partial s2 -> IPartial $ GroupByInitPair s1 s2
                        FL.Done b -> IDone (Right b)
                FL.Done b -> IDone (Left b)

    step (GroupByInitPair s1 s2) a = grouper s1 s2 a

    step (GroupByGroupingPair a0 s1 s2) a =
        if not (eq a0 a)
        then grouperL s1 s2 a0 a
        else grouperR s1 s2 a0 a

    step (GroupByGroupingPairL a0 s1 s2) a =
        if not (eq a0 a)
        then grouperL2 s1 s2 a
        else Done 1 . Left <$> fextract1 s1

    step (GroupByGroupingPairR a0 s1 s2) a =
        if eq a0 a
        then grouperR2 s1 s2 a
        else Done 1 . Right <$> fextract2 s2

    extract (GroupByInitPair s1 _) = Left <$> fextract1 s1
    extract (GroupByGroupingPairL _ s1 _) = Left <$> fextract1 s1
    extract (GroupByGroupingPairR _ _ s2) = Right <$> fextract2 s2
    extract (GroupByGroupingPair a s1 _) = do
                res <- fstep1 s1 a
                case res of
                    FL.Done b -> return $ Left b
                    FL.Partial s11 -> Left <$> fextract1 s11

--------------------------------------------------------------------------------
--- Spanning
--------------------------------------------------------------------------------

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- @
-- > let span_ p xs = Stream.parse (Parser.span p Fold.toList Fold.toList) $ Stream.fromList xs
--
-- > span_ (< 1) [1,2,3]
-- ([],[1,2,3])
--
-- > span_ (< 2) [1,2,3]
-- ([1],[2,3])
--
-- > span_ (< 4) [1,2,3]
-- ([1,2,3],[])
--
-- @
--
-- /Pre-release/
{-# INLINE span #-}
span :: Monad m => (a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (b, c)
span p f1 f2 = noErrorUnsafeSplitWith (,) (takeWhile p f1) (fromFold f2)

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
-- /Pre-release/
--
{-# INLINE spanBy #-}
spanBy ::
       Monad m
    => (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (b, c)
spanBy eq f1 f2 = noErrorUnsafeSplitWith (,) (groupBy eq f1) (fromFold f2)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
--
-- /Pre-release/
{-# INLINE spanByRolling #-}
spanByRolling ::
       Monad m
    => (a -> a -> Bool) -> Fold m a b -> Fold m a c -> Parser m a (b, c)
spanByRolling eq f1 f2 =
    noErrorUnsafeSplitWith (,) (groupByRolling eq f1) (fromFold f2)
