{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Parse
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains folding (eliminating) transformations involving
-- multiple streams. folds. There are two types of transformations generational
-- or eliminational. Eliminational transformations are like the "Eliminate"
-- module but they transform a stream by eliminating parts of the stream
-- instead of eliminating the whole stream.
--
-- These combinators involve transformation, generation, elimination so can be
-- classified under any of those.

-- Flipped versions can be named as:
-- groupsFor :: stream -> fold -> stream (flipped groupsWhile)
--
-- Flipped versions for folds:
-- foldMany :: outer fold -> inner fold -> fold (original version)
-- groupFoldFor :: inner fold -> outer fold -> fold (flipped version)
-- groupStepFor :: inner fold -> outer fold step -> fold (flipped version)
-- This can be convenient for defining the outer fold step using a lambda.
--
module Streamly.Internal.Data.Stream.Parse
    (
    -- * Eliminate
    -- | Folding and Parsing chunks of streams to eliminate nested streams.
    -- Functions generally ending in these shapes:
    --
    -- @
    -- f (Fold m a b) -> t m a -> t m b
    -- f (Parser a m b) -> t m a -> t m b
    -- @

    -- ** Folding
    -- | Apply folds on a stream.
      foldSequence
    , foldIterateM

    -- ** Parsing
    -- | Parsing is opposite to flattening. 'parseMany' is dual to concatMap or
    -- unfoldEach concatMap generates a stream from single values in a
    -- stream and flattens, parseMany does the opposite of flattening by
    -- splitting the stream and then folds each such split to single value in
    -- the output stream.
    , parseMany
    , parseManyPos
    , parseSequence
    , parseManyTill
    , parseIterate
    , parseIteratePos

    -- ** Grouping
    -- | Group segments of a stream and fold. Special case of parsing.
    , groupsWhile
    , groupsRollingBy

    -- ** Splitting
    -- | A special case of parsing.
    , takeEndBySeq
    , takeEndBySeq_
    , wordsBy
    , splitSepBySeq_
    , splitEndBySeq
    , splitEndBySeq_
    , splitOnSuffixSeq -- internal

    , splitBeginBy_
    , splitEndBySeqOneOf
    , splitSepBySeqOneOf

    -- * Transform (Nested Containers)
    -- | Opposite to compact in ArrayStream
    , splitInnerBy -- XXX innerSplitOn
    , splitInnerBySuffix -- XXX innerSplitOnSuffix

    -- * Reduce By Streams
    , dropPrefix
    , dropInfix
    , dropSuffix

    -- * Deprecated
    , parseManyD
    , parseIterateD
    , groupsBy
    , splitOnSeq
    )
where

#include "deprecation.h"
#include "inline.hs"
#include "ArrayMacros.h"

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import Fusion.Plugin.Types (Fuse(..))
import GHC.Types (SPEC(..))

import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.Parser (ParseError(..), ParseErrorPos)
import Streamly.Internal.Data.RingArray (RingArray(..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Unbox (Unbox(..))

import qualified Streamly.Internal.Data.Array.Type as A
import qualified Streamly.Internal.Data.MutArray.Type as MutArray
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser as PRD
import qualified Streamly.Internal.Data.ParserDrivers as Drivers
import qualified Streamly.Internal.Data.RingArray as RB

import Streamly.Internal.Data.Stream.Type

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | Apply a stream of folds to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE foldSequence #-}
foldSequence
       :: -- Monad m =>
       Stream m (Fold m a b)
    -> Stream m a
    -> Stream m b
foldSequence _f _m = undefined

{-# ANN type FIterState Fuse #-}
data FIterState s f m a b
    = FIterInit s f
    | forall fs. FIterStream s (fs -> a -> m (FL.Step fs b)) fs (fs -> m b)
        (fs -> m b)
    | FIterYield b (FIterState s f m a b)
    | FIterStop

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- Usage:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> f x = return (Fold.take 2 (Fold.sconcat x))
-- >>> s = fmap Sum $ Stream.fromList [1..10]
-- >>> Stream.fold Fold.toList $ fmap getSum $ Stream.foldIterateM f (pure 0) s
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- folds where next fold is dependent on the previous fold.
--
-- /Pre-release/
--
{-# INLINE_NORMAL foldIterateM #-}
foldIterateM ::
       Monad m => (b -> m (FL.Fold m a b)) -> m b -> Stream m a -> Stream m b
foldIterateM func seed0 (Stream step state) =
    Stream stepOuter (FIterInit state seed0)

    where

    {-# INLINE iterStep #-}
    iterStep from st fstep extract final = do
        res <- from
        return
            $ Skip
            $ case res of
                  FL.Partial fs -> FIterStream st fstep fs extract final
                  FL.Done fb -> FIterYield fb $ FIterInit st (return fb)

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (FIterInit st seed) = do
        (FL.Fold fstep initial extract final) <- seed >>= func
        iterStep initial st fstep extract final
    stepOuter gst (FIterStream st fstep fs extract final) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                iterStep (fstep fs x) s fstep extract final
            Skip s -> return $ Skip $ FIterStream s fstep fs extract final
            Stop -> do
                b <- final fs
                return $ Skip $ FIterYield b FIterStop
    stepOuter _ (FIterYield a next) = return $ Yield a next
    stepOuter _ FIterStop = return Stop

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- Usage:
--
-- >>> s = Stream.fromList [1..10]
-- >>> parser = Parser.takeBetween 0 2 Fold.sum
-- >>> Stream.toList $ Stream.parseMany parser s
-- [Right 3,Right 7,Right 11,Right 15,Right 19]
--
-- This is the streaming equivalent of the 'Streamly.Data.Parser.many' parse
-- combinator.
--
-- Known Issues: When the parser fails there is no way to get the remaining
-- stream.
--
{-# INLINE parseMany #-}
parseMany
    :: Monad m
    => PRD.Parser a m b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseMany = Drivers.parseMany

-- | Like 'parseMany' but includes stream position information in the error
-- messages.
--
{-# INLINE parseManyPos #-}
parseManyPos
    :: Monad m
    => PRD.Parser a m b
    -> Stream m a
    -> Stream m (Either ParseErrorPos b)
parseManyPos = Drivers.parseManyPos

{-# DEPRECATED parseManyD "Please use parseMany instead." #-}
{-# INLINE parseManyD #-}
parseManyD
    :: Monad m
    => PR.Parser a m b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseManyD = parseMany

-- | Apply a stream of parsers to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE parseSequence #-}
parseSequence
       :: -- Monad m =>
       Stream m (PR.Parser a m b)
    -> Stream m a
    -> Stream m b
parseSequence _f _m = undefined

-- XXX Change the parser arguments' order

-- | @parseManyTill collect test stream@ tries the parser @test@ on the input,
-- if @test@ fails it backtracks and tries @collect@, after @collect@ succeeds
-- @test@ is tried again and so on. The parser stops when @test@ succeeds.  The
-- output of @test@ is discarded and the output of @collect@ is emitted in the
-- output stream. The parser fails if @collect@ fails.
--
-- /Unimplemented/
--
{-# INLINE parseManyTill #-}
parseManyTill ::
    -- MonadThrow m =>
       PR.Parser a m b
    -> PR.Parser a m x
    -> Stream m a
    -> Stream m b
parseManyTill = undefined

-- | Iterate a parser generating function on a stream. The initial value @b@ is
-- used to generate the first parser, the parser is applied on the stream and
-- the result is used to generate the next parser and so on.
--
-- Example:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> s = Stream.fromList [1..10]
-- >>> Stream.toList $ fmap getSum $ Stream.catRights $ Stream.parseIterate (\b -> Parser.takeBetween 0 2 (Fold.sconcat b)) (Sum 0) $ fmap Sum s
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Pre-release/
--
{-# INLINE parseIterate #-}
parseIterate
    :: Monad m
    => (b -> PRD.Parser a m b)
    -> b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseIterate = Drivers.parseIterate

-- | Like 'parseIterate' but includes stream position information in the error
-- messages.
--
{-# INLINE parseIteratePos #-}
parseIteratePos
    :: Monad m
    => (b -> PRD.Parser a m b)
    -> b
    -> Stream m a
    -> Stream m (Either ParseErrorPos b)
parseIteratePos = Drivers.parseIteratePos

{-# DEPRECATED parseIterateD "Please use parseIterate instead." #-}
{-# INLINE parseIterateD #-}
parseIterateD
    :: Monad m
    => (b -> PR.Parser a m b)
    -> b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseIterateD = parseIterate

------------------------------------------------------------------------------
-- Grouping
------------------------------------------------------------------------------

data GroupByState st fs a b
    = GroupingInit st
    | GroupingDo st !fs
    | GroupingInitWith st !a
    | GroupingDoWith st !fs !a
    | GroupingYield !b (GroupByState st fs a b)
    | GroupingDone

-- | Keep collecting items in a group as long as the comparison function
-- returns true. The comparison function is @cmp old new@ where @old@ is the
-- first item in the group and @new@ is the incoming item being tested for
-- membership of the group. The collected items are folded by the supplied
-- fold.
--
-- Definition:
--
-- >>> groupsWhile cmp f = Stream.parseMany (Parser.groupBy cmp f)
{-# INLINE_NORMAL groupsWhile #-}
groupsWhile :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
{-
groupsWhile eq fld = parseMany (PRD.groupBy eq fld)
-}
groupsWhile cmp (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter (GroupingInit state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (GroupingInit st) = do
        -- XXX Note that if the stream stops without yielding a single element
        -- in the group we discard the "initial" effect.
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ GroupingDo st s
                  FL.Done b -> Yield b $ GroupingInit st
    stepOuter gst (GroupingDo st fs) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> go SPEC x s fs1
                    FL.Done b -> return $ Yield b (GroupingInit s)
            Skip s -> return $ Skip $ GroupingDo s fs
            Stop -> final fs >> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC prev s fs1
                            FL.Done b -> return $ Yield b (GroupingInit s)
                    else do
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                Skip s -> go SPEC prev s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingInitWith st x) = do
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ GroupingDoWith st s x
                  FL.Done b -> Yield b $ GroupingInitWith st x
    stepOuter gst (GroupingDoWith st fs prev) = do
        res <- fstep fs prev
        case res of
            FL.Partial fs1 -> go SPEC st fs1
            FL.Done b -> return $ Yield b (GroupingInit st)

        where

        -- XXX code duplicated from the previous equation
        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC s fs1
                            FL.Done b -> return $ Yield b (GroupingInit s)
                    else do
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                Skip s -> go SPEC s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingYield _ _) = error "groupsWhile: Unreachable"
    stepOuter _ GroupingDone = return Stop

-- | The argument order of the comparison function in `groupsWhile` is
-- different than that of `groupsBy`.
--
-- In `groupsBy` the comparison function takes the next element as the first
-- argument and the previous element as the second argument. In `groupsWhile`
-- the first argument is the previous element and second argument is the next
-- element.
{-# DEPRECATED groupsBy "Please use groupsWhile instead. Please note the change in the argument order of the comparison function." #-}
{-# INLINE_NORMAL groupsBy #-}
groupsBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsBy cmp = groupsWhile (flip cmp)

-- |
--
-- Definition:
--
-- >>> groupsRollingBy cmp f = Stream.parseMany (Parser.groupByRolling cmp f)
--
{-# INLINE_NORMAL groupsRollingBy #-}
groupsRollingBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
{-
groupsRollingBy eq fld = parseMany (PRD.groupByRolling eq fld)
-}
groupsRollingBy cmp (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter (GroupingInit state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (GroupingInit st) = do
        -- XXX Note that if the stream stops without yielding a single element
        -- in the group we discard the "initial" effect.
        res <- initial
        return
            $ case res of
                  FL.Partial fs -> Skip $ GroupingDo st fs
                  FL.Done fb -> Yield fb $ GroupingInit st
    stepOuter gst (GroupingDo st fs) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> go SPEC x s fs1
                    FL.Done fb -> return $ Yield fb (GroupingInit s)
            Skip s -> return $ Skip $ GroupingDo s fs
            Stop -> final fs >> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC x s fs1
                            FL.Done b -> return $ Yield b (GroupingInit s)
                    else do
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                Skip s -> go SPEC prev s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingInitWith st x) = do
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ GroupingDoWith st s x
                  FL.Done b -> Yield b $ GroupingInitWith st x
    stepOuter gst (GroupingDoWith st fs previous) = do
        res <- fstep fs previous
        case res of
            FL.Partial s -> go SPEC previous st s
            FL.Done b -> return $ Yield b (GroupingInit st)

        where

        -- XXX GHC: groupsWhile has one less parameter in this go loop and it
        -- fuses. However, groupsRollingBy does not fuse, removing the prev
        -- parameter makes it fuse. Something needs to be fixed in GHC. The
        -- workaround for this is noted in the comments below.
        go !_ prev !stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC x s fs1
                            FL.Done b -> return $ Yield b (GroupingInit st)
                    else do
                        {-
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                        -}
                        -- The code above does not let groupBy fuse. We use the
                        -- alternative code below instead.  Instead of jumping
                        -- to GroupingInitWith state, we unroll the code of
                        -- GroupingInitWith state here to help GHC with stream
                        -- fusion.
                        result <- initial
                        r <- final acc
                        return
                            $ Yield r
                            $ case result of
                                  FL.Partial fsi -> GroupingDoWith s fsi x
                                  FL.Done b -> GroupingYield b (GroupingInit s)
                Skip s -> go SPEC prev s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingYield r next) = return $ Yield r next
    stepOuter _ GroupingDone = return Stop

------------------------------------------------------------------------------
-- Splitting - by a predicate
------------------------------------------------------------------------------

data WordsByState st fs b
    = WordsByInit st
    | WordsByDo st !fs
    | WordsByDone
    | WordsByYield !b (WordsByState st fs b)

-- | Split the stream after stripping leading, trailing, and repeated
-- separators determined by the predicate supplied. The tokens after splitting
-- are collected by the supplied fold. In other words, the tokens are parsed in
-- the same way as words are parsed from whitespace separated text.
--
-- >>> f x = Stream.toList $ Stream.wordsBy (== '.') Fold.toList $ Stream.fromList x
-- >>> f "a.b"
-- ["a","b"]
-- >>> f "a..b"
-- ["a","b"]
-- >>> f ".a..b."
-- ["a","b"]
--
{-# INLINE_NORMAL wordsBy #-}
wordsBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
wordsBy predicate (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter (WordsByInit state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (WordsByInit st) = do
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ WordsByDo st s
                  FL.Done b -> Yield b (WordsByInit st)

    stepOuter gst (WordsByDo st fs) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                if predicate x
                then do
                    resi <- initial
                    return
                        $ case resi of
                              FL.Partial fs1 -> Skip $ WordsByDo s fs1
                              FL.Done b -> Yield b (WordsByInit s)
                else do
                    r <- fstep fs x
                    case r of
                        FL.Partial fs1 -> go SPEC s fs1
                        FL.Done b -> return $ Yield b (WordsByInit s)
            Skip s    -> return $ Skip $ WordsByDo s fs
            Stop      -> final fs >> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then do
                        {-
                        r <- final acc
                        return $ Yield r (WordsByInit s)
                        -}
                        -- The above code does not fuse well. Need to check why
                        -- GHC is not able to simplify it well.  Using the code
                        -- below, instead of jumping through the WordsByInit
                        -- state always, we directly go to WordsByDo state in
                        -- the common case of Partial.
                        resi <- initial
                        r <- final acc
                        return
                            $ Yield r
                            $ case resi of
                                  FL.Partial fs1 -> WordsByDo s fs1
                                  FL.Done b -> WordsByYield b (WordsByInit s)
                    else do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC s fs1
                            FL.Done b -> return $ Yield b (WordsByInit s)
                Skip s -> go SPEC s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r WordsByDone

    stepOuter _ WordsByDone = return Stop

    stepOuter _ (WordsByYield b next) = return $ Yield b next

------------------------------------------------------------------------------
-- Splitting on a sequence
------------------------------------------------------------------------------

-- String search algorithms:
-- http://www-igm.univ-mlv.fr/~lecroq/string/index.html

-- XXX Can GHC find a way to modularise this? Can we write different cases
-- i.e.g single element, word hash, karp-rabin as different functions and then
-- be able to combine them into a single state machine?

{-# ANN type TakeEndBySeqState Fuse #-}
data TakeEndBySeqState mba rb rh ck w s b x =
      TakeEndBySeqInit
    | TakeEndBySeqYield !b (TakeEndBySeqState mba rb rh ck w s b x)
    | TakeEndBySeqDone

    | TakeEndBySeqSingle s x

    | TakeEndBySeqWordInit !Int !w s
    | TakeEndBySeqWordLoop !w s
    | TakeEndBySeqWordDone !Int !w

    | TakeEndBySeqKRInit s mba
    | TakeEndBySeqKRInit1 s mba !Int
    | TakeEndBySeqKRLoop s mba !rh !ck
    | TakeEndBySeqKRCheck s mba !rh
    | TakeEndBySeqKRDone !Int rb

-- | If the pattern is empty the output stream is empty.
{-# INLINE_NORMAL takeEndBySeqWith #-}
takeEndBySeqWith
    :: forall m a. (MonadIO m, Unbox a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Stream m a
    -> Stream m a
takeEndBySeqWith withSep patArr (Stream step state) =
    Stream stepOuter TakeEndBySeqInit

    where

    patLen = A.length patArr
    patBytes = A.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)
    elemBits = SIZE_OF(a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    {-# INLINE yield #-}
    yield x !s = skip $ TakeEndBySeqYield x s

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ TakeEndBySeqInit = do
        -- XXX When we statically specify the method compiler is able to
        -- simplify the code better and removes the handling of other states.
        -- When it is determined dynamically, the code is less efficient. For
        -- example, the single element search degrades by 80% if the handling
        -- of other cases is present. We need to investigate this further but
        -- until then we can guide the compiler statically where we can. If we
        -- want to use single element search statically then we can use
        -- takeEndBy instead.
        --
        -- XXX Is there a way for GHC to statically determine patLen when we
        -- use an array created from a static string as pattern e.g. "\n".
        case () of
            _ | patLen == 0 -> return Stop
              | patLen == 1 -> do
                    pat <- liftIO $ A.unsafeGetIndexIO 0 patArr
                    return $ Skip $ TakeEndBySeqSingle state pat
              | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    return $ Skip $ TakeEndBySeqWordInit 0 0 state
              | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MutArray.emptyOf patLen
                    skip $ TakeEndBySeqKRInit state mba

    ---------------------
    -- Single yield point
    ---------------------

    stepOuter _ (TakeEndBySeqYield x next) = return $ Yield x next

    -----------------
    -- Done
    -----------------

    stepOuter _ TakeEndBySeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (TakeEndBySeqSingle st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s ->
                if pat /= x
                then yield x (TakeEndBySeqSingle s pat)
                else do
                    if withSep
                    then yield x TakeEndBySeqDone
                    else return Stop
            Skip s -> skip $ TakeEndBySeqSingle s pat
            Stop -> return Stop

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    -- Note: Karp-Rabin is roughly 15% slower than word hash for a 2 element
    -- pattern. This may be useful for common cases like splitting lines using
    -- "\r\n".
    stepOuter _ (TakeEndBySeqWordDone 0 _) = do
        return Stop
    stepOuter _ (TakeEndBySeqWordDone n wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
         in yield
                (toEnum $ fromIntegral old)
                (TakeEndBySeqWordDone (n - 1) wrd)

    -- XXX If we remove this init state for perf experiment the time taken
    -- reduces to half, there may be some optimization opportunity here.
    stepOuter gst (TakeEndBySeqWordInit idx wrd st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                let wrd1 = addToWord wrd x
                    next
                      | idx /= maxIndex =
                            TakeEndBySeqWordInit (idx + 1) wrd1 s
                      | wrd1 .&. wordMask /= wordPat =
                            TakeEndBySeqWordLoop wrd1 s
                      | otherwise = TakeEndBySeqDone
                if withSep
                then yield x next
                else skip next
            Skip s -> skip $ TakeEndBySeqWordInit idx wrd s
            Stop ->
                if withSep
                then return Stop
                else skip $ TakeEndBySeqWordDone idx wrd

    stepOuter gst (TakeEndBySeqWordLoop wrd st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                -- XXX Never use a lazy expression as state, that causes issues
                -- in simplification because the state argument of Yield is
                -- lazy, maybe we can make that strict.
                let wrd1 = addToWord wrd x
                    old = (wordMask .&. wrd)
                            `shiftR` (elemBits * (patLen - 1))
                    !y =
                            if withSep
                            then x
                            else toEnum $ fromIntegral old
                -- Note: changing the nesting order of if and yield makes a
                -- difference in performance.
                if wrd1 .&. wordMask /= wordPat
                then yield y (TakeEndBySeqWordLoop wrd1 s)
                else yield y TakeEndBySeqDone
            Skip s -> skip $ TakeEndBySeqWordLoop wrd s
            Stop ->
                 if withSep
                 then return Stop
                 else skip $ TakeEndBySeqWordDone patLen wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (TakeEndBySeqKRInit st0 mba) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                liftIO $ pokeAt 0 mba x
                if withSep
                then yield x (TakeEndBySeqKRInit1 s mba (SIZE_OF(a)))
                else skip $ TakeEndBySeqKRInit1 s mba (SIZE_OF(a))
            Skip s -> skip $ TakeEndBySeqKRInit s mba
            Stop -> return Stop

    stepOuter gst (TakeEndBySeqKRInit1 st mba offset) = do
        res <- step (adaptState gst) st
        let arr :: Array a = Array
                    { arrContents = mba
                    , arrStart = 0
                    , arrEnd = patBytes
                    }
        case res of
            Yield x s -> do
                liftIO $ pokeAt offset mba x
                let next =
                        if offset /= maxOffset
                        then TakeEndBySeqKRInit1 s mba (offset + SIZE_OF(a))
                        else
                            let ringHash = A.foldl' addCksum 0 arr
                             in if ringHash == patHash
                                then TakeEndBySeqKRCheck s mba 0
                                else TakeEndBySeqKRLoop s mba 0 ringHash
                if withSep
                then yield x next
                else skip next
            Skip s -> skip $ TakeEndBySeqKRInit1 s mba offset
            Stop -> do
                if withSep
                then return Stop
                else do
                    let rb = RingArray
                            { ringContents = mba
                            , ringSize = offset
                            , ringHead = 0
                            }
                     in skip $ TakeEndBySeqKRDone offset rb

    stepOuter gst (TakeEndBySeqKRLoop st mba rh cksum) = do
        res <- step (adaptState gst) st
        let rb = RingArray
                { ringContents = mba
                , ringSize = patBytes
                , ringHead = rh
                }
        case res of
            Yield x s -> do
                (rb1, old) <- liftIO (RB.replace rb x)
                let cksum1 = deltaCksum cksum old x
                let rh1 = ringHead rb1
                    next =
                        if cksum1 /= patHash
                        then TakeEndBySeqKRLoop s mba rh1 cksum1
                        else TakeEndBySeqKRCheck s mba rh1
                if withSep
                then yield x next
                else yield old next
            Skip s -> skip $ TakeEndBySeqKRLoop s mba rh cksum
            Stop -> do
                if withSep
                then return Stop
                else skip $ TakeEndBySeqKRDone patBytes rb

    stepOuter _ (TakeEndBySeqKRCheck st mba rh) = do
        let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
        matches <- liftIO $ RB.eqArray rb patArr
        if matches
        then return Stop
        else skip $ TakeEndBySeqKRLoop st mba rh patHash

    stepOuter _ (TakeEndBySeqKRDone 0 _) = return Stop
    stepOuter _ (TakeEndBySeqKRDone len rb) = do
        assert (len >= 0) (return ())
        old <- RB.unsafeGetHead rb
        let rb1 = RB.moveForward rb
        yield old $ TakeEndBySeqKRDone (len - SIZE_OF(a)) rb1

-- | Take the stream until the supplied sequence is encountered. Take the
-- sequence as well and stop.
--
-- Usage:
--
-- >>> f pat xs = Stream.toList $ Stream.takeEndBySeq (Array.fromList pat) $ Stream.fromList xs
-- >>> f "fgh" "abcdefghijk"
-- "abcdefgh"
-- >>> f "lmn" "abcdefghijk"
-- "abcdefghijk"
-- >>> f "" "abcdefghijk"
-- ""
--
{-# INLINE takeEndBySeq #-}
takeEndBySeq
    :: forall m a. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Stream m a
    -> Stream m a
takeEndBySeq = takeEndBySeqWith True

-- | Take the stream until the supplied sequence is encountered. Do not take
-- the sequence.
--
-- Usage:
--
-- >>> f pat xs = Stream.toList $ Stream.takeEndBySeq_ (Array.fromList pat) $ Stream.fromList xs
-- >>> f "fgh" "abcdefghijk"
-- "abcde"
-- >>> f "lmn" "abcdefghijk"
-- "abcdefghijk"
-- >>> f "" "abcdefghijk"
-- ""
--
{-# INLINE takeEndBySeq_ #-}
takeEndBySeq_
    :: forall m a. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Stream m a
    -> Stream m a
takeEndBySeq_ = takeEndBySeqWith False

{-
-- TODO can we unify the splitting operations using a splitting configuration
-- like in the split package.
--
data SplitStyle = Infix | Suffix | Prefix deriving (Eq, Show)
data SplitOptions = SplitOptions
    { style    :: SplitStyle
    , withSep  :: Bool  -- ^ keep the separators in output
    -- , compact  :: Bool  -- ^ treat multiple consecutive separators as one
    -- , trimHead :: Bool  -- ^ drop blank at head
    -- , trimTail :: Bool  -- ^ drop blank at tail
    }
-}

-- XXX using "fs" as the last arg in Constructors may simplify the code a bit,
-- because we can use the constructor directly without having to create "jump"
-- functions.
{-# ANN type SplitOnSeqState Fuse #-}
data SplitOnSeqState mba rb rh ck w fs s b x =
      SplitOnSeqInit
    | SplitOnSeqYield b (SplitOnSeqState mba rb rh ck w fs s b x)
    | SplitOnSeqDone

    | SplitOnSeqEmpty !fs s

    | SplitOnSeqSingle0 !fs s x
    | SplitOnSeqSingle !fs s x

    | SplitOnSeqWordInit0 !fs s
    | SplitOnSeqWordInit Int Word !fs s
    | SplitOnSeqWordLoop !w s !fs
    | SplitOnSeqWordDone Int !fs !w

    | SplitOnSeqKRInit0 Int !fs s mba
    | SplitOnSeqKRInit Int !fs s mba
    | SplitOnSeqKRLoop fs s mba !rh !ck
    | SplitOnSeqKRCheck fs s mba !rh
    | SplitOnSeqKRDone Int !fs rb

    | SplitOnSeqReinit (fs -> SplitOnSeqState mba rb rh ck w fs s b x)

-- XXX Need to fix empty stream split behavior

-- | Like 'splitSepBy_' but splits the stream on a sequence of elements rather than
-- a single element. Parses a sequence of tokens separated by an infixed
-- separator e.g. @a;b;c@ is parsed as @a@, @b@, @c@. If the pattern is empty
-- then each element is a match, thus the fold is finalized on each element.
--
-- >>> splitSepBy p xs = Stream.fold Fold.toList $ Stream.splitSepBySeq_ (Array.fromList p) Fold.toList (Stream.fromList xs)
--
-- >>> splitSepBy "" ""
-- []
--
-- >>> splitSepBy "" "a...b"
-- ["a",".",".",".","b"]
--
-- >>> splitSepBy ".." ""
-- []
--
-- >>> splitSepBy ".." "a...b"
-- ["a",".b"]
--
-- >>> splitSepBy ".." "abc"
-- ["abc"]
--
-- >>> splitSepBy ".." ".."
-- ["",""]
--
-- >>> splitSepBy "." ".a"
-- ["","a"]
--
-- >>> splitSepBy "." "a."
-- ["a",""]
--
-- Uses Rabin-Karp algorithm for substring search.
--
{-# INLINE_NORMAL splitSepBySeq_ #-}
splitSepBySeq_, splitOnSeq
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitSepBySeq_ patArr (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter SplitOnSeqInit

    where

    patLen = A.length patArr
    patBytes = A.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)
    elemBits = SIZE_OF(a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    nextAfterInit nextGen stepRes =
        case stepRes of
            FL.Partial s -> nextGen s
            FL.Done b -> SplitOnSeqYield b (SplitOnSeqReinit nextGen)

    {-# INLINE yieldReinit #-}
    yieldReinit nextGen fs =
        initial >>= skip . SplitOnSeqYield fs . nextAfterInit nextGen

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ SplitOnSeqInit = do
        res <- initial
        case res of
            FL.Partial acc
                | patLen == 0 ->
                    return $ Skip $ SplitOnSeqEmpty acc state
                | patLen == 1 -> do
                    pat <- liftIO $ A.unsafeGetIndexIO 0 patArr
                    return $ Skip $ SplitOnSeqSingle0 acc state pat
                | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    return $ Skip $ SplitOnSeqWordInit0 acc state
                | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MutArray.emptyOf patLen
                    skip $ SplitOnSeqKRInit0 0 acc state mba
            FL.Done b -> skip $ SplitOnSeqYield b SplitOnSeqInit

    stepOuter _ (SplitOnSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Checkpoint
    ---------------------------

    stepOuter _ (SplitOnSeqReinit nextGen) =
        initial >>= skip . nextAfterInit nextGen

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSeqEmpty acc st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep acc x
                b1 <-
                    case r of
                        FL.Partial acc1 -> final acc1
                        FL.Done b -> return b
                let jump c = SplitOnSeqEmpty c s
                 in yieldReinit jump b1
            Skip s -> skip (SplitOnSeqEmpty acc s)
            Stop -> final acc >> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSeqSingle0 fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                -- XXX This code block is duplicated in SplitOnSeqSingle state
                let jump c = SplitOnSeqSingle c s pat
                if pat == x
                then final fs >>= yieldReinit jump
                else do
                    r <- fstep fs x
                    case r of
                        FL.Partial fs1 ->
                            pure $ Skip $ SplitOnSeqSingle fs1 s pat
                        FL.Done b -> yieldReinit jump b
            Skip s -> pure $ Skip $ SplitOnSeqSingle0 fs s pat
            Stop -> final fs >> pure Stop

    stepOuter gst (SplitOnSeqSingle fs0 st0 pat) = do
        go SPEC fs0 st0

        where

        -- The local loop increases allocations by 6% but improves CPU
        -- performance by 14%.
        go !_ !fs !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSeqSingle c s pat
                    if pat == x
                    then final fs >>= yieldReinit jump
                    else do
                        r <- fstep fs x
                        case r of
                            FL.Partial fs1 -> go SPEC fs1 s
                            FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC fs s
                Stop -> do
                    r <- final fs
                    return $ Skip $ SplitOnSeqYield r SplitOnSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    -- Note: We fill the matching buffer before we emit anything, in case it
    -- matches and we have to drop it. Though we could be more eager in
    -- emitting as soon as we know that the pattern cannot match. But still the
    -- worst case will remain the same, in case a match is going to happen we
    -- will have to delay until the very end.

    stepOuter _ (SplitOnSeqWordDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                 let jump c = SplitOnSeqWordDone (n - 1) c wrd
                 yieldReinit jump b

    stepOuter gst (SplitOnSeqWordInit0 fs st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s ->
                let wrd1 = addToWord 0 x
                 in pure $ Skip $ SplitOnSeqWordInit 1 wrd1 fs s
            Skip s -> pure $ Skip $ SplitOnSeqWordInit0 fs s
            Stop -> final fs >> pure Stop

    stepOuter gst (SplitOnSeqWordInit idx0 wrd0 fs st0) =
        go SPEC idx0 wrd0 st0

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                    if idx == maxIndex
                    then do
                        if wrd1 .&. wordMask == wordPat
                        then do
                            let jump c = SplitOnSeqWordInit 0 0 c s
                            final fs >>= yieldReinit jump
                        else skip $ SplitOnSeqWordLoop wrd1 s fs
                    else go SPEC (idx + 1) wrd1 s
                Skip s -> go SPEC idx wrd s
                Stop -> do
                    if idx /= 0
                    then skip $ SplitOnSeqWordDone idx fs wrd
                    else do
                        r <- final fs
                        skip $ SplitOnSeqYield r SplitOnSeqDone

    stepOuter gst (SplitOnSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        -- This loop does not affect allocations but it improves the CPU
        -- performance signifcantly compared to looping using state.
        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSeqWordInit 0 0 c s
                        wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <- fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 -> do
                            if wrd1 .&. wordMask == wordPat
                            then final fs1 >>= yieldReinit jump
                            else go SPEC wrd1 s fs1
                        FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC wrd s fs
                Stop -> skip $ SplitOnSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    -- XXX Document this pattern for writing efficient code. Loop around only
    -- required elements in the recursive loop, build the structures being
    -- manipulated locally e.g. we are passing only mba, here and build an
    -- array using patLen and arrStart from the surrounding context.

    stepOuter gst (SplitOnSeqKRInit0 offset fs st mba) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                liftIO $ pokeAt offset mba x
                skip $ SplitOnSeqKRInit (offset + SIZE_OF(a)) fs s mba
            Skip s -> skip $ SplitOnSeqKRInit0 offset fs s mba
            Stop -> final fs >> pure Stop

    stepOuter gst (SplitOnSeqKRInit offset fs st mba) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                liftIO $ pokeAt offset mba x
                if offset == maxOffset
                then do
                    let arr :: Array a = Array
                                { arrContents = mba
                                , arrStart = 0
                                , arrEnd = patBytes
                                }
                    let ringHash = A.foldl' addCksum 0 arr
                    if ringHash == patHash && A.byteEq arr patArr
                    then skip $ SplitOnSeqKRCheck fs s mba 0
                    else skip $ SplitOnSeqKRLoop fs s mba 0 ringHash
                else skip $ SplitOnSeqKRInit (offset + SIZE_OF(a)) fs s mba
            Skip s -> skip $ SplitOnSeqKRInit offset fs s mba
            Stop -> do
                let rb = RingArray
                        { ringContents = mba
                        , ringSize = offset
                        , ringHead = 0
                        }
                skip $ SplitOnSeqKRDone offset fs rb

    -- XXX The recursive "go" is more efficient than the state based recursion
    -- code commented out below. Perhaps its more efficient because of
    -- factoring out "mba" outside the loop.
    --
    stepOuter gst (SplitOnSeqKRLoop fs0 st0 mba rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
            case res of
                Yield x s -> do
                    (rb1, old) <- liftIO (RB.replace rb x)
                    r <- fstep fs old
                    case r of
                        FL.Partial fs1 -> do
                            let cksum1 = deltaCksum cksum old x
                            let rh1 = ringHead rb1
                            if cksum1 == patHash
                            then skip $ SplitOnSeqKRCheck fs1 s mba rh1
                            else go SPEC fs1 s rh1 cksum1
                        FL.Done b -> do
                            -- XXX the old code looks wrong as we are resetting
                            -- the ring head but the ring still has old
                            -- elements as we are not resetting the size.
                            let jump c = SplitOnSeqKRInit 0 c s mba
                            yieldReinit jump b
                Skip s -> go SPEC fs s rh cksum
                Stop -> skip $ SplitOnSeqKRDone patBytes fs rb

    -- XXX The following code is 5 times slower compared to the recursive loop
    -- based code above. Need to investigate why. One possibility is that the
    -- go loop above does not thread around the ring buffer (rb). This code may
    -- be causing the state to bloat and getting allocated on each iteration.
    -- We can check the cmm/asm code to confirm.  If so a good GHC solution to
    -- such problem is needed. One way to avoid this could be to use unboxed
    -- mutable state?
    {-
    stepOuter gst (SplitOnSeqKRLoop fs st rb rh cksum) = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum1 = deltaCksum cksum old x
                    fs1 <- fstep fs old
                    if (cksum1 == patHash)
                    then do
                        r <- done fs1
                        skip $ SplitOnSeqYield r $ SplitOnSeqKRInit 0 s rb rh
                    else do
                        rh1 <- liftIO (RB.unsafeInsert rb rh x)
                        skip $ SplitOnSeqKRLoop fs1 s rb rh1 cksum1
                Skip s -> skip $ SplitOnSeqKRLoop fs s rb rh cksum
                Stop -> skip $ SplitOnSeqKRDone patLen fs rb rh
    -}

    stepOuter _ (SplitOnSeqKRCheck fs st mba rh) = do
        let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
        res <- liftIO $ RB.eqArray rb patArr
        if res
        then do
            r <- final fs
            let jump c = SplitOnSeqKRInit 0 c st mba
            yieldReinit jump r
        else skip $ SplitOnSeqKRLoop fs st mba rh patHash

    stepOuter _ (SplitOnSeqKRDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqKRDone len fs rb) = do
        assert (len >= 0) (return ())
        old <- RB.unsafeGetHead rb
        let rb1 = RB.moveForward rb
        r <- fstep fs old
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqKRDone (len - SIZE_OF(a)) fs1 rb1
            FL.Done b -> do
                 let jump c = SplitOnSeqKRDone (len - SIZE_OF(a)) c rb1
                 yieldReinit jump b

RENAME(splitOnSeq,splitSepBySeq_)

{-# ANN type SplitOnSuffixSeqState Fuse #-}
data SplitOnSuffixSeqState mba rb rh ck w fs s b x =
      SplitOnSuffixSeqInit
    | SplitOnSuffixSeqYield b (SplitOnSuffixSeqState mba rb rh ck w fs s b x)
    | SplitOnSuffixSeqDone

    | SplitOnSuffixSeqEmpty !fs s

    | SplitOnSuffixSeqSingleInit !fs s x
    | SplitOnSuffixSeqSingle !fs s x

    | SplitOnSuffixSeqWordInit !fs s
    | SplitOnSuffixSeqWordLoop !w s !fs
    | SplitOnSuffixSeqWordDone Int !fs !w

    | SplitOnSuffixSeqKRInit !fs s mba
    | SplitOnSuffixSeqKRInit1 !fs s mba
    | SplitOnSuffixSeqKRLoop fs s mba !rh !ck
    | SplitOnSuffixSeqKRCheck fs s mba !rh
    | SplitOnSuffixSeqKRDone Int !fs rb

    | SplitOnSuffixSeqReinit
          (fs -> SplitOnSuffixSeqState mba rb rh ck w fs s b x)

-- | @splitOnSuffixSeq withSep pat fld input@ splits the input using @pat@ as a
-- suffixed separator, the resulting split segments are fed to the fold @fld@.
-- If @withSep@ is True then the separator sequence is also suffixed with the
-- split segments.
--
-- /Internal/
{-# INLINE_NORMAL splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitOnSuffixSeq withSep patArr (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter SplitOnSuffixSeqInit

    where

    patLen = A.length patArr
    patBytes = A.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)
    elemBits = SIZE_OF(a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    nextAfterInit nextGen stepRes =
        case stepRes of
            FL.Partial s -> nextGen s
            FL.Done b ->
                SplitOnSuffixSeqYield b (SplitOnSuffixSeqReinit nextGen)

    {-# INLINE yieldReinit #-}
    yieldReinit nextGen fs =
        initial >>= skip . SplitOnSuffixSeqYield fs . nextAfterInit nextGen

    -- For single element pattern case
    {-# INLINE processYieldSingle #-}
    processYieldSingle pat x s fs = do
        let jump c = SplitOnSuffixSeqSingleInit c s pat
        if pat == x
        then do
            r <- if withSep then fstep fs x else return $ FL.Partial fs
            b1 <-
                case r of
                    FL.Partial fs1 -> final fs1
                    FL.Done b -> return b
            yieldReinit jump b1
        else do
            r <- fstep fs x
            case r of
                FL.Partial fs1 -> skip $ SplitOnSuffixSeqSingle fs1 s pat
                FL.Done b -> yieldReinit jump b

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ SplitOnSuffixSeqInit = do
        res <- initial
        case res of
            FL.Partial fs
                | patLen == 0 ->
                    skip $ SplitOnSuffixSeqEmpty fs state
                | patLen == 1 -> do
                    pat <- liftIO $ A.unsafeGetIndexIO 0 patArr
                    skip $ SplitOnSuffixSeqSingleInit fs state pat
                | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    skip $ SplitOnSuffixSeqWordInit fs state
                | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MutArray.emptyOf patLen
                    skip $ SplitOnSuffixSeqKRInit fs state mba
            FL.Done fb -> skip $ SplitOnSuffixSeqYield fb SplitOnSuffixSeqInit

    stepOuter _ (SplitOnSuffixSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Reinit
    ---------------------------

    stepOuter _ (SplitOnSuffixSeqReinit nextGen) =
        initial >>= skip . nextAfterInit nextGen

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSuffixSeqEmpty acc st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                let jump c = SplitOnSuffixSeqEmpty c s
                r <- fstep acc x
                b1 <-
                    case r of
                        FL.Partial fs -> final fs
                        FL.Done b -> return b
                yieldReinit jump b1
            Skip s -> skip (SplitOnSuffixSeqEmpty acc s)
            Stop -> final acc >> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSuffixSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSuffixSeqSingleInit fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> processYieldSingle pat x s fs
            Skip s -> skip $ SplitOnSuffixSeqSingleInit fs s pat
            Stop -> final fs >> return Stop

    stepOuter gst (SplitOnSuffixSeqSingle fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> processYieldSingle pat x s fs
            Skip s -> skip $ SplitOnSuffixSeqSingle fs s pat
            Stop -> do
                r <- final fs
                skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    stepOuter _ (SplitOnSuffixSeqWordDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
    stepOuter _ (SplitOnSuffixSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSuffixSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                let jump c = SplitOnSuffixSeqWordDone (n - 1) c wrd
                yieldReinit jump b

    stepOuter gst (SplitOnSuffixSeqWordInit fs0 st0) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                let wrd = addToWord 0 x
                r <- if withSep then fstep fs0 x else return $ FL.Partial fs0
                case r of
                    FL.Partial fs1 -> go SPEC 1 wrd s fs1
                    FL.Done b -> do
                        let jump c = SplitOnSuffixSeqWordInit c s
                        yieldReinit jump b
            Skip s -> skip (SplitOnSuffixSeqWordInit fs0 s)
            Stop -> final fs0 >> return Stop

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSuffixSeqWordInit c s
                    let wrd1 = addToWord wrd x
                    r <- if withSep then fstep fs x else return $ FL.Partial fs
                    case r of
                        FL.Partial fs1
                            | idx /= maxIndex ->
                                go SPEC (idx + 1) wrd1 s fs1
                            | wrd1 .&. wordMask /= wordPat ->
                                skip $ SplitOnSuffixSeqWordLoop wrd1 s fs1
                            | otherwise ->
                                final fs1 >>= yieldReinit jump
                        FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC idx wrd s fs
                Stop ->
                    if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqWordDone idx fs wrd

    stepOuter gst (SplitOnSuffixSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSuffixSeqWordInit c s
                        wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <-
                        if withSep
                        then fstep fs x
                        else fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 ->
                            if wrd1 .&. wordMask == wordPat
                            then final fs1 >>= yieldReinit jump
                            else go SPEC wrd1 s fs1
                        FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC wrd s fs
                Stop ->
                    if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (SplitOnSuffixSeqKRInit fs st0 mba) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                liftIO $ pokeAt 0 mba x
                r <- if withSep then fstep fs x else return $ FL.Partial fs
                case r of
                    FL.Partial fs1 ->
                        skip $ SplitOnSuffixSeqKRInit1 fs1 s mba
                    FL.Done b -> do
                        let jump c = SplitOnSuffixSeqKRInit c s mba
                        yieldReinit jump b
            Skip s -> skip $ SplitOnSuffixSeqKRInit fs s mba
            Stop -> final fs >> return Stop

    stepOuter gst (SplitOnSuffixSeqKRInit1 fs0 st0 mba) = do
        go SPEC (SIZE_OF(a)) st0 fs0

        where

        go !_ !offset st !fs = do
            res <- step (adaptState gst) st
            let arr :: Array a = Array
                        { arrContents = mba
                        , arrStart = 0
                        , arrEnd = patBytes
                        }
            case res of
                Yield x s -> do
                    liftIO $ pokeAt offset mba x
                    r <- if withSep then fstep fs x else return $ FL.Partial fs
                    let ringHash = A.foldl' addCksum 0 arr
                    case r of
                        FL.Partial fs1
                            | offset /= maxOffset ->
                                go SPEC (offset + SIZE_OF(a)) s fs1
                            | ringHash == patHash ->
                                skip $ SplitOnSuffixSeqKRCheck fs1 s mba 0
                            | otherwise ->
                                skip $ SplitOnSuffixSeqKRLoop
                                    fs1 s mba 0 ringHash
                        FL.Done b -> do
                            let jump c = SplitOnSuffixSeqKRInit c s mba
                            yieldReinit jump b
                Skip s -> go SPEC offset s fs
                Stop -> do
                    -- do not issue a blank segment when we end at pattern
                    if offset == maxOffset && A.byteEq arr patArr
                    then final fs >> return Stop
                    else if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else do
                        let rb = RingArray
                                { ringContents = mba
                                , ringSize = offset
                                , ringHead = 0
                                }
                         in skip $ SplitOnSuffixSeqKRDone offset fs rb

    stepOuter gst (SplitOnSuffixSeqKRLoop fs0 st0 mba rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
            case res of
                Yield x s -> do
                    (rb1, old) <- liftIO (RB.replace rb x)
                    let cksum1 = deltaCksum cksum old x
                    let rh1 = ringHead rb1
                    r <- if withSep then fstep fs x else fstep fs old
                    case r of
                        FL.Partial fs1 ->
                            if cksum1 /= patHash
                            then go SPEC fs1 s rh1 cksum1
                            else skip $ SplitOnSuffixSeqKRCheck fs1 s mba rh1
                        FL.Done b -> do
                            let jump c = SplitOnSuffixSeqKRInit c s mba
                            yieldReinit jump b
                Skip s -> go SPEC fs s rh cksum
                Stop -> do
                    if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqKRDone patBytes fs rb

    stepOuter _ (SplitOnSuffixSeqKRCheck fs st mba rh) = do
        let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
        matches <- liftIO $ RB.eqArray rb patArr
        if matches
        then do
            r <- final fs
            let jump c = SplitOnSuffixSeqKRInit c st mba
            yieldReinit jump r
        else skip $ SplitOnSuffixSeqKRLoop fs st mba rh patHash

    stepOuter _ (SplitOnSuffixSeqKRDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
    stepOuter _ (SplitOnSuffixSeqKRDone len fs rb) = do
        assert (len >= 0) (return ())
        old <- RB.unsafeGetHead rb
        let rb1 = RB.moveForward rb
        r <- fstep fs old
        case r of
            FL.Partial fs1 ->
                skip $ SplitOnSuffixSeqKRDone (len - SIZE_OF(a)) fs1 rb1
            FL.Done b -> do
                let jump c = SplitOnSuffixSeqKRDone (len - SIZE_OF(a)) c rb1
                yieldReinit jump b

-- | Parses a sequence of tokens suffixed by a separator e.g. @a;b;c;@ is
-- parsed as @a;@, @b;@, @c;@. If the pattern is empty the input stream is
-- returned as it is.
--
-- Equivalent to the following:
--
-- >>> splitEndBySeq pat f = Stream.foldMany (Fold.takeEndBySeq pat f)
--
-- Usage:
--
-- >>> f p = Stream.splitEndBySeq (Array.fromList p) Fold.toList
-- >>> splitEndBy p xs = Stream.fold Fold.toList $ f p (Stream.fromList xs)
--
-- >>> splitEndBy "" ""
-- []
--
-- >>> splitEndBy "" "a...b"
-- ["a",".",".",".","b"]
--
-- >>> splitEndBy ".." ""
-- []
--
--
-- >>> splitEndBy ".." "a...b"
-- ["a..",".b"]
--
--
-- >>> splitEndBy ".." "abc"
-- ["abc"]
--
-- >>> splitEndBy ".." ".."
-- [".."]
--
-- >>> splitEndBy "." ".a"
-- [".","a"]
--
-- >>> splitEndBy "." "a."
-- ["a."]
--
-- Uses Rabin-Karp algorithm for substring search.
--
{-# INLINE_NORMAL splitEndBySeq #-}
splitEndBySeq
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitEndBySeq = splitOnSuffixSeq True

-- | Like 'splitEndBySeq' but drops the separators and returns only the tokens.
--
-- Equivalent to the following:
--
-- >>> splitEndBySeq_ pat f = Stream.foldMany (Fold.takeEndBySeq_ pat f)
--
-- Usage:
--
-- >>> f p = Stream.splitEndBySeq_ (Array.fromList p) Fold.toList
-- >>> splitEndBy_ p xs = Stream.fold Fold.toList $ f p (Stream.fromList xs)
--
-- >>> splitEndBy_ "" ""
-- []
--
-- >>> splitEndBy_ "" "a...b"
-- ["a",".",".",".","b"]
--
-- >>> splitEndBy_ ".." ""
-- []
--
-- >>> splitEndBy_ ".." "a...b"
-- ["a",".b"]
--
-- >>> splitEndBy_ ".." "abc"
-- ["abc"]
--
-- >>> splitEndBy_ ".." ".."
-- [""]
--
-- >>> splitEndBy_ "." ".a"
-- ["","a"]
--
-- >>> splitEndBy_ "." "a."
-- ["a"]
--
-- Uses Rabin-Karp algorithm for substring search.
--
{-# INLINE_NORMAL splitEndBySeq_ #-}
splitEndBySeq_
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitEndBySeq_ = splitOnSuffixSeq False

-- Implement this as a fold or a parser instead.
-- This can be implemented easily using Rabin Karp

-- | Split post any one of the given patterns.
--
-- /Unimplemented/
{-# INLINE splitEndBySeqOneOf #-}
splitEndBySeqOneOf :: -- (Monad m, Unboxed a, Integral a) =>
    [Array a] -> Fold m a b -> Stream m a -> Stream m b
splitEndBySeqOneOf _subseq _f _m = undefined

-- | Split on a prefixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.
--
-- @
-- > splitOnPrefix' p xs = Stream.toList $ Stream.splitOnPrefix p (Fold.toList) (Stream.fromList xs)
-- > splitOnPrefix' (== '.') ".a.b"
-- ["a","b"]
-- @
--
-- An empty stream results in an empty output stream:
-- @
-- > splitOnPrefix' (== '.') ""
-- []
-- @
--
-- An empty segment consisting of only a prefix is folded to the default output
-- of the fold:
--
-- @
-- > splitOnPrefix' (== '.') "."
-- [""]
--
-- > splitOnPrefix' (== '.') ".a.b."
-- ["a","b",""]
--
-- > splitOnPrefix' (== '.') ".a..b"
-- ["a","","b"]
--
-- @
--
-- A prefix is optional at the beginning of the stream:
--
-- @
-- > splitOnPrefix' (== '.') "a"
-- ["a"]
--
-- > splitOnPrefix' (== '.') "a.b"
-- ["a","b"]
-- @
--
-- 'splitOnPrefix' is an inverse of 'intercalatePrefix' with a single element:
--
-- > Stream.intercalatePrefix (Stream.fromPure '.') Unfold.fromList . Stream.splitOnPrefix (== '.') Fold.toList === id
--
-- Assuming the input stream does not contain the separator:
--
-- > Stream.splitOnPrefix (== '.') Fold.toList . Stream.intercalatePrefix (Stream.fromPure '.') Unfold.fromList === id
--
-- /Unimplemented/
{-# INLINE splitBeginBy_ #-}
splitBeginBy_ :: -- (MonadCatch m) =>
    (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitBeginBy_ _predicate _f = undefined
    -- parseMany (Parser.sliceBeginBy predicate f)

-- Int list examples for splitOn:
--
-- >>> splitList [] [1,2,3,3,4]
-- > [[1],[2],[3],[3],[4]]
--
-- >>> splitList [5] [1,2,3,3,4]
-- > [[1,2,3,3,4]]
--
-- >>> splitList [1] [1,2,3,3,4]
-- > [[],[2,3,3,4]]
--
-- >>> splitList [4] [1,2,3,3,4]
-- > [[1,2,3,3],[]]
--
-- >>> splitList [2] [1,2,3,3,4]
-- > [[1],[3,3,4]]
--
-- >>> splitList [3] [1,2,3,3,4]
-- > [[1,2],[],[4]]
--
-- >>> splitList [3,3] [1,2,3,3,4]
-- > [[1,2],[4]]
--
-- >>> splitList [1,2,3,3,4] [1,2,3,3,4]
-- > [[],[]]

-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
--
-- /Unimplemented/
--
{-# INLINE splitSepBySeqOneOf #-}
splitSepBySeqOneOf :: -- (Monad m, Unboxed a, Integral a) =>
    [Array a] -> Fold m a b -> Stream m a -> Stream m b
splitSepBySeqOneOf _subseq _f _m =
    undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)

------------------------------------------------------------------------------
-- Nested Container Transformation
------------------------------------------------------------------------------

{-# ANN type SplitState Fuse #-}
data SplitState s arr
    = SplitInitial s
    | SplitBuffering s arr
    | SplitSplitting s arr
    | SplitYielding arr (SplitState s arr)
    | SplitFinishing

-- XXX An alternative approach would be to use a partial fold (Fold m a b) to
-- split using a splitBy like combinator. The Fold would consume upto the
-- separator and return any leftover which can then be fed to the next fold.
--
-- We can revisit this once we have partial folds/parsers.
--
-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBy #-}
splitInnerBy
    :: Monad m
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBy splitter joiner (Stream step1 state1) =
    Stream step (SplitInitial state1)

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop -> return $ Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return Stop

-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBySuffix #-}
splitInnerBySuffix
    :: Monad m
    => (f a -> Bool)                  -- isEmpty?
    -> (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBySuffix isEmpty splitter joiner (Stream step1 state1) =
    Stream step (SplitInitial state1)

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop ->
                return $
                    if isEmpty buf
                    then Stop
                    else Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return Stop

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Drop prefix from the input stream if present.
--
-- Space: @O(1)@
--
-- See also stripPrefix.
--
-- /Unimplemented/
{-# INLINE dropPrefix #-}
dropPrefix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropPrefix = error "Not implemented yet!"

-- | Drop all matching infix from the input stream if present. Infix stream
-- may be consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the infix.
--
-- See also stripInfix.
--
-- /Unimplemented/
{-# INLINE dropInfix #-}
dropInfix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropInfix = error "Not implemented yet!"

-- | Drop suffix from the input stream if present. Suffix stream may be
-- consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the suffix.
--
-- See also stripSuffix.
--
-- /Unimplemented/
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropSuffix = error "Not implemented yet!"
