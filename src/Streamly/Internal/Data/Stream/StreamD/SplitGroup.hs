#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.SplitGroup
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.SplitGroup
    (
    -- ** Grouping
      groupsOf2
    , groupsBy
    , groupsRollingBy

    -- ** Splitting
    , wordsBy

    , splitOnSeq
    , splitOnSuffixSeq

    , splitInnerBy
    , splitInnerBySuffix
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Word (Word32)
import Foreign.Storable (Storable(..))
import GHC.Types (SPEC(..))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Memory.Ring as RB

import Prelude hiding
       ( map, mapM, mapM_, repeat, foldr, last, take, filter
       , takeWhile, drop, dropWhile, all, any, maximum, minimum, elem
       , notElem, null, head, tail, zipWith, lookup, foldr1, sequence
       , (!!), scanl, scanl1, concatMap, replicate, enumFromTo, concat
       , reverse, iterate, splitAt)
import Streamly.Internal.Data.Stream.StreamD.Type
import Streamly.Internal.Data.SVar

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

{-# INLINE_NORMAL groupsBy #-}
groupsBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsBy cmp f (Stream step state) = Stream (stepOuter f) (Just state, Nothing)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st, Nothing) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                fs <- initial
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> go SPEC x s fs1
                    FL.Done b -> return $ Yield b (Just s, Just x)
            Skip s -> return $ Skip (Just s, Nothing)
            Stop -> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp x prev
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC prev s fs1
                            FL.Done b -> return $ Yield b (Just s, Just x)
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC prev s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)
    stepOuter (Fold fstep initial done) gst (Just st, Just prev) = do
        fs <- initial
        r <- fstep fs prev
        case r of
            FL.Partial fs1 -> go SPEC st fs1
            FL.Done b -> return $ Yield b (Just st, Nothing)

        where

        -- XXX code duplicated from the previous equation
        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp x prev
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC s fs1
                            FL.Done b -> return $ Yield b (Just s, Just x)
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)
    stepOuter _ _ (Nothing, _) = return Stop

{-# INLINE_NORMAL groupsRollingBy #-}
groupsRollingBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsRollingBy cmp f (Stream step state) =
    Stream (stepOuter f) (Just state, Nothing)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (Just st, Nothing) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                fs <- initial
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> go SPEC x s fs1
                    FL.Done b -> return $ Yield b (Just s, Just x)
            Skip s -> return $ Skip $ (Just s, Nothing)
            Stop -> return Stop

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
                            FL.Done b -> return $ Yield b (Just s, Just x)
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC prev s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)
    stepOuter (Fold fstep initial done) gst (Just st, Just prev') = do
        fs <- initial
        r <- fstep fs prev'
        case r of
            FL.Partial fs1 -> go SPEC prev' st fs1
            FL.Done b -> return $ Yield b (Just st, Nothing)

        where

        go !_ prevv stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prevv x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC x s fs1
                            FL.Done b -> return $ Yield b (Just s, Just x)
                    else done acc >>= \r -> return $ Yield r (Just s, Just x)
                Skip s -> go SPEC prevv s acc
                Stop -> done acc >>= \r -> return $ Yield r (Nothing, Nothing)
    stepOuter _ _ (Nothing, _) = return Stop

data WordsByState s = WordsByJust s | WordsByNothing

{-# INLINE_NORMAL wordsBy #-}
wordsBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
wordsBy predicate f (Stream step state) =
    Stream (stepOuter f) (WordsByJust state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter (Fold fstep initial done) gst (WordsByJust st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                if predicate x
                then return $ Skip (WordsByJust s)
                else do
                    fs <- initial
                    r <- fstep fs x
                    case r of
                        FL.Partial fs1 -> go SPEC s fs1
                        FL.Done b -> return $ Yield b (WordsByJust s)

            Skip s    -> return $ Skip $ WordsByJust s
            Stop      -> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then done acc >>= \r -> return $ Yield r (WordsByJust s)
                    else do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC s fs1
                            FL.Done b -> return $ Yield b (WordsByJust s)
                Skip s -> go SPEC s acc
                Stop -> done acc >>= \r -> return $ Yield r WordsByNothing

    stepOuter _ _ WordsByNothing = return Stop

-- String search algorithms:
-- http://www-igm.univ-mlv.fr/~lecroq/string/index.html

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

{-# ANN type SplitOnSeqState Fuse #-}
data SplitOnSeqState rb rh ck w fs s b x =
      SplitOnSeqInit
    | SplitOnSeqYield b (SplitOnSeqState rb rh ck w fs s b x)
    | SplitOnSeqDone

    | SplitOnSeqEmpty s

    | SplitOnSeqSingle !fs s x

    | SplitOnSeqWordInit s
    | SplitOnSeqWordLoop !w s !fs
    | SplitOnSeqWordDone Int !fs !w

    | SplitOnSeqKRInit Int s rb !rh
    | SplitOnSeqKRLoop fs s rb !rh !ck
    | SplitOnSeqKRCheck fs s rb !rh
    | SplitOnSeqKRDone Int !fs rb !rh

{-# INLINE_NORMAL splitOnSeq #-}
splitOnSeq
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitOnSeq patArr (Fold fstep initial done) (Stream step state) =
    Stream stepOuter SplitOnSeqInit

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

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

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ SplitOnSeqInit =
        if patLen == 0
        then return $ Skip $ SplitOnSeqEmpty state
        else if patLen == 1
             then do
                 acc <- initial
                 pat <- liftIO $ A.unsafeIndexIO patArr 0
                 return $ Skip $ SplitOnSeqSingle acc state pat
             else if sizeOf (undefined :: a) * patLen
                       <= sizeOf (undefined :: Word)
                  then return $ Skip $ SplitOnSeqWordInit state
                  else do
                      (rb, rhead) <- liftIO $ RB.new patLen
                      skip $ SplitOnSeqKRInit 0 state rb rhead

    stepOuter _ (SplitOnSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSeqEmpty st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                fs <- initial
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> do
                        b <- done fs1
                        skip $ SplitOnSeqYield b (SplitOnSeqEmpty s)
                    FL.Done b -> skip $ SplitOnSeqYield b (SplitOnSeqEmpty s)

            Skip s -> return $ Skip (SplitOnSeqEmpty s)
            Stop -> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSeqSingle fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                if pat == x
                then do
                    r <- done fs
                    fs1 <- initial
                    return $ Skip $ SplitOnSeqYield r (SplitOnSeqSingle fs1 s pat)
                else do
                    r <- fstep fs x
                    case r of
                        FL.Partial fs1 -> skip $ SplitOnSeqSingle fs1 s pat
                        FL.Done b -> do
                            fs1 <- initial
                            skip $ SplitOnSeqYield b (SplitOnSeqSingle fs1 s pat)
            Skip s -> return $ Skip $ SplitOnSeqSingle fs s pat
            Stop -> do
                r <- done fs
                return $ Skip $ SplitOnSeqYield r SplitOnSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    stepOuter _ (SplitOnSeqWordDone 0 fs _) = do
        r <- done fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                fs1 <- initial
                let next = SplitOnSeqWordDone (n - 1) fs1 wrd
                skip $ SplitOnSeqYield b next

    stepOuter gst (SplitOnSeqWordInit st0) =
        go SPEC 0 0 st0

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                    if idx == maxIndex
                    then do
                        fs <- initial
                        if wrd1 .&. wordMask == wordPat
                        then do
                            r <- done fs
                            let next = SplitOnSeqWordInit s
                            skip $ SplitOnSeqYield r next
                        else skip $ SplitOnSeqWordLoop wrd1 s fs
                    else go SPEC (idx + 1) wrd1 s
                Skip s -> go SPEC idx wrd s
                Stop -> do
                    fs <- initial
                    if idx /= 0
                    then skip $ SplitOnSeqWordDone idx fs wrd
                    else do
                        r <- done fs
                        skip $ SplitOnSeqYield r SplitOnSeqDone

    stepOuter gst (SplitOnSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <- fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 -> do
                            if wrd1 .&. wordMask == wordPat
                            then do
                                b <- done fs1
                                let next = SplitOnSeqWordInit s
                                skip $ SplitOnSeqYield b next
                            else go SPEC wrd1 s fs1
                        FL.Done b ->
                               let next = SplitOnSeqWordInit s
                                in skip $ SplitOnSeqYield b next
                Skip s -> go SPEC wrd s fs
                Stop -> skip $ SplitOnSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (SplitOnSeqKRInit idx st rb rh) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                rh1 <- liftIO $ RB.unsafeInsert rb rh x
                if idx == maxIndex
                then do
                    let fold = RB.unsafeFoldRing (RB.ringBound rb)
                    let !ringHash = fold addCksum 0 rb
                    fs <- initial
                    if ringHash == patHash
                    then skip $ SplitOnSeqKRCheck fs s rb rh1
                    else skip $ SplitOnSeqKRLoop fs s rb rh1 ringHash
                else skip $ SplitOnSeqKRInit (idx + 1) s rb rh1
            Skip s -> skip $ SplitOnSeqKRInit idx s rb rh
            Stop -> do
                fs <- initial
                skip $ SplitOnSeqKRDone idx fs rb (RB.startOf rb)

    -- XXX The recursive "go" is more efficient than the state based recursion
    -- code commented out below. Perhaps its more efficient because of
    -- factoring out "rb" outside the loop.
    --
    stepOuter gst (SplitOnSeqKRLoop fs0 st0 rb rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum1 = deltaCksum cksum old x
                    r <- fstep fs old
                    case r of
                        FL.Partial fs1 -> do
                            rh1 <- liftIO (RB.unsafeInsert rb rh x)
                            if cksum1 == patHash
                            then skip $ SplitOnSeqKRCheck fs1 s rb rh1
                            else go SPEC fs1 s rh1 cksum1
                        FL.Done b ->
                            let next = SplitOnSeqKRInit 0 s rb (RB.startOf rb)
                             in skip $ SplitOnSeqYield b next
                Skip s -> go SPEC fs s rh cksum
                Stop -> skip $ SplitOnSeqKRDone patLen fs rb rh

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

    stepOuter _ (SplitOnSeqKRCheck fs st rb rh) = do
        if RB.unsafeEqArray rb rh patArr
        then do
            r <- done fs
            let next = SplitOnSeqKRInit 0 st rb (RB.startOf rb)
            skip $ SplitOnSeqYield r next
        else skip $ SplitOnSeqKRLoop fs st rb rh patHash
    stepOuter _ (SplitOnSeqKRDone 0 fs _ _) = do
        r <- done fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqKRDone n fs rb rh) = do
        old <- liftIO $ peek rh
        let rh1 = RB.advance rb rh
        r <- fstep fs old
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqKRDone (n - 1) fs1 rb rh1
            FL.Done b -> do
                fs1 <- initial
                let next = SplitOnSeqKRDone (n - 1) fs1 rb rh1
                skip $ SplitOnSeqYield b next

{-# ANN type SplitOnSuffixSeqState Fuse #-}
data SplitOnSuffixSeqState rb rh ck w fs s b x =
      SplitOnSuffixSeqInit
    | SplitOnSuffixSeqYield b (SplitOnSuffixSeqState rb rh ck w fs s b x)
    | SplitOnSuffixSeqDone

    | SplitOnSuffixSeqEmpty s

    | SplitOnSuffixSeqSingleInit s x
    | SplitOnSuffixSeqSingle !fs s x

    | SplitOnSuffixSeqWordInit s
    | SplitOnSuffixSeqWordLoop !w s !fs
    | SplitOnSuffixSeqWordDone Int !fs !w

    | SplitOnSuffixSeqKRInit Int s rb !rh
    | SplitOnSuffixSeqKRInit1 !fs s rb !rh
    | SplitOnSuffixSeqKRLoop fs s rb !rh !ck
    | SplitOnSuffixSeqKRCheck fs s rb !rh
    | SplitOnSuffixSeqKRDone Int !fs rb !rh

{-# INLINE_NORMAL splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitOnSuffixSeq withSep patArr (Fold fstep initial done) (Stream step state) =
    Stream stepOuter SplitOnSuffixSeqInit

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For single element pattern case
    {-# INLINE processYieldSingle #-}
    processYieldSingle pat x s fs =
        if pat == x
        then do
            r <- if withSep then fstep fs x else return $ FL.Partial fs
            case r of
                FL.Partial fs1 -> do
                    b <- done fs1
                    let next = SplitOnSuffixSeqSingleInit s pat
                    skip $ SplitOnSuffixSeqYield b next
                FL.Done b ->
                    let next = SplitOnSuffixSeqSingleInit s pat
                     in skip $ SplitOnSuffixSeqYield b next
        else do
            r <- fstep fs x
            case r of
                FL.Partial fs1 -> skip $ SplitOnSuffixSeqSingle fs1 s pat
                FL.Done b ->
                    let next = SplitOnSuffixSeqSingleInit s pat
                     in skip $ SplitOnSuffixSeqYield b next

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
    stepOuter _ SplitOnSuffixSeqInit =
        if patLen == 0
        then skip $ SplitOnSuffixSeqEmpty state
        else if patLen == 1
             then do
                 pat <- liftIO $ A.unsafeIndexIO patArr 0
                 skip $ SplitOnSuffixSeqSingleInit state pat
             else if sizeOf (undefined :: a) * patLen
                       <= sizeOf (undefined :: Word)
                  then skip $ SplitOnSuffixSeqWordInit state
                  else do
                      (rb, rhead) <- liftIO $ RB.new patLen
                      skip $ SplitOnSuffixSeqKRInit 0 state rb rhead

    stepOuter _ (SplitOnSuffixSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSuffixSeqEmpty st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                acc <- initial
                r <- fstep acc x
                case r of
                    FL.Partial fs -> do
                        b <- done fs
                        skip $ SplitOnSuffixSeqYield b (SplitOnSuffixSeqEmpty s)
                    FL.Done b ->
                        skip $ SplitOnSuffixSeqYield b (SplitOnSuffixSeqEmpty s)
            Skip s -> skip (SplitOnSuffixSeqEmpty s)
            Stop -> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSuffixSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSuffixSeqSingleInit st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> initial >>= processYieldSingle pat x s
            Skip s -> skip $ SplitOnSuffixSeqSingleInit s pat
            Stop -> return Stop

    stepOuter gst (SplitOnSuffixSeqSingle fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> processYieldSingle pat x s fs
            Skip s -> skip $ SplitOnSuffixSeqSingle fs s pat
            Stop -> do
                r <- done fs
                skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    stepOuter _ (SplitOnSuffixSeqWordDone 0 fs _) = do
        r <- done fs
        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
    stepOuter _ (SplitOnSuffixSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSuffixSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                fs1 <- initial
                let next = SplitOnSuffixSeqWordDone (n - 1) fs1 wrd
                skip $ SplitOnSuffixSeqYield b next

    stepOuter gst (SplitOnSuffixSeqWordInit st0) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                fs <- initial
                let wrd = addToWord 0 x
                r <- if withSep then fstep fs x else return $ FL.Partial fs
                case r of
                    FL.Partial fs1 -> go SPEC 1 wrd s fs1
                    FL.Done b -> do
                        let next = SplitOnSuffixSeqWordInit s
                         in skip $ SplitOnSuffixSeqYield b next
            Skip s -> skip (SplitOnSuffixSeqWordInit s)
            Stop -> return Stop

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                    r <- if withSep then fstep fs x else return $ FL.Partial fs
                    case r of
                        FL.Partial fs1 ->
                            if idx /= maxIndex
                            then go SPEC (idx + 1) wrd1 s fs1
                            else if wrd1 .&. wordMask /= wordPat
                            then skip $ SplitOnSuffixSeqWordLoop wrd1 s fs1
                            else do
                                b <- done fs
                                let next = SplitOnSuffixSeqWordInit s
                                skip $ SplitOnSuffixSeqYield b next
                        FL.Done b ->
                            let next = SplitOnSuffixSeqWordInit s
                             in skip $ SplitOnSuffixSeqYield b next
                Skip s -> go SPEC idx wrd s fs
                Stop -> skip $ SplitOnSuffixSeqWordDone idx fs wrd

    stepOuter gst (SplitOnSuffixSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <-
                        if withSep
                        then fstep fs x
                        else fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 ->
                            if wrd1 .&. wordMask == wordPat
                            then do
                                b <- done fs1
                                let next = SplitOnSuffixSeqWordInit s
                                skip $ SplitOnSuffixSeqYield b next
                            else go SPEC wrd1 s fs1
                        FL.Done b ->
                            let next = SplitOnSuffixSeqWordInit s
                             in skip $ SplitOnSuffixSeqYield b next
                Skip s -> go SPEC wrd s fs
                Stop ->
                    if wrd .&. wordMask == wordPat
                    then return Stop
                    else if withSep
                    then do
                        r <- done fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (SplitOnSuffixSeqKRInit idx0 st0 rb rh0) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                rh1 <- liftIO $ RB.unsafeInsert rb rh0 x
                fs <- initial
                r <- if withSep then fstep fs x else return $ FL.Partial fs
                case r of
                    FL.Partial fs1 ->
                        skip $ SplitOnSuffixSeqKRInit1 fs1 s rb rh1
                    FL.Done b ->
                        let next = SplitOnSuffixSeqKRInit 0 s rb (RB.startOf rb)
                         in skip $ SplitOnSuffixSeqYield b next
            Skip s -> skip $ SplitOnSuffixSeqKRInit idx0 s rb rh0
            Stop -> return Stop

    stepOuter gst (SplitOnSuffixSeqKRInit1 fs0 st0 rb rh0) = do
        go SPEC 1 rh0 st0 fs0

        where

        go !_ !idx !rh st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    rh1 <- liftIO (RB.unsafeInsert rb rh x)
                    r <- if withSep then fstep fs x else return $ FL.Partial fs
                    case r of
                        FL.Partial fs1 ->
                            if idx /= maxIndex
                            then go SPEC (idx + 1) rh1 s fs1
                            else skip $
                                let fold = RB.unsafeFoldRing (RB.ringBound rb)
                                    !ringHash = fold addCksum 0 rb
                                 in if ringHash == patHash
                                    then SplitOnSuffixSeqKRCheck fs1 s rb rh1
                                    else SplitOnSuffixSeqKRLoop
                                            fs1 s rb rh1 ringHash
                        FL.Done b ->
                            let next = SplitOnSuffixSeqKRInit
                                            0 st rb (RB.startOf rb)
                             in skip $ SplitOnSuffixSeqYield b next
                Skip s -> go SPEC idx rh s fs
                Stop -> do
                    -- do not issue a blank segment when we end at pattern
                    if (idx == maxIndex) && RB.unsafeEqArray rb rh patArr
                    then return Stop
                    else if withSep
                    then do
                        r <- done fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqKRDone idx fs rb (RB.startOf rb)

    stepOuter gst (SplitOnSuffixSeqKRLoop fs0 st0 rb rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    rh1 <- liftIO (RB.unsafeInsert rb rh x)
                    let cksum1 = deltaCksum cksum old x
                    r <- if withSep then fstep fs x else fstep fs old
                    case r of
                        FL.Partial fs1 ->
                            if (cksum1 /= patHash)
                            then go SPEC fs1 s rh1 cksum1
                            else skip $ SplitOnSuffixSeqKRCheck fs1 s rb rh1
                        FL.Done b ->
                            let next = SplitOnSuffixSeqKRInit
                                            0 st rb (RB.startOf rb)
                             in skip $ SplitOnSuffixSeqYield b next
                Skip s -> go SPEC fs s rh cksum
                Stop ->
                    if RB.unsafeEqArray rb rh patArr
                    then return Stop
                    else if withSep
                    then do
                        r <- done fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqKRDone patLen fs rb rh

    stepOuter _ (SplitOnSuffixSeqKRCheck fs st rb rh) = do
        if RB.unsafeEqArray rb rh patArr
        then do
            r <- done fs
            let next = SplitOnSuffixSeqKRInit 0 st rb (RB.startOf rb)
            skip $ SplitOnSuffixSeqYield r next
        else skip $ SplitOnSuffixSeqKRLoop fs st rb rh patHash

    stepOuter _ (SplitOnSuffixSeqKRDone 0 fs _ _) = do
        r <- done fs
        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
    stepOuter _ (SplitOnSuffixSeqKRDone n fs rb rh) = do
        old <- liftIO $ peek rh
        let rh1 = RB.advance rb rh
        r <- fstep fs old
        case r of
            FL.Partial fs1 -> skip $ SplitOnSuffixSeqKRDone (n - 1) fs1 rb rh1
            FL.Done b -> do
                fs1 <- initial
                let next = SplitOnSuffixSeqKRDone (n - 1) fs1 rb rh1
                skip $ SplitOnSuffixSeqYield b next

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
    (Stream step (SplitInitial state1))

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
            Stop -> return $ Stop

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
    step _ SplitFinishing = return $ Stop

-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBySuffix #-}
splitInnerBySuffix
    :: (Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBySuffix splitter joiner (Stream step1 state1) =
    (Stream step (SplitInitial state1))

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
            Stop -> return $ Stop

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
            Stop -> return $
                if buf == mempty
                then Stop
                else Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return Stop
