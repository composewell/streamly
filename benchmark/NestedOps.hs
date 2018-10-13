-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NestedOps where

import Control.Exception (try)
import GHC.Exception (ErrorCall)

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

sumCount :: Int
sumCount = 1000000

prodCount :: Int
prodCount = 100

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> Int -> t m Int
source = sourceUnfoldrM

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM n value = S.serially $ S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldr start n = S.unfoldr step start
    where
    step cnt =
        if cnt > start + n
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

{-# INLINE runToList #-}
runToList :: Monad m => Stream m a -> m [a]
runToList = S.toList

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
toNull
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m ()
toNull t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    return $ x + y

{-# INLINE toList #-}
toList
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m [Int]
toList t start = runToList . t $ do
    x <- source start prodCount
    y <- source start prodCount
    return $ x + y

{-# INLINE toListSome #-}
toListSome
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m [Int]
toListSome t start =
    runToList . t $ S.take 1000 $ do
        x <- source start prodCount
        y <- source start prodCount
        return $ x + y

{-# INLINE filterAllOut #-}
filterAllOut
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m ()
filterAllOut t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    let s = x + y
    if s < 0
    then return s
    else S.nil

{-# INLINE filterAllIn #-}
filterAllIn
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m ()
filterAllIn t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    let s = x + y
    if s > 0
    then return s
    else S.nil

{-# INLINE filterSome #-}
filterSome
    :: (S.IsStream t, S.MonadAsync m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m ()
filterSome t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    let s = x + y
    if s > 1100000
    then return s
    else S.nil

{-# INLINE breakAfterSome #-}
breakAfterSome
    :: (S.IsStream t, Monad (t IO))
    => (t IO Int -> S.SerialT IO Int) -> Int -> IO ()
breakAfterSome t start = do
    (_ :: Either ErrorCall ()) <- try $ runStream . t $ do
        x <- source start prodCount
        y <- source start prodCount
        let s = x + y
        if s > 1100000
        then error "break"
        else return s
    return ()
