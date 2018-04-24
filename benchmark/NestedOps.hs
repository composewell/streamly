-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE ScopedTypeVariables #-}

module NestedOps where

import Control.Exception (try)
import GHC.Exception (ErrorCall)

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

sumCount :: Int
sumCount = 1000000

prodCount :: Int
prodCount = 1000

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.StreamT m a

{-# INLINE source #-}
source :: S.IsStream t => Int -> Int -> t m Int
source start n = S.fromFoldable [start..start+n]

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

{-# INLINE runToList #-}
runToList :: Monad m => Stream m a -> m [a]
runToList = S.toList

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNullLinear #-}
toNullLinear :: Monad m => Int -> m ()
toNullLinear start = runStream $ source start sumCount

{-# INLINE toListLinear #-}
toListLinear :: Monad m => Int -> m [Int]
toListLinear start = runToList $ source start sumCount

{-# INLINE append #-}
append
    :: (Monoid (t m Int), Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m ()
append t start = runStream $ t $ foldMap return [start..start+sumCount]

{-# INLINE toNull0 #-}
toNull0
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m (Int, Int) -> S.StreamT m (Int, Int)) -> Int -> m ()
toNull0 t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    return (x,y)

{-# INLINE toList0 #-}
toList0
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m (Int, Int) -> S.StreamT m (Int, Int)) -> Int -> m [(Int, Int)]
toList0 t start = runToList . t $ do
    x <- source start prodCount
    y <- source start prodCount
    return (x,y)

{-# INLINE toNull #-}
toNull
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m ()
toNull t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    return $ x * x + y * y

{-# INLINE toList #-}
toList
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m [Int]
toList t start = runToList . t $ do
    x <- source start prodCount
    y <- source start prodCount
    return $ x * x + y * y

{-# INLINE toListSome #-}
toListSome
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m [Int]
toListSome t start =
    runToList . t $ S.take 1000 $ do
        x <- source start prodCount
        y <- source start prodCount
        return $ x * x + y * y

{-# INLINE filterAllOut #-}
filterAllOut
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m ()
filterAllOut t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    let s = x * x + y * y
    if (s < 1)
    then return s
    else S.nil

{-# INLINE filterAllIn #-}
filterAllIn
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m ()
filterAllIn t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    let s = x * x + y * y
    if (s > 1)
    then return s
    else S.nil

{-# INLINE filterSome #-}
filterSome
    :: (S.IsStream t, Monad m, Monad (t m))
    => (t m Int -> S.StreamT m Int) -> Int -> m ()
filterSome t start = runStream . t $ do
    x <- source start prodCount
    y <- source start prodCount
    let s = x * x + y * y
    if (s > 1100000)
    then return s
    else S.nil

{-# INLINE breakAfterSome #-}
breakAfterSome
    :: (S.IsStream t, Monad (t IO))
    => (t IO Int -> S.StreamT IO Int) -> Int -> IO ()
breakAfterSome t start = do
    (_ :: Either ErrorCall ()) <- try $ runStream . t $ do
        x <- source start prodCount
        y <- source start prodCount
        let s = x * x + y * y
        if (s > 1100000)
        then error "break"
        else return s
    return ()
