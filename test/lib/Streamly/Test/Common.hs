-- |
-- Module      : Streamly.Test.Common
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Common
    ( equals
    , listEquals
    , checkListEqual
    , chooseInt
    , chooseDouble
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List ((\\))
import Test.QuickCheck (Property, Gen, choose, counterexample)
import Test.QuickCheck.Monadic (PropertyM, assert, monitor, monadicIO)

equals
    :: (Show a, Monad m)
    => (a -> a -> Bool) -> a -> a -> PropertyM m ()
equals eq stream list = do
    when (not $ stream `eq` list) $
        monitor
            (counterexample $
             "stream " <> show stream
             <> "\nlist   " <> show list
            )
    assert (stream `eq` list)

listEquals
    :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq stream list = do
    when (not $ stream `eq` list) $ liftIO $ putStrLn $
                  "stream " <> show stream
             <> "\nlist   " <> show list
             <> "\nstream length  " <> show (length stream)
             <> "\nlist length  " <> show (length list)
             <> "\nstream \\\\ list " <> show (stream \\ list)
             <> "\nlist \\\\ stream " <> show (list \\ stream)
    when (not $ stream `eq` list) $
        monitor
            (counterexample $
                  "stream " <> show stream
             <> "\nlist   " <> show list
             <> "\nstream length  " <> show (length stream)
             <> "\nlist length  " <> show (length list)
             <> "\nstream \\\\ list " <> show (stream \\ list)
             <> "\nlist \\\\ stream " <> show (list \\ stream)
             )
    assert (stream `eq` list)

checkListEqual :: (Show a, Eq a) => [a] -> [a] -> Property
checkListEqual ls_1 ls_2 = monadicIO (listEquals (==) ls_1 ls_2)

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

chooseDouble :: (Double, Double) -> Gen Double
chooseDouble = choose
