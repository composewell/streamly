{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif
-- compile with:
-- ghc -O2 -fspec-constr-recursive=10 -fmax-worker-args=16 word-classifier.hs
--
import qualified Data.Char as Char
import           Data.Foldable
import           Data.Function ((&))
import           Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as Map
import           Data.Hashable
import qualified Data.List as List
import qualified Data.Ord as Ord
import           Foreign.Storable (Storable(..))
import qualified Streamly.Data.String as Streamly
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Fold as FL
import qualified Streamly.Internal as Streamly
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as Streamly
import           System.Environment (getArgs)
import           System.IO (openFile, IOMode(..))
import           Data.IORef

instance (Enum a, Storable a) => Hashable (A.Array a) where
    hash arr = runIdentity $ Streamly.runFold Streamly.rollingHash (A.read arr)
    hashWithSalt salt arr = runIdentity $
        Streamly.runFold (Streamly.rollingHashWithSalt salt) (A.read arr)

{-# INLINE toLower #-}
toLower :: Char -> Char
toLower c
  | uc >= 0x61 && uc <= 0x7a = c
  | otherwise = Char.toLower c
  where
    uc = fromIntegral (Char.ord c) :: Word

{-# INLINE isAlpha #-}
isAlpha :: Char -> Bool
isAlpha c
  | uc >= 0x61 && uc <= 0x7a = True
  | otherwise = Char.isAlpha c
  where
    uc = fromIntegral (Char.ord c) :: Word

main :: IO ()
main =
    let
        increment m str = Map.upsertM insert update str (1 :: Int) m
            where
            insert v = newIORef v
            update ref new = modifyIORef' ref $ (+ new)
    in do
        name <- fmap head getArgs
        src <- openFile name ReadMode
        FH.read src                     -- SerialT IO Word8
         & Streamly.decodeChar8         -- SerialT IO Char
         & Streamly.map toLower         -- SerialT IO Char
         & Streamly.foldWords FL.toList -- SerialT IO String
         & Streamly.filter (all isAlpha)
         & Streamly.foldlM' increment Map.empty
         & fmap Map.toList
         >>= mapM (\(w, ref) -> readIORef ref >>= \cnt -> return (w, cnt))
         & fmap (List.take 25 . List.sortOn (Ord.Down . snd))
         >>= traverse_ print
