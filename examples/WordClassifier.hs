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
import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.IORef
import qualified Data.List as List
import qualified Data.Ord as Ord
import           Foreign.Storable (Storable(..))
import qualified Streamly.Data.Unicode.Stream as Streamly
import qualified Streamly.Internal.Data.Unicode.Stream as Streamly
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as Streamly
import           System.Environment (getArgs)
import           System.IO (openFile, IOMode(..))

instance (Enum a, Storable a) => Hashable (A.Array a) where
    hash arr = runIdentity $ IUF.fold A.read IFL.rollingHash arr
    hashWithSalt salt arr = runIdentity $
        IUF.fold A.read (IFL.rollingHashWithSalt salt) arr

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
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode

    -- Write the stream to a hashmap consisting of word counts
    mp <-
        let
            alter Nothing    = fmap Just $ newIORef (1 :: Int)
            alter (Just ref) = modifyIORef' ref (+ 1) >> return (Just ref)
        in Streamly.unfold FH.read src   -- SerialT IO Word8
         & Streamly.decodeLatin1         -- SerialT IO Char
         & Streamly.map toLower          -- SerialT IO Char
         & Streamly.words FL.toList      -- SerialT IO String
         & Streamly.filter (all isAlpha) -- SerialT IO String
         & Streamly.foldlM' (flip (Map.alterF alter)) Map.empty -- IO (Map String (IORef Int))

    -- Print the top hashmap entries
    counts <-
        let readRef (w, ref) = do
                cnt <- readIORef ref
                return (w, cnt)
         in Map.toList mp
          & mapM readRef

    traverse_ print $ List.sortOn (Ord.Down . snd) counts
                    & List.take 25
