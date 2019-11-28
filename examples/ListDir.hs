module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly (async)
import Data.Function ((&))

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Dir as Dir

-- | List the current directory recursively using concurrent processing
--
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- XXX Fix bug in enqueueAhead when mixing serial with ahead:
    -- (\x -> S.yield dir <> S.concatMapWith ahead recursePath x)
    --  :: SerialT IO String
    -- or S.cons dir . S.concatMapWith ahead recursePath
    S.mapM_ print $ S.concatMapTreeWith async listDir
        (S.yieldM $ return (Left "."))

    where

    listDir dir =
          Dir.toEither dir            -- SerialT IO (Either String String)
        & S.map (prefixDir dir)       -- SerialT IO (Either String String)

    prefixDir :: String -> Either String String -> Either String String
    prefixDir dir (Right x) = Right $ dir ++ "/" ++ x
    prefixDir dir (Left x)  = Left  $ dir ++ "/" ++ x
