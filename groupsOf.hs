import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal as Internal
import System.Environment (getArgs)
import System.IO (IOMode(..), openFile)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    devNull <- openFile "/dev/null" WriteMode

    -- Do cabal v2-build to build the library and then build this file using:
    -- ghc -O2 -fspec-constr-recursive=10 -ddump-simpl -ddump-to-file -dsuppress-all groupsOf.hs

    -- To generate a 10x better, fused code comment line 602 in
    -- src/Streamly/Memory/Array/Types.hs, then do a cabal v2-build and compile
    -- this file again with the same command as above.
    --
    -- Compare the generated core.

    -- Where to find Internal.writeS
        -- src/Streamly/FileSystem/Handle/Internal.hs
        -- line 114
        -- line 101
        -- src/Streamly/Memory/ArrayStream.hs
        -- line 130
        -- src/Streamly/Memory/Array/Types.hs
        -- line 594
    -- Where to find FH.read
        -- src/Streamly/FileSystem/Handle.hs
        -- line 246
        -- line 218
        -- line 196
        -- line 157
    Internal.writeS devNull $ FH.read src
