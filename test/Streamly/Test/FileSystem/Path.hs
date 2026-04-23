-- |
-- Module      : Streamly.Test.FileSystem.Path
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.FileSystem.Path (main) where

import Data.Maybe (isJust, isNothing)
import Test.Hspec as H

import qualified Streamly.Internal.FileSystem.Path as Path

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Build a path from a string, failing at runtime if invalid.
p :: String -> Path.Path
p = Path.fromString_

-- | Round-trip a path through toString.
str :: Path.Path -> String
str = Path.toString


-------------------------------------------------------------------------------
-- Construction and conversion
-------------------------------------------------------------------------------

testFromString :: Spec
testFromString = describe "fromString" $ do
    it "valid absolute path" $
        isJust (Path.fromString "/usr/bin" :: Maybe Path.Path) `shouldBe` True
    it "valid relative path" $
        isJust (Path.fromString "a/b/c" :: Maybe Path.Path) `shouldBe` True
    it "empty string is invalid" $
        isNothing (Path.fromString "" :: Maybe Path.Path) `shouldBe` True
    it "toString . fromString_ roundtrip" $
        str (p "/usr/bin") `shouldBe` "/usr/bin"
    it "relative roundtrip" $
        str (p "a/b/c") `shouldBe` "a/b/c"

-------------------------------------------------------------------------------
-- Separators
-------------------------------------------------------------------------------

testSeparators :: Spec
testSeparators = describe "separators" $ do
    it "hasTrailingSeparator with trailing" $
        Path.hasTrailingSeparator (p "foo/") `shouldBe` True
    it "hasTrailingSeparator without trailing" $
        Path.hasTrailingSeparator (p "foo") `shouldBe` False
    it "dropTrailingSeparators" $
        str (Path.dropTrailingSeparators (p "foo/")) `shouldBe` "foo"
    it "dropTrailingSeparators idempotent" $
        str (Path.dropTrailingSeparators (p "foo")) `shouldBe` "foo"
    it "addTrailingSeparator adds separator" $
        Path.hasTrailingSeparator (Path.addTrailingSeparator (p "foo"))
            `shouldBe` True
    it "addTrailingSeparator idempotent" $
        str (Path.addTrailingSeparator (p "foo/")) `shouldBe` "foo/"

-------------------------------------------------------------------------------
-- Root detection
-------------------------------------------------------------------------------

testRooted :: Spec
testRooted = describe "isRooted/isUnrooted" $ do
    it "/ is rooted" $ Path.isRooted (p "/") `shouldBe` True
    it "/x is rooted" $ Path.isRooted (p "/x") `shouldBe` True
    it ". is rooted" $ Path.isRooted (p ".") `shouldBe` True
    it "./x is rooted" $ Path.isRooted (p "./x") `shouldBe` True
    it "x is unrooted" $ Path.isUnrooted (p "x") `shouldBe` True
    it "x/y is unrooted" $ Path.isUnrooted (p "x/y") `shouldBe` True
    it ".. is unrooted" $ Path.isUnrooted (p "..") `shouldBe` True
    it "../x is unrooted" $ Path.isUnrooted (p "../x") `shouldBe` True

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

testJoin :: Spec
testJoin = describe "join" $ do
    it "join two segments" $
        str (Path.join (p "/usr") (p "bin")) `shouldBe` "/usr/bin"
    it "join with trailing sep on first" $
        str (Path.join (p "/usr/") (p "bin")) `shouldBe` "/usr/bin"
    it "unsafeJoin ignores leading sep" $
        str (Path.unsafeJoin (p "x") (p "/y")) `shouldBe` "x/y"
    it "joinStr appends string" $
        str (Path.joinStr (p "/usr") "bin") `shouldBe` "/usr/bin"
    it "joinDir requires trailing sep" $
        str (Path.joinDir (p "/usr/") (p "bin")) `shouldBe` "/usr/bin"

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

testSplitRoot :: Spec
testSplitRoot = describe "splitRoot" $ do
    it "/ has root only" $ do
        let r = Path.splitRoot (p "/")
        isJust r `shouldBe` True
        fmap (str . fst) r `shouldBe` Just "/"
        fmap (fmap str . snd) r `shouldBe` Just Nothing
    it "/home has root and path" $ do
        let r = Path.splitRoot (p "/home")
        fmap (str . fst) r `shouldBe` Just "/"
        fmap (fmap str . snd) r `shouldBe` Just (Just "home")
    it "relative has no root" $
        isNothing (Path.splitRoot (p "home")) `shouldBe` True
    it ". is root" $
        fmap (str . fst) (Path.splitRoot (p ".")) `shouldBe` Just "."
    it "./home splits correctly" $ do
        let r = Path.splitRoot (p "./home")
        fmap (str . fst) r `shouldBe` Just "./"
        fmap (fmap str . snd) r `shouldBe` Just (Just "home")

testSplitFile :: Spec
testSplitFile = describe "splitFile" $ do
    it "/ has no file" $
        isNothing (Path.splitFile (p "/")) `shouldBe` True
    it ". has no file" $
        isNothing (Path.splitFile (p ".")) `shouldBe` True
    it "/home splits to dir+file" $ do
        let r = Path.splitFile (p "/home")
        fmap (fmap str . fst) r `shouldBe` Just (Just "/")
        fmap (str . snd) r `shouldBe` Just "home"
    it "home alone has no dir" $ do
        let r = Path.splitFile (p "home")
        fmap (fmap str . fst) r `shouldBe` Just Nothing
        fmap (str . snd) r `shouldBe` Just "home"
    it "x/ has no file (dir path)" $
        isNothing (Path.splitFile (p "x/")) `shouldBe` True

testPathView :: Spec
testPathView = describe "path view" $ do
    it "takeFileName" $
        fmap str (Path.takeFileName (p "/home/user/file.txt"))
            `shouldBe` Just "file.txt"
    it "takeFileName on dir returns Nothing" $
        isNothing (Path.takeFileName (p "/home/user/")) `shouldBe` True
    it "takeDirectory" $
        fmap str (Path.takeDirectory (p "/home/user/file.txt"))
            `shouldBe` Just "/home/user/"
    it "takeDirectory on plain file returns Nothing" $
        isNothing (Path.takeDirectory (p "file.txt")) `shouldBe` True

-------------------------------------------------------------------------------
-- Extensions
-------------------------------------------------------------------------------

testExtensions :: Spec
testExtensions = describe "extensions" $ do
    it "splitExtension with extension" $ do
        let r = Path.splitExtension (p "/home/user/file.txt")
        fmap (str . fst) r `shouldBe` Just "/home/user/file"
        fmap (str . snd) r `shouldBe` Just ".txt"
    it "splitExtension no extension" $
        isNothing (Path.splitExtension (p "x")) `shouldBe` True
    it "splitExtension dot file" $
        isNothing (Path.splitExtension (p ".hidden")) `shouldBe` True
    it "takeExtension" $
        fmap str (Path.takeExtension (p "file.tar")) `shouldBe` Just ".tar"
    it "dropExtension" $
        str (Path.dropExtension (p "/home/file.txt")) `shouldBe` "/home/file"
    it "dropExtension no-op when no extension" $
        str (Path.dropExtension (p "/home/file")) `shouldBe` "/home/file"
    it "takeFileBase" $
        fmap str (Path.takeFileBase (p "/home/user/file.txt"))
            `shouldBe` Just "file"
    it "takeFileBase hidden file" $
        fmap str (Path.takeFileBase (p "/home/user/.hidden"))
            `shouldBe` Just ".hidden"

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

testEqPath :: Spec
testEqPath = describe "eqPath" $ do
    it "equal plain paths" $
        Path.eqPath id (p "x") (p "x") `shouldBe` True
    it "redundant separators ignored" $
        Path.eqPath id (p "x//y") (p "x/y") `shouldBe` True
    it "dot segments ignored" $
        Path.eqPath id (p "x/./y") (p "x/y") `shouldBe` True
    it "trailing sep matters by default" $
        Path.eqPath id (p "x/") (p "x") `shouldBe` False
    it "ignoreTrailingSeparators" $
        Path.eqPath (Path.ignoreTrailingSeparators True) (p "x/") (p "x")
            `shouldBe` True
    it "case sensitive by default" $
        Path.eqPath id (p "x") (p "X") `shouldBe` False
    it "relative paths not equal by default" $
        Path.eqPath id (p ".") (p ".") `shouldBe` False
    it "allowRelativeEquality" $
        Path.eqPath (Path.allowRelativeEquality True) (p ".") (p ".")
            `shouldBe` True
    it "eqPathBytes exact match" $
        Path.eqPathBytes (p "x//y") (p "x//y") `shouldBe` True
    it "eqPathBytes differs" $
        Path.eqPathBytes (p "x//y") (p "x/y") `shouldBe` False

-------------------------------------------------------------------------------
-- Normalization
-------------------------------------------------------------------------------

testPathDepth :: Spec
testPathDepth = describe "pathDepth" $ do
    it "root has depth 0" $
        Path.pathDepth (p "/") `shouldBe` 0
    it "single segment" $
        Path.pathDepth (p "x") `shouldBe` 1
    it "single segment with trailing sep" $
        Path.pathDepth (p "x/") `shouldBe` 1
    it "/x has depth 1" $
        Path.pathDepth (p "/x") `shouldBe` 1
    it "/x/y has depth 2" $
        Path.pathDepth (p "/x/y") `shouldBe` 2
    it "x/y has depth 2" $
        Path.pathDepth (p "x/y") `shouldBe` 2
    it ". has depth 0 (rooted, 1 comp - 1)" $
        Path.pathDepth (p ".") `shouldBe` 0
    it "./x has depth 1" $
        Path.pathDepth (p "./x") `shouldBe` 1

testCollapseSeparators :: Spec
testCollapseSeparators = describe "collapseSeparators" $ do
    it "no change needed" $
        str (Path.collapseSeparators (p "a/b")) `shouldBe` "a/b"
    it "double separator" $
        str (Path.collapseSeparators (p "a//b")) `shouldBe` "a/b"
    it "triple separator" $
        str (Path.collapseSeparators (p "a///b")) `shouldBe` "a/b"
    it "preserves leading separator" $
        str (Path.collapseSeparators (p "/a//b")) `shouldBe` "/a/b"
    it "preserves trailing separator" $
        str (Path.collapseSeparators (p "a//b/")) `shouldBe` "a/b/"

testCollapseDotDots :: Spec
testCollapseDotDots = describe "collapseDotDots" $ do
    it "no change needed" $
        str (Path.collapseDotDots (p "a/b/c")) `shouldBe` "a/b/c"
    it "basic collapse" $
        str (Path.collapseDotDots (p "a/b/../c")) `shouldBe` "a/c"
    it "multiple collapses" $
        str (Path.collapseDotDots (p "a/b/c/../../d")) `shouldBe` "a/d"
    it "cannot go above start - leading dotdot kept" $
        str (Path.collapseDotDots (p "a/../../c")) `shouldBe` "../c"
    it "absolute path collapse" $
        str (Path.collapseDotDots (p "/a/b/../c")) `shouldBe` "/a/c"
    it "cannot go above root" $ do
        -- /.. should stay as / (root is preserved)
        let result = Path.collapseDotDots (p "/a/..")
        str result `shouldBe` "/"
    it "all segments cancel gives dot" $
        str (Path.collapseDotDots (p "a/..")) `shouldBe` "."
    it "dotdot at start preserved" $
        str (Path.collapseDotDots (p "../a")) `shouldBe` "../a"

testNormalise :: Spec
testNormalise = describe "normalise" $ do
    it "collapses separators and dot segments" $
        str (Path.normalise id (p "a//./b")) `shouldBe` "a/b"
    it "with ignoreTrailingSeparators drops trailing" $
        str (Path.normalise (Path.ignoreTrailingSeparators True) (p "a//b/"))
            `shouldBe` "a/b"
    it "without flag keeps trailing sep only if no dot segments" $
        str (Path.normalise id (p "a//b")) `shouldBe` "a/b"

-------------------------------------------------------------------------------
-- Path prefix
-------------------------------------------------------------------------------

testCommonPrefix :: Spec
testCommonPrefix = describe "takeCommonPrefix" $ do
    it "common prefix of same paths" $
        fmap str (Path.takeCommonPrefix id (p "/a/b") (p "/a/b"))
            `shouldBe` Just "/a/b"
    it "common prefix to segment boundary" $
        fmap str (Path.takeCommonPrefix id (p "/a/b/c") (p "/a/b/d"))
            `shouldBe` Just "/a/b"
    it "no common prefix for relative paths" $
        isNothing (Path.takeCommonPrefix id (p "a/b") (p "c/d")) `shouldBe` True
    it "one is prefix of other" $
        fmap str (Path.takeCommonPrefix id (p "/a/b") (p "/a/b/c"))
            `shouldBe` Just "/a/b"
    it "root only in common" $
        fmap str (Path.takeCommonPrefix id (p "/a") (p "/b"))
            `shouldBe` Just "/"
    it "no separator in common" $
        isNothing (Path.takeCommonPrefix id (p "abc") (p "abd")) `shouldBe` True

testStripPrefix :: Spec
testStripPrefix = describe "stripPrefix" $ do
    it "strips matching prefix" $
        fmap str (Path.stripPrefix id (p "/a/b") (p "/a/b/c"))
            `shouldBe` Just "c"
    it "strips with trailing sep in prefix" $
        fmap str (Path.stripPrefix id (p "/a/b/") (p "/a/b/c"))
            `shouldBe` Just "c"
    it "no match returns Nothing" $
        isNothing (Path.stripPrefix id (p "/a/x") (p "/a/b/c")) `shouldBe` True
    it "not at segment boundary returns Nothing" $
        isNothing (Path.stripPrefix id (p "/a/b") (p "/a/bc")) `shouldBe` True
    it "prefix equals path returns Nothing (empty remainder)" $
        isNothing (Path.stripPrefix id (p "/a/b") (p "/a/b")) `shouldBe` True
    it "strips nested prefix" $
        fmap str (Path.stripPrefix id (p "/a") (p "/a/b/c"))
            `shouldBe` Just "b/c"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.Path"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        testFromString
        testSeparators
        testRooted
        testJoin
        testSplitRoot
        testSplitFile
        testPathView
        testExtensions
        testEqPath
        testPathDepth
        testCollapseSeparators
        testCollapseDotDots
        testNormalise
        testCommonPrefix
        testStripPrefix
