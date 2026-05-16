-- |
-- Module      : Streamly.Test.FileSystem.PosixPath
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for "Streamly.Internal.FileSystem.PosixPath". Posix path semantics
-- are independent of the host OS, so this test suite runs on every platform.

module Streamly.Test.FileSystem.PosixPath (main) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (isJust, isNothing)
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import Test.Hspec as H

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.PosixPath as Path

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Build a path from a string, failing at runtime if invalid.
p :: String -> PosixPath
p = Path.fromString_

-- | Round-trip a path through toString.
str :: PosixPath -> String
str = Path.toString


-------------------------------------------------------------------------------
-- Construction and conversion
-------------------------------------------------------------------------------

testFromString :: Spec
testFromString = describe "fromString" $ do
    it "valid absolute path" $
        isJust (Path.fromString "/usr/bin" :: Maybe PosixPath) `shouldBe` True
    it "valid relative path" $
        isJust (Path.fromString "a/b/c" :: Maybe PosixPath) `shouldBe` True
    it "empty string is invalid" $
        isNothing (Path.fromString "" :: Maybe PosixPath) `shouldBe` True
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
-- Validation
-------------------------------------------------------------------------------

testValidatePath :: Spec
testValidatePath = describe "validatePath" $ do
    let isValid = isJust . Path.validatePath . Path.encodeString
    it "empty path invalid" $ isValid "" `shouldBe` False
    it "null char invalid" $ isValid "\0" `shouldBe` False

-------------------------------------------------------------------------------
-- splitPath (with separators kept on dir components)
-------------------------------------------------------------------------------

splitToList :: (PosixPath -> Stream.Stream Identity PosixPath) -> String -> [String]
splitToList f = runIdentity . Stream.toList . fmap str . f . p

testSplitPath :: Spec
testSplitPath = describe "splitPath" $ do
    let cases =
            [ (".",         ["."])
            , ("././",      ["./"])
            , ("./a/b/.",   ["./", "a/", "b/"])
            , ("..",        [".."])
            , ("../",       ["../"])
            , ("a/..",      ["a/", ".."])
            , ("/",         ["/"])
            , ("//",        ["/"])
            , ("/x",        ["/", "x"])
            , ("/./x/",     ["/", "x/"])
            , ("/x/./y",    ["/", "x/", "y"])
            , ("/x/../y",   ["/", "x/", "../", "y"])
            , ("/x///y",    ["/", "x/", "y"])
            , ("/x/\\y",    ["/", "x/", "\\y"])
            ]
    mapM_
        (\(input, expected) ->
            it ("splitPath " ++ show input) $
                splitToList Path.splitPath input `shouldBe` expected)
        cases

testSplitPath_ :: Spec
testSplitPath_ = describe "splitPath_ (no separators on dir)" $ do
    let cases =
            [ (".",         ["."])
            , ("././",      ["."])
            , (".//",       ["."])
            , ("//",        ["/"])
            , ("//x/y/",    ["/", "x", "y"])
            , ("./a",       [".", "a"])
            , ("a/.",       ["a"])
            , ("/",         ["/"])
            , ("/x",        ["/", "x"])
            , ("/./x/",     ["/", "x"])
            , ("/x/./y",    ["/", "x", "y"])
            , ("/x/../y",   ["/", "x", "..", "y"])
            , ("/x///y",    ["/", "x", "y"])
            , ("/x/\\y",    ["/", "x", "\\y"])
            ]
    mapM_
        (\(input, expected) ->
            it ("splitPath_ " ++ show input) $
                splitToList Path.splitPath_ input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- Extended splitRoot
-------------------------------------------------------------------------------

testSplitRootExtended :: Spec
testSplitRootExtended = describe "splitRoot (extended)" $ do
    let split = fmap toList . Path.splitRoot . p
        toList (a, b) = (str a, fmap str b)
        cases =
            [ ("/",      Just ("/", Nothing))
            , (".",      Just (".", Nothing))
            , ("./",     Just ("./", Nothing))
            , ("/home",  Just ("/", Just "home"))
            , ("//",     Just ("//", Nothing))
            , ("./home", Just ("./", Just "home"))
            , ("home",   Nothing)
            ]
    mapM_
        (\(input, expected) ->
            it ("splitRoot " ++ show input) $ split input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- Extended splitFile
-------------------------------------------------------------------------------

testSplitFileExtended :: Spec
testSplitFileExtended = describe "splitFile (extended)" $ do
    let split = fmap toList . Path.splitFile . p
        toList (a, b) = (fmap str a, str b)
        cases =
            [ ("/",       Nothing)
            , (".",       Nothing)
            , ("/.",      Nothing)
            , ("..",      Nothing)
            , ("/home",   Just (Just "/",   "home"))
            , ("./home",  Just (Just "./",  "home"))
            , ("home",    Just (Nothing,    "home"))
            , ("x/",      Nothing)
            , ("x/y",     Just (Just "x/",  "y"))
            , ("x//y",    Just (Just "x//", "y"))
            , ("x/./y",   Just (Just "x/./", "y"))
            ]
    mapM_
        (\(input, expected) ->
            it ("splitFile " ++ show input) $ split input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- Extended splitExtension
-------------------------------------------------------------------------------

testSplitExtensionExtended :: Spec
testSplitExtensionExtended = describe "splitExtension (extended)" $ do
    let split = fmap toList . Path.splitExtension . p
        toList (a, b) = (str a, str b)
        cases =
            [ ("/",      Nothing)
            , (".",      Nothing)
            , ("..",     Nothing)
            , ("x",      Nothing)
            , ("/x",     Nothing)
            , ("x/",     Nothing)
            , ("./x",    Nothing)
            , ("x/.",    Nothing)
            , ("x/y.",   Nothing)
            , ("/x.y",   Just ("/x",    ".y"))
            , ("/x.y.",  Nothing)
            , ("/x.y..", Nothing)
            , ("x/.y",   Nothing)
            , (".x",     Nothing)
            , ("x.",     Nothing)
            , (".x.y",   Just (".x",    ".y"))
            , ("x/y.z",  Just ("x/y",   ".z"))
            , ("x.y.z",  Just ("x.y",   ".z"))
            , ("x..y",   Just ("x.",    ".y"))
            , ("...",    Nothing)
            , ("..x",    Just (".",     ".x"))
            , ("...x",   Just ("..",    ".x"))
            , ("x/y.z/", Nothing)
            , ("x/y",    Nothing)
            ]
    mapM_
        (\(input, expected) ->
            it ("splitExtension " ++ show input) $
                split input `shouldBe` expected)
        cases

testTakeFileBaseExtended :: Spec
testTakeFileBaseExtended = describe "takeFileBase (extended)" $ do
    let tb = fmap str . Path.takeFileBase . p
    it "with extension" $ tb "/home/user/file.txt" `shouldBe` Just "file"
    it "no extension" $ tb "/home/user/file" `shouldBe` Just "file"
    it "leading dot only segment" $ tb "/home/user/.txt" `shouldBe` Just ".txt"
    it "trailing separator means no file" $
        tb "/home/user/" `shouldBe` Nothing

-------------------------------------------------------------------------------
-- unsafeJoin
-------------------------------------------------------------------------------

testUnsafeJoin :: Spec
testUnsafeJoin = describe "unsafeJoin" $ do
    let f a b = str $ Path.unsafeJoin (p a) (p b)
    it "x y" $ f "x" "y" `shouldBe` "x/y"
    it "x/ y" $ f "x/" "y" `shouldBe` "x/y"
    it "x /y" $ f "x" "/y" `shouldBe` "x/y"
    it "x/ /y" $ f "x/" "/y" `shouldBe` "x/y"

-------------------------------------------------------------------------------
-- dropTrailingSeparators / hasTrailingSeparator (extras)
-------------------------------------------------------------------------------

testDropTrailingSeparatorsExtras :: Spec
testDropTrailingSeparatorsExtras =
    describe "dropTrailingSeparators (extras)" $ do
        let f = str . Path.dropTrailingSeparators . p
        it "./ -> ." $ f "./" `shouldBe` "."

-------------------------------------------------------------------------------
-- normalise (more cases)
-------------------------------------------------------------------------------

testNormaliseExtras :: Spec
testNormaliseExtras = describe "normalise (extras)" $ do
    let nrm cfg = str . Path.normalise cfg . p
    it "ignoreTrailingSeparators False keeps trailing sep" $
        nrm id "a//b/" `shouldBe` "a/b/"
    it "leading dot dropped when collapsing dot segments" $
        nrm id "./a/./b" `shouldBe` "a/b"
    it "absolute path collapses dot segments" $
        nrm id "/a//./b" `shouldBe` "/a/b"

-------------------------------------------------------------------------------
-- Path equality edge cases
-------------------------------------------------------------------------------

testEqPathExtended :: Spec
testEqPathExtended = describe "eqPath (extended)" $ do
    let eq = Path.eqPath id
    it "/x equals //x" $ eq (p "/x") (p "//x") `shouldBe` True
    it "x/y/. equals x/y" $ eq (p "x/y/.") (p "x/y") `shouldBe` True
    it ". equal to . is False by default" $
        eq (p ".") (p ".") `shouldBe` False
    it "./x equal to ./x is False by default" $
        eq (p "./x") (p "./x") `shouldBe` False
    it "./x equal to x is False by default" $
        eq (p "./x") (p "x") `shouldBe` False
    it "allowRelativeEquality . . True" $
        Path.eqPath (Path.allowRelativeEquality True) (p ".") (p ".")
            `shouldBe` True
    it "allowRelativeEquality ./x x True" $
        Path.eqPath (Path.allowRelativeEquality True) (p "./x") (p "x")
            `shouldBe` True
    it "allowRelativeEquality ./x ././x True" $
        Path.eqPath (Path.allowRelativeEquality True) (p "./x") (p "././x")
            `shouldBe` True
    it "ignoreTrailingSeparators True" $
        Path.eqPath (Path.ignoreTrailingSeparators True) (p "x/") (p "x")
            `shouldBe` True

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.PosixPath"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        testFromString
        testValidatePath
        testSeparators
        testDropTrailingSeparatorsExtras
        testRooted
        testJoin
        testUnsafeJoin
        testSplitRoot
        testSplitRootExtended
        testSplitFile
        testSplitFileExtended
        testSplitPath
        testSplitPath_
        testPathView
        testExtensions
        testSplitExtensionExtended
        testTakeFileBaseExtended
        testEqPath
        testEqPathExtended
        testPathDepth
        testCollapseSeparators
        testCollapseDotDots
        testNormalise
        testNormaliseExtras
        testCommonPrefix
        testStripPrefix
