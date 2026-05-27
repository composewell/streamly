-- |
-- Module      : Streamly.Test.FileSystem.WindowsPath
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for "Streamly.Internal.FileSystem.WindowsPath". Windows path
-- semantics are independent of the host OS, so this test suite runs on every
-- platform.

module Streamly.Test.FileSystem.WindowsPath (main) where

import Control.Exception (SomeException, evaluate, try)
import Data.Either (isLeft)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (isJust, isNothing)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath)
import Test.Hspec as H

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.WindowsPath as Path

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Build a path from a string, failing at runtime if invalid.
p :: String -> WindowsPath
p = Path.fromString_

-- | Round-trip a path through toString.
str :: WindowsPath -> String
str = Path.toString

-- | True if a pure 'String' value forces to an exception.
fails :: String -> IO Bool
fails x = isLeft <$> (try (evaluate x) :: IO (Either SomeException String))

-------------------------------------------------------------------------------
-- Construction and conversion
-------------------------------------------------------------------------------

testFromString :: Spec
testFromString = describe "fromString" $ do
    it "valid drive-absolute path" $
        isJust (Path.fromString "C:\\Users" :: Maybe WindowsPath)
            `shouldBe` True
    it "valid relative path" $
        isJust (Path.fromString "a\\b\\c" :: Maybe WindowsPath)
            `shouldBe` True
    it "valid UNC path" $
        isJust (Path.fromString "\\\\server\\share\\x" :: Maybe WindowsPath)
            `shouldBe` True
    it "valid verbatim path" $
        isJust (Path.fromString "\\\\?\\C:\\x" :: Maybe WindowsPath)
            `shouldBe` True
    it "empty string is invalid" $
        isNothing (Path.fromString "" :: Maybe WindowsPath) `shouldBe` True
    it "toString . fromString_ roundtrip" $
        str (p "C:\\Users") `shouldBe` "C:\\Users"
    it "forward slashes preserved on roundtrip" $
        str (p "a/b") `shouldBe` "a/b"
    -- test correct array size allocation for unicode encoding
    it "non-ASCII (BMP) roundtrip" $
        str (p "\945.txt") `shouldBe` "\945.txt"
    -- Non-BMP chars require a UTF-16 surrogate pair (2 words for 1 char),
    -- which would be truncated if the array were sized by char count.
    it "multi-word UTF-16 roundtrip (non-BMP char)" $
        str (p "\x1F600\\file") `shouldBe` "\x1F600\\file"

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Separators
-------------------------------------------------------------------------------

testSeparators :: Spec
testSeparators = describe "separators" $ do
    it "hasTrailingSeparator backslash" $
        Path.hasTrailingSeparator (p "foo\\") `shouldBe` True
    it "hasTrailingSeparator forward slash also counts" $
        Path.hasTrailingSeparator (p "foo/") `shouldBe` True
    it "dropTrailingSeparators" $
        str (Path.dropTrailingSeparators (p "foo\\")) `shouldBe` "foo"
    it "addTrailingSeparator adds primary separator" $
        Path.hasTrailingSeparator (Path.addTrailingSeparator (p "foo"))
            `shouldBe` True

-------------------------------------------------------------------------------
-- Root detection
-------------------------------------------------------------------------------

testRooted :: Spec
testRooted = describe "isRooted" $ do
    it "drive root is rooted" $ Path.isRooted (p "C:\\") `shouldBe` True
    it "drive-only is rooted" $ Path.isRooted (p "C:") `shouldBe` True
    it "UNC root is rooted" $
        Path.isRooted (p "\\\\server\\share\\") `shouldBe` True
    it "verbatim root is rooted" $
        Path.isRooted (p "\\\\?\\C:\\x") `shouldBe` True
    it ". is unrooted" $ Path.isUnrooted (p ".") `shouldBe` True
    it "x is unrooted" $ Path.isUnrooted (p "x") `shouldBe` True

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

testJoin :: Spec
testJoin = describe "join" $ do
    it "join two segments" $
        str (Path.join (p "C:\\Users") (p "bin")) `shouldBe` "C:\\Users\\bin"
    it "trailing separator on first is kept" $
        str (Path.join (p "x\\") (p "y")) `shouldBe` "x\\y"
    it "forward separator is preserved if present" $
        str (Path.join (p "x/") (p "y")) `shouldBe` "x/y"

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

testSplitRoot :: Spec
testSplitRoot = describe "splitRoot" $ do
    let split = fmap toList . Path.splitRoot . p
        toList (a, b) = (str a, fmap str b)
        cases =
            [ ("C:",                Just ("C:",           Nothing))
            , ("C:/",               Just ("C:/",          Nothing))
              -- UNC: share name is part of the root
            , ("//x/y",             Just ("//x/y",        Nothing))
            , ("//x/y/",            Just ("//x/y/",       Nothing))
            , ("//x/y/z",           Just ("//x/y/",       Just "z"))
              -- Verbatim drive paths
            , ("\\\\?\\c:\\",       Just ("\\\\?\\c:\\",  Nothing))
            , ("\\\\?\\c:\\home",   Just ("\\\\?\\c:\\",  Just "home"))
              -- Verbatim UNC paths: share name is part of the root
            , ("\\\\?\\UNC\\c:\\x",
                Just ("\\\\?\\UNC\\c:\\x", Nothing))
            , ("\\\\?\\UNC\\srv\\share\\x",
                Just ("\\\\?\\UNC\\srv\\share\\", Just "x"))
            ]
    mapM_
        (\(input, expected) ->
            it ("splitRoot " ++ show input) $
                split input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

eq :: String -> String -> Bool
eq a b = Path.eqPath id (p a) (p b)

eqWith :: (Path.EqCfg -> Path.EqCfg) -> String -> String -> Bool
eqWith cfg a b = Path.eqPath cfg (p a) (p b)

testEqPathDefault :: Spec
testEqPathDefault = describe "eqPath default" $ do
    it "equal absolute paths" $ eq "C:\\x" "C:\\x" `shouldBe` True
    it "forward/backward separators interchangeable" $
        eq "x\\y" "x/y" `shouldBe` True
    it "case sensitive for non-root segments" $
        eq "x" "X" `shouldBe` False
    it "drive-only paths equal under default (allowRelativeEquality True)" $ do
        eq "C:" "C:" `shouldBe` True
        eq "C:x" "C:x" `shouldBe` True
    it "drive letter case-insensitive even for relative drive paths" $
        eq "c:" "C:" `shouldBe` True
    it "allowRelativeEquality False rejects drive-only relatives" $ do
        eqWith (Path.allowRelativeEquality False) "C:" "C:" `shouldBe` False
        eqWith (Path.allowRelativeEquality False) "C:x" "C:x" `shouldBe` False
    it "C:bin equals C:./bin" $
        eq "C:bin" "C:./bin" `shouldBe` True
    -- Note: C: cannot carry a trailing separator (C:\\ means the absolute
    -- root of drive C, not the current dir). Compare against C:. instead.
    it "C: equals C:." $
        eq "C:" "C:." `shouldBe` True
    it "C:.. equals C:./.." $
        eq "C:.." "C:./.." `shouldBe` True
    it "redundant separators ignored" $
        eq "x//y" "x/y" `shouldBe` True
    it "dot segments ignored" $
        eq "x/./y" "x/y" `shouldBe` True
    it "trailing separator matters" $
        eq "x/" "x" `shouldBe` False

testEqPathDriveAlwaysIgnoresCase :: Spec
testEqPathDriveAlwaysIgnoresCase =
    describe "eqPath drive root is always case-insensitive" $ do
        it "absolute drive paths differ only in drive letter case" $
            eq "c:/x" "C:/x" `shouldBe` True
        it "UNC server name case-insensitive" $
            eq "\\\\Server\\share\\x" "\\\\server\\share\\x" `shouldBe` True
        it "UNC share name case-insensitive (share is part of root)" $
            eq "\\\\server\\Share\\x" "\\\\server\\share\\x" `shouldBe` True
        it "Body case still respected with default" $
            eq "C:/X" "C:/x" `shouldBe` False

testEqPathVerbatim :: Spec
testEqPathVerbatim = describe "eqPath verbatim \\\\?\\ paths" $ do
    it "verbatim paths byte-compared - identical equal" $
        eq "\\\\?\\C:\\x" "\\\\?\\C:\\x" `shouldBe` True
    it "verbatim paths case-sensitive on drive letter" $
        eq "\\\\?\\C:\\x" "\\\\?\\c:\\x" `shouldBe` False
    it "verbatim paths case-sensitive on body" $
        eq "\\\\?\\C:\\x" "\\\\?\\C:\\X" `shouldBe` False
    it "verbatim paths separator-sensitive" $
        eq "\\\\?\\C:\\x" "\\\\?\\C:/x" `shouldBe` False
    it "verbatim never equal to non-verbatim equivalent" $
        eq "\\\\?\\C:\\x" "C:\\x" `shouldBe` False
    it "ignoreCase True still does not affect verbatim" $
        eqWith (Path.ignoreCase True) "\\\\?\\C:\\x" "\\\\?\\c:\\x"
            `shouldBe` False

testEqPathRelaxed :: Spec
testEqPathRelaxed =
    describe "eqPath with relaxed config" $ do
        let cfg =
                  Path.ignoreTrailingSeparators True
                . Path.ignoreCase True
                . Path.allowRelativeEquality True
        it "leading dot ignored" $
            eqWith cfg "./x" "x" `shouldBe` True
        it "trailing sep ignored" $
            eqWith cfg "X/" "x" `shouldBe` True
        it "drive-only relative paths equal" $
            eqWith cfg "C:x" "c:X" `shouldBe` True
        it "mixed separators with leading dot" $
            eqWith cfg ".\\x" "./X" `shouldBe` True
        it "double sep collapse" $
            eqWith cfg "x//y" "x/y" `shouldBe` True

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

testNormalise :: Spec
testNormalise = describe "normalise" $ do
    let nrm = str . Path.normalise id . p
    it "drive letter uppercased" $
        nrm "c:\\x" `shouldBe` "C:\\x"
    it "drive root only uppercased" $
        nrm "c:" `shouldBe` "C:"
    it "UNC root (server and share) lowercased" $
        nrm "\\\\Server\\Share\\x" `shouldBe` "\\\\server\\share\\x"
    it "UNC root lowercased when only root present" $
        nrm "\\\\Server\\Share\\" `shouldBe` "\\\\server\\share\\"
    it "verbatim path untouched" $
        nrm "\\\\?\\C:\\Foo" `shouldBe` "\\\\?\\C:\\Foo"
    it "verbatim path with mixed case kept verbatim" $
        nrm "\\\\?\\c:\\Foo" `shouldBe` "\\\\?\\c:\\Foo"
    it "separators in non-verbatim normalised to primary" $
        nrm "C:/x/y" `shouldBe` "C:\\x\\y"
    it "ignoreCase True does not change verbatim" $
        str (Path.normalise (Path.ignoreCase True) (p "\\\\?\\C:\\Foo"))
            `shouldBe` "\\\\?\\C:\\Foo"

-------------------------------------------------------------------------------
-- Path prefix
-------------------------------------------------------------------------------

testCommonPrefix :: Spec
testCommonPrefix = describe "takeCommonPrefix" $ do
    let cp a b = fmap str $ Path.takeCommonPrefix id (p a) (p b)
    it "same drive-absolute path" $
        cp "C:\\a\\b" "C:\\a\\b" `shouldBe` Just "C:\\a\\b"
    it "common segments under same drive" $
        cp "C:\\a\\b\\c" "C:\\a\\b\\d" `shouldBe` Just "C:\\a\\b"
    it "drive letters differ in case still common" $
        cp "c:\\a" "C:\\a" `shouldBe` Just "C:\\a"
    it "verbatim paths byte-equal share full path" $
        cp "\\\\?\\C:\\a" "\\\\?\\C:\\a" `shouldBe` Just "\\\\?\\C:\\a"
    it "verbatim paths share the device root when bodies differ" $
        cp "\\\\?\\C:\\Foo" "\\\\?\\C:\\foo" `shouldBe` Just "\\\\?\\C:\\"

testStripPrefix :: Spec
testStripPrefix = describe "stripPrefix" $ do
    let sp a b = fmap str $ Path.stripPrefix id (p a) (p b)
    it "strips matching prefix" $
        sp "C:\\a\\b" "C:\\a\\b\\c" `shouldBe` Just "c"
    it "drive case-insensitive match" $
        sp "c:\\a" "C:\\a\\b" `shouldBe` Just "b"
    it "no match" $
        sp "C:\\a\\x" "C:\\a\\b\\c" `shouldBe` Nothing
    it "verbatim prefix byte-strict" $
        sp "\\\\?\\C:\\a" "\\\\?\\C:\\a\\b" `shouldBe` Just "b"
    it "verbatim differs in case no strip" $
        sp "\\\\?\\c:\\a" "\\\\?\\C:\\a\\b" `shouldBe` Nothing

-------------------------------------------------------------------------------
-- Path view
-------------------------------------------------------------------------------

testPathView :: Spec
testPathView = describe "path view" $ do
    it "takeFileName" $
        fmap str (Path.takeFileName (p "C:\\Users\\file.txt"))
            `shouldBe` Just "file.txt"
    it "takeDirectory" $
        fmap str (Path.takeDirectory (p "C:\\Users\\file.txt"))
            `shouldBe` Just "C:\\Users\\"

-------------------------------------------------------------------------------
-- validatePath (windows-specific)
-------------------------------------------------------------------------------

testValidatePath :: Spec
testValidatePath = describe "validatePath" $ do
    let isValid = isJust . Path.validatePath . Path.encodeString
        cases =
            [ ("",                False)
            , ("\0",              False)
              -- invalid characters
            , ("c::",             False)
            , ("c:\\x:y",         False)
            , ("x*",              False)
            , ("x\ty",            False)
              -- reserved names
            , ("pRn.txt",         False)
            , (" pRn .txt",       False)
            , ("c:\\x\\pRn",      False)
            , ("c:\\x\\pRn.txt",  False)
            , ("c:\\pRn\\x",      False)
            , ("c:\\ pRn \\x",    False)
            , ("pRn.x.txt",       False)
              -- drive root forms
            , ("c:",              True)
            , ("c:a\\b",          True)
            , ("c:\\",            True)
            , ("c:\\\\",          False)
            , ("c:\\/",           False)
            , ("c:\\\\x",         False)
            , ("c:\\/x",          False)
              -- mixed separators
            , ("/x\\y",           True)
            , ("\\/",             True)
            , ("/\\",             True)
            , ("\\/x/y",          True)
            , ("/x/\\y",          True)
            , ("/x\\/y",          True)
              -- share path / UNC
            , ("\\",              True)
            , ("\\\\",            False)
            , ("\\\\\\",          False)
            , ("\\\\x",           False)
            , ("\\\\x\\",         False) -- server only, no share
            , ("\\\\server\\",    False)
            , ("\\\\x\\y",        True)
            , ("\\\\server\\x",   True)
            , ("//x/y",           True)
            , ("\\\\prn\\y",      False)
            , ("\\\\x\\\\",       False)
            , ("\\\\x\\\\x",      False)
            , ("\\\\\\x",         False)
              -- short UNC (\\?\C:\)
            , ("\\\\?\\c:",       False)
            , ("\\\\?\\c:\\",     True)
            , ("\\\\?\\c:x",      False)
            , ("\\\\?\\c:\\\\",   False)
            , ("\\\\?\\c:\\x",    True)
            , ("\\\\?\\c:\\\\\\", False)
            , ("\\\\?\\c:\\\\x",  False)
              -- long UNC (\\?\UNC\)
            , ("\\\\?\\UnC\\x",   True)  -- UnC is treated as share name
            , ("\\\\?\\UNC\\x",   False)
            , ("\\\\?\\UNC\\server", False)
            , ("\\\\?\\UNC\\c:\\x", True)
              -- DOS device namespace
            , ("\\\\.\\x",        True)
            , ("\\\\??\\x",       True)
            ]
    mapM_
        (\(input, expected) ->
            it ("validatePath " ++ show input) $
                isValid input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- isRooted (windows-specific)
-------------------------------------------------------------------------------

testIsRootedWindows :: Spec
testIsRootedWindows = describe "isRooted (windows-specific)" $ do
    let rooted = Path.isRooted . p
        cases =
            [ ("/",     True)
            , ("/x",    True)
            , (".",     False)
            , ("./x",   False)
            , ("c:",    True)
            , ("c:x",   True)
            , ("c:/",   True)
            , ("//x/y", True)
            ]
    mapM_
        (\(input, expected) ->
            it ("isRooted " ++ show input) $
                rooted input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- unsafeJoin / join (windows-specific)
-------------------------------------------------------------------------------

testUnsafeJoin :: Spec
testUnsafeJoin = describe "unsafeJoin" $ do
    let f a b = str $ Path.unsafeJoin (p a) (p b)
    it "x y" $ f "x" "y" `shouldBe` "x\\y"
    it "x/ y" $ f "x/" "y" `shouldBe` "x/y"
    it "x /y" $ f "x" "/y" `shouldBe` "x/y"
    it "x/ /y" $ f "x/" "/y" `shouldBe` "x/y"
    -- joinDrive-equivalent cases
    it "c: /x" $ f "c:" "/x" `shouldBe` "c:/x"
    it "//x/y/ /z" $ f "//x/y/" "/z" `shouldBe` "//x/y/z"

testJoinWindows :: Spec
testJoinWindows = describe "join (windows-specific)" $ do
    let f a b = str $ Path.join (p a) (p b)
    it "x y" $ f "x" "y" `shouldBe` "x\\y"
    it "c: y" $ f "c:" "y" `shouldBe` "c:y"
    it "c:x y" $ f "c:x" "y" `shouldBe` "c:x\\y"
    it "\\x y" $ f "\\x" "y" `shouldBe` "\\x\\y"
    it "c:/ y" $ f "c:/" "y" `shouldBe` "c:/y"
    it "//x/y/ z" $ f "//x/y/" "z" `shouldBe` "//x/y/z"
    it "trailing forward slash kept" $ f "x/" "y" `shouldBe` "x/y"
    it "second-path /y rejected for any first" $ do
        mapM_
            (\a -> fails (f a "/y") >>= (`shouldBe` True))
            ["c:/", "c:/x", "c:", "c:x", "/x", "x", "//x/y/"]
    it "second-path c:/ rejected" $ fails (f "c:" "c:/") >>= (`shouldBe` True)

-------------------------------------------------------------------------------
-- splitPath / splitPath_ (windows-specific)
-------------------------------------------------------------------------------

splitToList ::
       (WindowsPath -> Stream.Stream Identity WindowsPath)
    -> String -> [String]
splitToList f = runIdentity . Stream.toList . fmap str . f . p

testSplitPath_ :: Spec
testSplitPath_ = describe "splitPath_ (windows-specific)" $ do
    let cases =
            [ ("c:x",      ["c:", "x"])
            , ("c:/",      ["c:/"])
            , ("c:/x",     ["c:/", "x"])
            , ("//x/y/",   ["//x/y"])
            , ("//x/y/z",  ["//x/y", "z"])
            , ("./a",      [".", "a"])
            , ("c:./a",    ["c:", "a"])
            , ("a/.",      ["a"])
            , ("/x",       ["/", "x"])
            , ("/x/\\y",   ["/", "x", "y"])
            , ("\\x/\\y",  ["\\", "x", "y"])
            ]
    mapM_
        (\(input, expected) ->
            it ("splitPath_ " ++ show input) $
                splitToList Path.splitPath_ input `shouldBe` expected)
        cases

testSplitPath :: Spec
testSplitPath = describe "splitPath (windows-specific)" $ do
    let cases =
            [ ("/x",       ["/", "x"])
            , ("/x/\\y",   ["/", "x/", "y"])
            , ("\\x/\\y",  ["\\", "x/", "y"])
            ]
    mapM_
        (\(input, expected) ->
            it ("splitPath " ++ show input) $
                splitToList Path.splitPath input `shouldBe` expected)
        cases

-------------------------------------------------------------------------------
-- splitExtension (windows-specific)
-------------------------------------------------------------------------------

testSplitExtensionWindows :: Spec
testSplitExtensionWindows = describe "splitExtension (windows-specific)" $ do
    let split = fmap toList . Path.splitExtension . p
        toList (a, b) = (str a, str b)
    it "drive-only-relative x:y has no extension" $
        split "x:y" `shouldBe` Nothing
    it "drive-only-relative x:.y has no extension" $
        split "x:.y" `shouldBe` Nothing

-------------------------------------------------------------------------------
-- readArray / showArray round-trip
-------------------------------------------------------------------------------

testReadArray :: Spec
testReadArray = describe "readArray/showArray round-trip" $
    it "round-trip via show + read" $ do
        let arr = Path.encodeString "hello"
            -- The doctest in the source uses: readArray $ show arr
            shown = show arr
        Path.showArray (Path.readArray shown :: WindowsPath)
            `shouldBe` "fromList [104,101,108,108,111]"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.WindowsPath"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        testFromString
        testValidatePath
        testSeparators
        testRooted
        testIsRootedWindows
        testJoin
        testJoinWindows
        testUnsafeJoin
        testSplitRoot
        testSplitPath
        testSplitPath_
        testSplitExtensionWindows
        testEqPathDefault
        testEqPathDriveAlwaysIgnoresCase
        testEqPathVerbatim
        testEqPathRelaxed
        testNormalise
        testCommonPrefix
        testStripPrefix
        testPathView
        testReadArray
