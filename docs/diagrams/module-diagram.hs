{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List (isPrefixOf)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim str = go str
  where
    go [] = [[]]
    go s
      | delim `isPrefixOf` s = [] : go (drop (length delim) s)
      | otherwise =
          let (c:cs) = s
              (x:xs) = go cs
          in (c:x) : xs

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

title1 t =
  vsep 0.3 [ text line # fontSizeL 0.3 # bold | line <- splitOn "\n" t ]
title2 t = text t # fontSizeL 0.2 # bold

textBox x y t = t <> strutX x <> strutY y

item1 t = text t # fontSizeL 0.25
enumerate items = vsep 0.3 [ textBox 2.0 0.4 (item1 i) | i <- items ]

boxedXY :: Double -> Double -> Diagram B -> Diagram B
boxedXY w h content = content # centerXY  <> rect w h # lw thin

boxedEnum x y items =
    let list = enumerate items
    in boxedXY x y (list # centerXY)

titleW = 2.0
titleH = 0.5

boxedTitledEnum1 x y title items =
    let ttext = textBox titleW titleH (title1 title)
        list = enumerate items
    in boxedXY x y (vsep 0 [ttext, boxedXY x (y - titleH) (list # centerXY)])

connect1 = connectOutside' (with & arrowHead .~ tri & headLength .~ global 0.2)

coreOval :: String -> Diagram B
coreOval tag =
    textBox 2.0 0.5 (title1 tag # centerXY)
    <> ellipseXY 1.5 0.75 # fc lightblue # lw medium

-------------------------------------------------------------------------------
-- streamly-core diagram
-------------------------------------------------------------------------------

bigBoxW = 12
bigBoxH = 4
centerBoxW = bigBoxH
centerBoxH = bigBoxH

box1 = boxedXY bigBoxW bigBoxH
box2 = boxedXY centerBoxW centerBoxH

bigBoxSubContent = boxedTitledEnum1 (bigBoxW / 3) (bigBoxH - titleH)

streamsBox :: Diagram B
streamsBox =
    let prod    = bigBoxSubContent "Generation" ["Stream", "StreamK", "Unfold"]
        transf  = bigBoxSubContent "Transformation" ["Scanl"]
        cons    = bigBoxSubContent "Consumption" ["Fold", "Parser", "ParserK"]
        row     = hsep 0 [prod, transf, cons]
        title   = textBox titleW titleH (title1 "Streams")
        content = vsep 0 [title, row # centerXY]
    in box1 (content # centerXY)

arraysBox :: Diagram B
arraysBox =
    let immut  = bigBoxSubContent "Immutable" ["Array", "Array.Generic"]
        mut    = bigBoxSubContent "Mutable" ["MutArray", "MutArray.Generic", "RingArray"]
        ser    = bigBoxSubContent "Serialization" ["MutByteArray", "Unbox", "Serialize"]
        row    = hsep 0 [immut, mut, ser]
        title  = textBox titleW titleH (title1 "Arrays")
        content= vsep 0 [title, row # centerXY]
    in box1 (content # centerXY)

fileSystemBox :: Diagram B
fileSystemBox =
    let items = ["Console.Stdio", "FileIO", "DirIO", "Handle", "Path"]
        content = boxedTitledEnum1 centerBoxW centerBoxH "File System" items
    in box2 (content # centerXY)

otherModulesBox :: Diagram B
otherModulesBox =
    let unicode = boxedTitledEnum1 centerBoxW (centerBoxH * 2/3) "Unicode" ["Parser", "Stream", "String"]
        resmgmt = boxedTitledEnum1 centerBoxW (centerBoxH * 1/3) "Resource Management" ["Control.Exception"]
        content = vsep 0 [unicode, resmgmt]
    in box2 (content # centerXY)

streamlyCore :: Diagram B
streamlyCore =
    let center    = coreOval "streamly-core\n(types and modules)" # named "core"
        topBox    = streamsBox # named "streams"
        bottomBox = arraysBox # named "arrays"
        leftBox   = fileSystemBox # named "fs"
        rightBox  = otherModulesBox # named "others"

        placed = position
          [ (p2 (0,0), center)
          , (p2 (0,4), topBox)
          , (p2 (0,-4), bottomBox)
          , (p2 (-4,0), leftBox)
          , (p2 (4,0), rightBox)
          ]

        arrows = applyAll
          [ connect1 "core" "streams"
          , connect1 "core" "arrays"
          , connect1 "core" "fs"
          , connect1 "core" "others"
          ]
    in arrows placed

-------------------------------------------------------------------------------
-- streamly diagram
-------------------------------------------------------------------------------

concurrentStreamsBox :: Diagram B
concurrentStreamsBox =
    let prod    = bigBoxSubContent "Generation" ["Stream.Prelude"]
        transf  = bigBoxSubContent "Transformation" ["Scanl.Prelude"]
        cons    = bigBoxSubContent "Consumption" ["Fold.Prelude"]
        row     = hsep 0 [prod, transf, cons]
        title   = textBox titleW titleH (title1 "Concurrent Streams")
        content = vsep 0 [title, row # centerXY]
    in box1 (content # centerXY)

networkBox :: Diagram B
networkBox =
    let items = ["Network.Socket", "Network.Inet.TCP"]
        content = boxedTitledEnum1 centerBoxW centerBoxH "Network" items
    in box2 (content # centerXY)

streamly :: Diagram B
streamly =
    let center    = coreOval "streamly" # named "streamly"
        topBox    = concurrentStreamsBox # named "concurrent-streams"
        bottomBox = networkBox # named "network"

        placed = position
          [ (p2 (0,0), center)
          , (p2 (0,4), topBox)
          , (p2 (0,-4), bottomBox)
          ]

        arrows = applyAll
          [ connect1 "streamly" "concurrent-streams"
          , connect1 "streamly" "network"
          ]
    in arrows placed

main :: IO ()
main =
    mainWith
        [ ("streamly-core", (streamlyCore # centerXY # pad 1.1))
        , ("streamly", (streamly # centerXY # pad 1.1))
        ]
