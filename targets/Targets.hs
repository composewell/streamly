module Targets
    ( groupTargets
    , individualTargets
    )
where

import Data.Map (Map)

import qualified Data.Map as Map

groupTargets :: Map String [String]
groupTargets =
    Map.fromList
        [ ( "base_stream_grp"
          , [ "Data.Stream.StreamD"
            , "Data.Stream.StreamK"
            , "Data.Stream.StreamDK"
            ])
        , ("prelude_serial_grp", preludeSerialGrp)
        , ("prelude_concurrent_grp", preludeConcurrentGrp)
        , ( "prelude_other_grp"
          , ["Prelude.Rate", "Prelude.Concurrent", "Prelude.Adaptive"])
        , ( "array_grp"
          , [ "Data.Array"
            , "Data.Array.Foreign"
            , "Data.Array.Prim"
            , "Data.SmallArray"
            , "Data.Array.Prim.Pinned"
            ])
        , ("base_parser_grp", ["Data.Parser.ParserD", "Data.Parser.ParserK"])
        , ("parser_grp", ["Data.Fold", "Data.Parser"])
        , ("list_grp ", [])
        , ( "infinite_grp"
          , preludeSerialGrp ++ preludeConcurrentGrp ++ ["Prelude.Rate"])
        ]

    where

    preludeSerialGrp = ["Prelude.Serial", "Prelude.WSerial", "Prelude.ZipSerial"]
    preludeConcurrentGrp =
        [ "Prelude.Async"
        , "Prelude.WAsync"
        , "Prelude.Ahead"
        , "Prelude.Parallel"
        , "Prelude.ZipAsync"
        ]

individualTargets :: [String]
individualTargets =
    [ "Data.Unfold"
    , "Unicode.Stream"
    , "Unicode.Char"
    , "Unicode.Utf8"
    , "FileSystem.Handle"
    ]
