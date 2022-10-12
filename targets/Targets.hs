module Targets
    ( targets
    )
where

targets :: [(String, [String])]
targets =
    [ -- Base streams
      ("Data.Stream.StreamD",
            [ "base_stream_grp"
            , "base_stream_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.StreamK",
            [ "base_stream_grp"
            , "base_stream_cmp"
            , "noTest"
            ]
      )

    -- Streams
    , ("Data.Stream",
            [ "prelude_serial_grp"
            , "infinite_grp"
            , "serial_wserial_cmp"
            , "serial_async_cmp"
            ]
      )
    , ("Data.Stream.Concurrent",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            , "serial_async_cmp"
            ]
      )
    , ("Data.Stream.ConcurrentEager",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            ]
      )
    , ("Data.Stream.ConcurrentOrdered",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            ]
      )
    , ("Prelude.WSerial", ["serial_wserial_cmp"])
    , ("Prelude.WSerial",
            [ "prelude_serial_grp"
            , "infinite_grp"
            , "serial_wserial_cmp"
            ]
      )
    , ("Prelude.ZipSerial",
            [ "prelude_serial_grp"
            , "infinite_grp"
            ]
      )
    , ("Prelude.Async",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            , "serial_async_cmp"
            ]
      )
    , ("Prelude.WAsync",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            ]
      )
    , ("Prelude.Ahead",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            ]
      )
    , ("Prelude.Parallel",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            ]
      )
    , ("Prelude.ZipAsync",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            ]
      )
    , ("Prelude.Concurrent", [ "prelude_other_grp" ])
    , ("Prelude.Rate",
            [ "prelude_other_grp"
            , "infinite_grp"
            , "testDevOnly"
            ]
      )
    , ("Prelude.Adaptive",
            [ "prelude_other_grp"
            , "noTest"
            ]
      )

    -- Arrays
    , ("Data.Array",
            [ "array_grp"
            , "array_cmp"
            ]
      )
    , ("Data.Array.Unboxed",
            [ "array_grp"
            , "array_cmp"
            , "pinned_array_cmp"
            ]
      )
    , ("Data.Array.Unboxed.Mut",
            [ "array_grp"
            , "array_cmp"
            , "noTest"
            ]
      )

    -- Ring
    , ("Data.Ring.Unboxed", [])

    -- Parsers
    , ("Data.Parser.ParserD",
            [ "base_parser_grp"
            , "base_parser_cmp"
            ]
      )
    , ("Data.Parser.ParserK",
            [ "base_parser_grp"
            , "base_parser_cmp"
            , "noTest"
            ]
      )
    , ("Data.Fold", [ "parser_grp" ])
    , ("Data.Parser", [ "parser_grp" ])

    , ("Data.Unfold", [])
    , ("FileSystem.Handle", [])
    , ("Unicode.Stream", [])
    , ("Unicode.Utf8", ["noTest"])
    , ("Unicode.Char", ["testDevOnly"])

    -- test only, no benchmarks
    , ("Prelude", ["prelude_other_grp", "noBench"])
    , ("Prelude.Fold", ["prelude_other_grp", "noBench"])
    , ("FileSystem.Event", ["noBench"])
    , ("Network.Socket", ["noBench"])
    , ("Network.Inet.TCP", ["noBench"])
    , ("version-bounds", ["noBench"])

    , ("Data.List", ["list_grp", "noBench"])
    , ("Data.List.Base", ["list_grp", "noBench"])
    ]
