module Targets
    ( targets
    )
where

-- Special tags
-- noTest
-- noBench
-- testDevOnly
targets :: [(String, [String])]
targets =
    [ -- Base streams
      ("Data.Stream",
            [ "base_stream_grp"
            , "base_stream_cmp"
            , "noTest"
            ]
      )
    , ("Data.StreamK",
            [ "base_stream_grp"
            , "base_stream_cmp"
            , "noTest"
            ]
      )

{-
    -- XXX Need devOnly flag support in BenchRunner
    , ("Data.Stream.ToStreamK",
            [ "noTest"
            , "devOnly"
            ]
      )
-}

    -- Streams
    , ("Data.Stream",
            [ "prelude_serial_grp"
            , "infinite_grp"
            , "serial_wserial_cmp"
            , "serial_async_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.StreamDK",
            [ "prelude_serial_grp"
            , "infinite_grp"
            , "noTest"
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
            , "noTest"
            ]
      )
    , ("Data.Stream.ConcurrentOrdered",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.ConcurrentInterleaved",
            [ "prelude_concurrent_grp"
            , "infinite_grp"
            , "concurrent_cmp"
            , "noTest"
            ]
      )
    , ("Data.Array.Stream",
            [ "prelude_serial_grp"
            , "infinite_grp"
            ]
      )
    -- Enabled only when use-prelude flag is set
    -- , ("Prelude.Serial",
    --         [ "prelude_serial_grp"
    --         , "infinite_grp"
    --         , "serial_wserial_cmp"
    --         , "noBench"
    --         ]
    --   )
    -- , ("Prelude.Top",
    --         [ "prelude_serial_grp"
    --         , "infinite_grp"
    --         , "noBench"
    --         ]
    --   )
    -- , ("Prelude.WSerial",
    --         [ "prelude_serial_grp"
    --         , "infinite_grp"
    --         , "serial_wserial_cmp"
    --         ]
    --   )
    -- , ("Prelude.Merge",
    --         [ "prelude_serial_grp"
    --         , "infinite_grp"
    --         , "noTest"
    --         ]
    --   )
    -- , ("Prelude.ZipSerial",
    --         [ "prelude_serial_grp"
    --         , "infinite_grp"
    --         ]
    --   )
    -- , ("Prelude.Async",
    --         [ "prelude_concurrent_grp"
    --         , "infinite_grp"
    --         , "concurrent_cmp"
    --         , "serial_async_cmp"
    --         ]
    --   )
    -- , ("Prelude.WAsync",
    --         [ "prelude_concurrent_grp"
    --         , "infinite_grp"
    --         , "concurrent_cmp"
    --         ]
    --   )
    -- , ("Prelude.Ahead",
    --         [ "prelude_concurrent_grp"
    --         , "infinite_grp"
    --         , "concurrent_cmp"
    --         ]
    --   )
    -- , ("Prelude.Parallel",
    --         [ "prelude_concurrent_grp"
    --         , "infinite_grp"
    --         , "concurrent_cmp"
    --         ]
    --   )
    -- , ("Prelude.ZipAsync",
    --         [ "prelude_concurrent_grp"
    --         , "infinite_grp"
    --         ]
    --   )
    -- , ("Prelude.Concurrent", [ "prelude_other_grp" ])
    -- , ("Prelude.Rate",
    --         [ "prelude_other_grp"
    --         , "infinite_grp"
    --         , "testDevOnly"
    --         ]
    --   )
    -- , ("Prelude.Adaptive",
    --         [ "prelude_other_grp"
    --         , "noTest"
    --         ]
    --   )

    -- Arrays
    , ("Data.Array.Generic",
            [ "array_grp"
            , "array_cmp"
            ]
      )
    , ("Data.Array",
            [ "array_grp"
            , "array_cmp"
            , "pinned_array_cmp"
            ]
      )
    , ("Data.MutArray",
            [ "array_grp"
            , "array_cmp"
            ]
      )

    -- Ring
    , ("Data.Ring.Unboxed", [])

    -- Parsers
    , ("Data.ParserK",
            [ "base_parser_grp"
            , "base_parser_cmp"
            , "infinite_grp"
            , "noTest"
            ]
      )
    , ("Data.Fold", [ "parser_grp", "infinite_grp" ])
    , ("Data.Fold.Window", [ "parser_grp", "infinite_grp" ])
    , ("Data.Parser", [ "parser_grp", "infinite_grp" ])

    , ("Data.Unbox", ["noTest"])
    , ("Data.Unbox.TH", ["noBench"])
    , ("Data.Unbox.Derive.Generic", ["noBench"])
    , ("Data.Unbox.Derive.TH", [])
    , ("Data.Serialize", [])
    , ("Data.Serialize.Derive.TH", ["noBench"])
    , ("Data.Serialize.ENABLE_constructorTagAsString", ["noBench"])
    , ("Data.Unfold", ["infinite_grp"])
    , ("FileSystem.Handle", [])
    , ("Unicode.Stream", [])
    , ("Unicode.Utf8", ["noTest"])
    , ("Unicode.Char", ["testDevOnly"])

    -- test only, no benchmarks
    -- , ("Prelude", ["prelude_other_grp", "noBench"])
    -- , ("Prelude.Fold", ["prelude_other_grp", "noBench"])
    , ("FileSystem.Event", ["noBench"])
    , ("Network.Socket", ["noBench"])
    , ("Network.Inet.TCP", ["noBench"])
    , ("version-bounds", ["noBench"])

    , ("Data.List", ["list_grp", "noBench", "testDevOnly"])
    , ("Data.List.Base", ["list_grp", "noBench"])
    ]
