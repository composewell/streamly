module Targets
    ( targets
    )
where

-- Special tags:
-- *_grp groups
-- *_cmp comparison groups
-- noTest not available in tests
-- noBench not available in bench
-- testDevOnly dev only test
targets :: [(String, [String])]
targets =
    [
      -------------------------------------------------------------------------
      -- Alphabetical order
      -------------------------------------------------------------------------

      ("Data.Array",
            [ "array_grp"
            , "array_cmp"
            ]
      )
    , ("Data.Array.Generic",
            [ "array_grp"
            , "array_cmp"
            ]
      )
    , ("Data.Array.Stream",
            [ "infinite_grp"
            , "serial_stream_grp"
            ]
      )
    , ("Data.Fold",
            [ "infinite_grp"
            , "fold_parser_grp"
            ]
      )
    , ("Data.Fold.Window",
            [ "infinite_grp"
            , "fold_parser_grp"
            ]
      )
    , ("Data.List",
            [ "list_grp"
            , "noBench"
            , "testDevOnly"
            ]
      )
    , ("Data.List.Base",
            [ "list_grp"
            , "noBench"
            ]
      )
    , ("Data.MutArray",
            [ "array_grp"
            , "array_cmp"
            ]
      )
    , ("Data.Parser",
            [ "infinite_grp"
            , "fold_parser_grp"
            , "parser_cmp"
            ]
      )
    , ("Data.ParserK",
            [ "infinite_grp"
            , "fold_parser_grp"
            , "parser_cmp"
            , "noTest"
            ]
      )
    , ("Data.ParserK.Chunked",
            [ "infinite_grp"
            , "fold_parser_grp"
            , "parser_cmp"
            , "noTest"
            ]
      )
    , ("Data.ParserK.Chunked.Generic",
            [ "infinite_grp"
            , "fold_parser_grp"
            , "parser_cmp"
            , "noTest"
            ]
      )
    , ("Data.Ring.Unboxed",
            [ "array_grp"
            ]
      )
    , ("Data.Serialize",
            [ "mut_bytearray_grp"
            ]
      )
    , ("Data.Serialize.Derive.TH",
            [ "mut_bytearray_grp"
            , "noBench"
            ]
      )
    , ("Data.Serialize.ENABLE_constructorTagAsString",
            [ "mut_bytearray_grp"
            , "noBench"
            ]
      )
    , ("Data.Stream",
            [ "infinite_grp"
            , "serial_stream_grp"
            , "serial_stream_cmp"
            , "serial_concurrent_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.Concurrent",
            [ "infinite_grp"
            , "concurrent_stream_grp"
            , "concurrent_stream_cmp"
            , "serial_concurrent_cmp"
            ]
      )
    , ("Data.Stream.ConcurrentEager",
            [ "infinite_grp"
            , "concurrent_stream_grp"
            , "concurrent_stream_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.ConcurrentInterleaved",
            [ "infinite_grp"
            , "concurrent_stream_grp"
            , "concurrent_stream_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.ConcurrentOrdered",
            [ "infinite_grp"
            , "concurrent_stream_grp"
            , "concurrent_stream_cmp"
            , "noTest"
            ]
      )
    , ("Data.Stream.Rate",
            [ "infinite_grp"
            , "concurrent_stream_grp"
            ]
      )
    , ("Data.StreamK",
            [ "infinite_grp"
            , "serial_stream_grp"
            , "serial_stream_cmp"
            , "noTest"
            ]
      )
{-
    -- XXX Need devOnly flag support in BenchRunner
    , ("Data.StreamK.FromStream",
            [ "infinite_grp"
            , "serial_stream_grp"
            , "noTest"
            , "devOnly"
            ]
      )
-}
    , ("Data.Unbox",
            [ "noTest"
            ]
      )
    , ("Data.Unbox.Derive.Generic",
            [ "noBench"
            ]
      )
    , ("Data.Unbox.Derive.TH",
            []
      )
    , ("Data.Unbox.TH",
            [ "noBench"
            ]
      )
    , ("Data.Unfold",
            [ "infinite_grp"
            , "serial_stream_grp"
            ]
      )
    , ("FileSystem.Event",
            [ "noBench"
            ]
      )
    -- XXX Use Linux os macro
    , ("FileSystem.Event.Linux",
            [ "noBench"
            ]
      )
    , ("FileSystem.Handle", [])
    , ("Network.Inet.TCP", ["noBench"])
    , ("Network.Socket", ["noBench"])
    , ("Unicode.Char", ["testDevOnly"])
    , ("Unicode.Parser", [])
    , ("Unicode.Stream", [])
    , ("Unicode.Utf8", ["noTest"])
    , ("version-bounds", ["noBench"])

    ---- DEPRECATED ----

    -- test only, no benchmarks
    -- , ("Prelude", ["prelude_other_grp", "noBench"])
    -- , ("Prelude.Fold", ["prelude_other_grp", "noBench"])
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
    ]
