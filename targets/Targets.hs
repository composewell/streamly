{-# LANGUAGE CPP #-}

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
    , ("Data.Binary",
            [ "noBench"
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
    , ("Data.RingArray",
            [ "array_grp"
            ]
      )
    , ("Data.Scanl",
            [ "infinite_grp"
            , "fold_parser_grp"
            , "noTest"
            ]
      )
    , ("Data.Scanl.Window",
            [ "infinite_grp"
            , "fold_parser_grp"
            , "noTest"
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
    , ("Data.SmallArray",
            [ "noBench"
            , "testDevOnly"
            ]
      )
    , ("Data.Stream",
            [ "infinite_grp"
            , "serial_stream_grp"
            , "serial_stream_cmp"
            , "serial_concurrent_cmp"
            ]
      )
    , ("Data.Stream.Adaptive",
            [ "concurrent_stream_grp"
            , "noTest"
            ]
      )
    -- XXX Concurrent and rate benchmarks take a long time with --long, perhaps
    -- need to use smaller stream sizes for these when using --long
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
    , ("Data.Stream.ConcurrentThreadHeavy",
            [ "concurrent_stream_grp"
            , "noTest"
            ]
      )
    , ("Data.Stream.Rate",
            [ "infinite_grp"
            , "concurrent_stream_grp"
            , "testDevOnly"
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
#if defined(darwin_HOST_OS)
    , ("FileSystem.Event.Darwin",
            [ "noBench"
            ]
      )
#endif
#if defined(linux_HOST_OS)
    , ("FileSystem.Event.Linux",
            [ "noBench"
            , "testDevOnly"
            ]
      )
#endif
#if defined(mingw32_HOST_OS)
    , ("FileSystem.Event.Windows",
            [ "noBench"
            ]
      )
#endif
    , ("FileSystem.Handle", [])
    , ("FileSystem.DirIO", [])
    , ("Network.Inet.TCP", ["noBench"])
    , ("Network.Socket", ["noBench"])
    , ("Unicode.Char", ["testDevOnly"])
    , ("Unicode.Parser", [])
    , ("Unicode.Stream", [])
    , ("Unicode.Utf8", ["noTest"])
    , ("version-bounds", ["noBench"])
    ]
