#ifndef PARSER_WITH_POS
#define PARSE_BREAK parseBreak
#define PARSE_BREAK_STREAMK parseBreakStreamK
#define PARSE_BREAK_CHUNKS parseBreakChunks
#define PARSE_BREAK_CHUNKS_GENERIC parseBreakChunksGeneric
#define PARSE_MANY parseMany
#define PARSE_ITERATE parseIterate
#define OPTIONAL(x)
#define PARSE_ERROR(x) ParseError
#define PARSE_ERROR_TYPE ParseError
#else
#undef PARSE_BREAK
#define PARSE_BREAK parseBreakPos
#undef PARSE_BREAK_STREAMK
#define PARSE_BREAK_STREAMK parseBreakStreamKPos
#undef PARSE_BREAK_CHUNKS
#define PARSE_BREAK_CHUNKS parseBreakChunksPos
#undef PARSE_BREAK_CHUNKS_GENERIC
#define PARSE_BREAK_CHUNKS_GENERIC parseBreakChunksGenericPos

#define ParseChunksState ParseChunksStatePos
#define ParseChunksInit ParseChunksInitPos
#define ParseChunksInitBuf ParseChunksInitBufPos
#define ParseChunksInitLeftOver ParseChunksInitLeftOverPos
#define ParseChunksStream ParseChunksStreamPos
#define ParseChunksStop ParseChunksStopPos
#define ParseChunksBuf ParseChunksBufPos
#define ParseChunksExtract ParseChunksExtractPos
#define ParseChunksYield ParseChunksYieldPos

#undef PARSE_MANY
#define PARSE_MANY parseManyPos

#define ConcatParseState ConcatParseStatePos
#define ConcatParseInit ConcatParseInitPos
#define ConcatParseInitBuf ConcatParseInitBufPos
#define ConcatParseInitLeftOver ConcatParseInitLeftOverPos
#define ConcatParseStop ConcatParseStopPos
#define ConcatParseStream ConcatParseStreamPos
#define ConcatParseBuf ConcatParseBufPos
#define ConcatParseExtract ConcatParseExtractPos
#define ConcatParseYield ConcatParseYieldPos

#undef PARSE_ITERATE
#define PARSE_ITERATE parseIteratePos
#undef OPTIONAL
#define OPTIONAL(x) (x)
#undef PARSE_ERROR
#define PARSE_ERROR(x) ParseErrorPos (x)
#undef PARSE_ERROR_TYPE
#define PARSE_ERROR_TYPE ParseErrorPos
#endif

{- HLINT ignore -}

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit OPTIONAL(Int) inpBuf st
    | ParseChunksInitBuf OPTIONAL(Int) inpBuf
    | ParseChunksInitLeftOver OPTIONAL(Int) inpBuf
    | ParseChunksStream OPTIONAL(Int) st inpBuf !pst
    | ParseChunksStop OPTIONAL(Int) inpBuf !pst
    | ParseChunksBuf OPTIONAL(Int) inpBuf st inpBuf !pst
    | ParseChunksExtract OPTIONAL(Int) inpBuf inpBuf !pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

-- XXX return the remaining stream as part of the error.
{-# INLINE_NORMAL PARSE_MANY #-}
PARSE_MANY
    :: Monad m
    => PRD.Parser a m b
    -> Stream m a
    -> Stream m (Either PARSE_ERROR_TYPE b)
PARSE_MANY (PRD.Parser pstep initial extract) (Stream step state) =
    Stream stepOuter (ParseChunksInit OPTIONAL(0) [] state)

    where

    {-# INLINE splitAt #-}
    splitAt = Stream.splitAt "Data.StreamK.parseMany"

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit OPTIONAL(i) [] st) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                res <- initial
                case res of
                    PRD.IPartial ps ->
                        return $ Skip $ ParseChunksBuf OPTIONAL(i) [x] s [] ps
                    PRD.IDone pb ->
                        let next = ParseChunksInit OPTIONAL(i) [x] s
                         in return $ Skip $ ParseChunksYield (Right pb) next
                    PRD.IError err ->
                        return
                            $ Skip
                            $ ParseChunksYield
                                (Left (PARSE_ERROR(i) err))
                                (ParseChunksInitLeftOver OPTIONAL(i) [])
            Skip s -> return $ Skip $ ParseChunksInit OPTIONAL(i) [] s
            Stop   -> return Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit OPTIONAL(i) src st) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ParseChunksBuf OPTIONAL(i) src st [] ps
            PRD.IDone pb ->
                let next = ParseChunksInit OPTIONAL(i) src st
                 in return $ Skip $ ParseChunksYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (PARSE_ERROR(i) err))
                        (ParseChunksInitLeftOver OPTIONAL(i) [])

    -- This is simplified ParseChunksInit
    stepOuter _ (ParseChunksInitBuf OPTIONAL(i) src) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ParseChunksExtract OPTIONAL(i) src [] ps
            PRD.IDone pb ->
                let next = ParseChunksInitBuf OPTIONAL(i) src
                 in return $ Skip $ ParseChunksYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (PARSE_ERROR(i) err))
                        (ParseChunksInitLeftOver OPTIONAL(i) [])

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver OPTIONAL(_) _) = return Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream OPTIONAL(i) st buf pst) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.SPartial 1 pst1 ->
                        return $ Skip $ ParseChunksStream OPTIONAL(i + 1) s [] pst1
                    PR.SPartial m pst1 -> do
                        let n = 1 - m
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf OPTIONAL(i + m) src s [] pst1
                    PR.SContinue 1 pst1 ->
                        return $ Skip $ ParseChunksStream OPTIONAL(i + 1) s (x:buf) pst1
                    PR.SContinue m pst1 -> do
                        let n = 1 - m
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf OPTIONAL(i + m) src s buf1 pst1
                    PR.SDone 1 b -> do
                        return $ Skip $
                            ParseChunksYield
                                (Right b) (ParseChunksInit OPTIONAL(i + 1) [] s)
                    PR.SDone m b -> do
                        let n = 1 - m
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ParseChunksYield
                                (Right b) (ParseChunksInit OPTIONAL(i + m) src s)
                    PR.SError err ->
                        return
                            $ Skip
                            $ ParseChunksYield
                                (Left (PARSE_ERROR(i + 1) err))
                                (ParseChunksInitLeftOver OPTIONAL(i + 1) [])
            Skip s -> return $ Skip $ ParseChunksStream OPTIONAL(i) s buf pst
            Stop -> return $ Skip $ ParseChunksStop OPTIONAL(i) buf pst

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf OPTIONAL(i) [] s buf pst) =
        return $ Skip $ ParseChunksStream OPTIONAL(i) s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf OPTIONAL(i) (x:xs) s buf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                return $ Skip $ ParseChunksBuf OPTIONAL(i + 1) xs s [] pst1
            PR.SPartial m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf OPTIONAL(i + m) src s [] pst1
            PR.SContinue 1 pst1 ->
                return $ Skip $ ParseChunksBuf OPTIONAL(i + 1) xs s (x:buf) pst1
            PR.SContinue m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf OPTIONAL(i + m) src s buf1 pst1
            PR.SDone 1 b ->
                return
                    $ Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit OPTIONAL(i + 1) xs s)
            PR.SDone m b -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip
                    $ ParseChunksYield
                        (Right b) (ParseChunksInit OPTIONAL(i + m) src s)
            PR.SError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (PARSE_ERROR(i + 1) err))
                        (ParseChunksInitLeftOver OPTIONAL(i + 1) [])

    -- This is simplified ParseChunksBuf
    stepOuter _ (ParseChunksExtract OPTIONAL(i) [] buf pst) =
        return $ Skip $ ParseChunksStop OPTIONAL(i) buf pst

    stepOuter _ (ParseChunksExtract OPTIONAL(i) (x:xs) buf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                return $ Skip $ ParseChunksExtract OPTIONAL(i + 1) xs [] pst1
            PR.SPartial m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksExtract OPTIONAL(i + m) src [] pst1
            PR.SContinue 1 pst1 ->
                return $ Skip $ ParseChunksExtract OPTIONAL(i + 1) xs (x:buf) pst1
            PR.SContinue m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksExtract OPTIONAL(i + m) src buf1 pst1
            PR.SDone 1 b ->
                return
                    $ Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf OPTIONAL(i + 1) xs)
            PR.SDone m b -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return
                    $ Skip
                    $ ParseChunksYield
                        (Right b) (ParseChunksInitBuf OPTIONAL(i + m) src)
            PR.SError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (PARSE_ERROR(i + 1) err))
                        (ParseChunksInitLeftOver OPTIONAL(i + 1) [])

    -- This is simplified ParseChunksExtract
    stepOuter _ (ParseChunksStop OPTIONAL(i) buf pst) = do
        pRes <- extract pst
        case pRes of
            PR.FContinue 0 pst1 ->
                return $ Skip $ ParseChunksStop OPTIONAL(i) buf pst1
            PR.FContinue m pst1 -> do
                let n = (- m)
                assert (n <= length buf) (return ())
                let (src0, buf1) = splitAt n buf
                    src  = Prelude.reverse src0
                return $ Skip $ ParseChunksExtract OPTIONAL(i + m) src buf1 pst1
            PR.FDone 0 b -> do
                return $ Skip $
                    ParseChunksYield (Right b) (ParseChunksInitLeftOver OPTIONAL(i) [])
            PR.FDone m b -> do
                let n = (- m)
                assert (n <= length buf) (return ())
                let src = Prelude.reverse (Prelude.take n buf)
                return $ Skip $
                    ParseChunksYield (Right b) (ParseChunksInitBuf OPTIONAL(i + m) src)
            PR.FError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (PARSE_ERROR(i) err))
                        (ParseChunksInitLeftOver OPTIONAL(i) [])

    stepOuter _ (ParseChunksYield a next) = return $ Yield a next

{-# ANN type ConcatParseState Fuse #-}
data ConcatParseState c b inpBuf st p m a =
      ConcatParseInit OPTIONAL(Int) inpBuf st p
    | ConcatParseInitBuf OPTIONAL(Int) inpBuf p
    | ConcatParseInitLeftOver OPTIONAL(Int) inpBuf
    | forall s. ConcatParseStop OPTIONAL(Int)
        inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Final s b))
    | forall s. ConcatParseStream OPTIONAL(Int)
        st inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Final s b))
    | forall s. ConcatParseBuf OPTIONAL(Int)
        inpBuf st inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Final s b))
    | forall s. ConcatParseExtract OPTIONAL(Int)
        inpBuf inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Final s b))
    | ConcatParseYield c (ConcatParseState c b inpBuf st p m a)

{-# INLINE_NORMAL PARSE_ITERATE #-}
PARSE_ITERATE
    :: Monad m
    => (b -> PRD.Parser a m b)
    -> b
    -> Stream m a
    -> Stream m (Either PARSE_ERROR_TYPE b)
PARSE_ITERATE func seed (Stream step state) =
    Stream stepOuter (ConcatParseInit OPTIONAL(0) [] state (func seed))

    where

    {-# INLINE splitAt #-}
    splitAt = Stream.splitAt "Data.StreamK.parseIterate"

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, go to stream processing loop
    stepOuter _ (ConcatParseInit OPTIONAL(i) [] st (PRD.Parser pstep initial extract)) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ConcatParseStream OPTIONAL(i) st [] pstep ps extract
            PRD.IDone pb ->
                let next = ConcatParseInit OPTIONAL(i) [] st (func pb)
                 in return $ Skip $ ConcatParseYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (PARSE_ERROR(i) err))
                        (ConcatParseInitLeftOver OPTIONAL(i) [])

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ConcatParseInit OPTIONAL(i) src st
                    (PRD.Parser pstep initial extract)) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ConcatParseBuf OPTIONAL(i) src st [] pstep ps extract
            PRD.IDone pb ->
                let next = ConcatParseInit OPTIONAL(i) src st (func pb)
                 in return $ Skip $ ConcatParseYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (PARSE_ERROR(i) err))
                        (ConcatParseInitLeftOver OPTIONAL(i) [])

    -- This is simplified ConcatParseInit
    stepOuter _ (ConcatParseInitBuf OPTIONAL(i) src
                    (PRD.Parser pstep initial extract)) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ConcatParseExtract OPTIONAL(i) src [] pstep ps extract
            PRD.IDone pb ->
                let next = ConcatParseInitBuf OPTIONAL(i) src (func pb)
                 in return $ Skip $ ConcatParseYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (PARSE_ERROR(i) err))
                        (ConcatParseInitLeftOver OPTIONAL(i) [])

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ConcatParseInitLeftOver OPTIONAL(_) _) = return Stop

    -- Buffer is empty process elements from the stream
    stepOuter gst (ConcatParseStream OPTIONAL(i) st buf pstep pst extract) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.SPartial 1 pst1 ->
                        return $ Skip
                            $ ConcatParseStream OPTIONAL(i + 1) s [] pstep pst1 extract
                    PR.SPartial m pst1 -> do
                        let n = 1 - m
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip
                            $ ConcatParseBuf
                                OPTIONAL(i + m) src s [] pstep pst1 extract
                    -- PR.SContinue 1 pst1 ->
                    --     return $ Skip $ ConcatParseStream s (x:buf) pst1
                    PR.SContinue m pst1 -> do
                        let n = 1 - m
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip
                            $ ConcatParseBuf
                                OPTIONAL(i + m) src s buf1 pstep pst1 extract
                    -- XXX Specialize for Stop 0 common case?
                    PR.SDone m b -> do
                        let n = 1 - m
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip
                            $ ConcatParseYield
                                (Right b)
                                (ConcatParseInit OPTIONAL(i + m) src s (func b))
                    PR.SError err ->
                        return
                            $ Skip
                            $ ConcatParseYield
                                (Left (PARSE_ERROR(i + 1) err))
                                (ConcatParseInitLeftOver OPTIONAL(i + 1) [])
            Skip s ->
                return $ Skip $ ConcatParseStream OPTIONAL(i) s buf pstep pst extract
            Stop -> return $ Skip $ ConcatParseStop OPTIONAL(i) buf pstep pst extract

    -- go back to stream processing mode
    stepOuter _ (ConcatParseBuf OPTIONAL(i) [] s buf pstep ps extract) =
        return $ Skip $ ConcatParseStream OPTIONAL(i) s buf pstep ps extract

    -- buffered processing loop
    stepOuter _ (ConcatParseBuf OPTIONAL(i) (x:xs) s buf pstep pst extract) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                return $ Skip
                    $ ConcatParseBuf OPTIONAL(i + 1) xs s [] pstep pst1 extract
            PR.SPartial m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip
                    $ ConcatParseBuf OPTIONAL(i + m) src s [] pstep pst1 extract
         -- PR.SContinue 1 pst1 -> return $ Skip $ ConcatParseBuf xs s (x:buf) pst1
            PR.SContinue m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip
                    $ ConcatParseBuf OPTIONAL(i + m) src s buf1 pstep pst1 extract
            -- XXX Specialize for Stop 0 common case?
            PR.SDone m b -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip
                    $ ConcatParseYield
                        (Right b) (ConcatParseInit OPTIONAL(i + m) src s (func b))
            PR.SError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (PARSE_ERROR(i + 1) err))
                        (ConcatParseInitLeftOver OPTIONAL(i + 1) [])

    -- This is simplified ConcatParseBuf
    stepOuter _ (ConcatParseExtract OPTIONAL(i) [] buf pstep pst extract) =
        return $ Skip $ ConcatParseStop OPTIONAL(i) buf pstep pst extract

    stepOuter _ (ConcatParseExtract OPTIONAL(i) (x:xs) buf pstep pst extract) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                return $ Skip
                    $ ConcatParseExtract OPTIONAL(i + 1) xs [] pstep pst1 extract
            PR.SPartial m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip
                    $ ConcatParseExtract OPTIONAL(i + m) src [] pstep pst1 extract
            PR.SContinue 1 pst1 ->
                return $ Skip
                    $ ConcatParseExtract OPTIONAL(i + 1) xs (x:buf) pstep pst1 extract
            PR.SContinue m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip
                    $ ConcatParseExtract OPTIONAL(i + m) src buf1 pstep pst1 extract
            PR.SDone 1 b ->
                 return $ Skip
                    $ ConcatParseYield
                        (Right b) (ConcatParseInitBuf OPTIONAL(i + 1) xs (func b))
            PR.SDone m b -> do
                let n = 1 - m
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip
                    $ ConcatParseYield
                        (Right b) (ConcatParseInitBuf OPTIONAL(i + m) src (func b))
            PR.SError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (PARSE_ERROR(i + 1) err))
                        (ConcatParseInitLeftOver OPTIONAL(i + 1) [])

    -- This is simplified ConcatParseExtract
    stepOuter _ (ConcatParseStop OPTIONAL(i) buf pstep pst extract) = do
        pRes <- extract pst
        case pRes of
            PR.FContinue 0 pst1 ->
                return $ Skip $ ConcatParseStop OPTIONAL(i) buf pstep pst1 extract
            PR.FContinue m pst1 -> do
                let n = (- m)
                assert (n <= length buf) (return ())
                let (src0, buf1) = splitAt n buf
                    src  = Prelude.reverse src0
                return $ Skip
                    $ ConcatParseExtract OPTIONAL(i + m) src buf1 pstep pst1 extract
            PR.FDone 0 b -> do
                return $ Skip $
                    ConcatParseYield (Right b) (ConcatParseInitLeftOver OPTIONAL(i) [])
            PR.FDone m b -> do
                let n = (- m)
                assert (n <= length buf) (return ())
                let src = Prelude.reverse (Prelude.take n buf)
                return $ Skip $
                    ConcatParseYield
                        (Right b) (ConcatParseInitBuf OPTIONAL(i + m) src (func b))
            PR.FError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (PARSE_ERROR(i) err))
                        (ConcatParseInitLeftOver OPTIONAL(i) [])

    stepOuter _ (ConcatParseYield a next) = return $ Yield a next

{-# INLINE PARSE_BREAK #-}
PARSE_BREAK :: Monad m =>
    PR.Parser a m b -> Stream m a -> m (Either PARSE_ERROR_TYPE b, Stream m a)
PARSE_BREAK (PRD.Parser pstep initial extract) stream@(Stream step state) = do
    res <- initial
    case res of
        PRD.IPartial s ->
            go SPEC state (List []) s OPTIONAL(0)
            -- Using go0 does improve alt and manyTill benchmarks dramatically
            -- but also degrades the split/monad benchmarks equally. Needs more
            -- investigation.
            -- go0 SPEC state s COUNT(0)
        PRD.IDone b -> return (Right b, stream)
        PRD.IError err -> return (Left (PARSE_ERROR(0) err), stream)

    where

    {-# INLINE splitAt #-}
    splitAt = Stream.splitAt "Data.Stream.parseBreak"

    -- "buf" contains last few items in the stream that we may have to
    -- backtrack to.
    --
    -- XXX currently we are using a dumb list based approach for backtracking
    -- buffer. This can be replaced by a sliding/ring buffer using Data.Array.
    -- That will allow us more efficient random back and forth movement.
    go !_ st buf !pst OPTIONAL(i) = do
        r <- step defState st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.SPartial 1 pst1 -> go SPEC s (List []) pst1 OPTIONAL(i+1)
                        -- go0 SPEC s pst1 (i + 1)
                    PR.SPartial 0 pst1 -> go1 SPEC s x pst1 OPTIONAL(i)
                    PR.SPartial m pst1 -> do
                        let n = 1 - m
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List []) (List src) pst1 OPTIONAL(i+m)
                    PR.SContinue 1 pst1 ->
                        go SPEC s (List (x:getList buf)) pst1 OPTIONAL(i+1)
                    PR.SContinue 0 pst1 -> gobuf SPEC s buf (List [x]) pst1 OPTIONAL(i)
                    PR.SContinue m pst1 -> do
                        let n = 1 - m
                        assert (n <= length (x:getList buf)) (return ())
                        let (src0, buf1) = splitAt n (x:getList buf)
                            src  = Prelude.reverse src0
                        gobuf SPEC s (List buf1) (List src) pst1 OPTIONAL(i+m)
                    PR.SDone 1 b -> return (Right b, Stream step s)
                    PR.SDone m b -> do
                        let n = 1 - m
                        assert (n <= length (x:getList buf)) (return ())
                        let src0 = Prelude.take n (x:getList buf)
                            src  = Prelude.reverse src0
                        -- XXX This would make it quadratic. We should probably
                        -- use StreamK if we have to append many times.
                        return
                            ( Right b,
                              Nesting.append (fromList src) (Stream step s))
                    PR.SError err -> do
                        let src = Prelude.reverse $ x:getList buf
                        return
                            ( Left (PARSE_ERROR(i+1) err)
                            , Nesting.append (fromList src) (Stream step s)
                            )

            Skip s -> go SPEC s buf pst OPTIONAL(i)
            Stop -> goStop SPEC buf pst OPTIONAL(i)

    {-
    go0 !_ st !pst i = do
        r <- step defState st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.SPartial 1 pst1 -> go0 SPEC s pst1 (i + 1)
                    PR.SPartial 0 pst1 -> go1 SPEC s x pst1 i
                    PR.SPartial _ _ -> error "Unreachable"
                    PR.SContinue 1 pst1 -> go SPEC s (List [x]) pst1 (i + 1)
                    PR.SContinue 0 pst1 -> go1 SPEC s x pst1 i
                    PR.SContinue _ _ -> error "Unreachable"
                    PR.SDone 1 b -> return (Right b, Stream step s)
                    PR.SDone 0 b ->
                        return ( Right b, StreamD.cons x (Stream step s))
                    PR.SDone _ _ -> error "Unreachable"
                    PR.SError err -> do
                        return
                            ( Left (PARSE_ERROR(i + 1) err)
                            , StreamD.cons x (Stream step s)
                            )

            Skip s -> go0 SPEC s pst i
            Stop -> goStop SPEC (List []) pst i
    -}

    go1 !_ s x !pst OPTIONAL(i) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                -- go0 SPEC s pst1 OPTIONAL(i + 1)
                go SPEC s (List []) pst1 OPTIONAL(i + 1)
            PR.SPartial 0 pst1 -> do
                go1 SPEC s x pst1 OPTIONAL(i)
            PR.SPartial m _ ->
                error $ "parseBreak: parser bug, go1: Partial m = " ++ show m
            PR.SContinue 1 pst1 ->
                go SPEC s (List [x]) pst1 OPTIONAL(i + 1)
            PR.SContinue 0 pst1 ->
                go1 SPEC s x pst1 OPTIONAL(i)
            PR.SContinue m _ -> do
                error $ "parseBreak: parser bug, go1: Continue m = " ++ show m
            PR.SDone 1 b -> do
                return (Right b, Stream step s)
            PR.SDone 0 b -> do
                return (Right b, StreamD.cons x (Stream step s))
            PR.SDone m _ -> do
                error $ "parseBreak: parser bug, go1: SDone m = " ++ show m
            PR.SError err ->
                return
                    ( Left (PARSE_ERROR(i + 1) err)
                    , Nesting.append (fromPure x) (Stream step s)
                    )

    -- gobuf !_ s (List []) (List []) !pst i = go0 SPEC s pst i
    gobuf !_ s buf (List []) !pst OPTIONAL(i) = go SPEC s buf pst OPTIONAL(i)
    gobuf !_ s buf (List (x:xs)) !pst OPTIONAL(i) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                gobuf SPEC s (List []) (List xs) pst1 OPTIONAL(i + 1)
            PR.SPartial m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List []) (List src) pst1 OPTIONAL(i + m)
            PR.SContinue 1 pst1 ->
                gobuf SPEC s (List (x:getList buf)) (List xs) pst1 OPTIONAL(i + 1)
            PR.SContinue 0 pst1 ->
                gobuf SPEC s buf (List (x:xs)) pst1 OPTIONAL(i)
            PR.SContinue m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                gobuf SPEC s (List buf1) (List src) pst1 OPTIONAL(i + m)
            PR.SDone m b -> do
                let n = 1 - m
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                return (Right b, Nesting.append (fromList src) (Stream step s))
            PR.SError err -> do
                let src = Prelude.reverse (getList buf) ++ x:xs
                return
                    ( Left (PARSE_ERROR(i + 1) err)
                    , Nesting.append (fromList src) (Stream step s)
                    )

    -- This is simplified gobuf
    goExtract !_ buf (List []) !pst OPTIONAL(i) = goStop SPEC buf pst OPTIONAL(i)
    goExtract !_ buf (List (x:xs)) !pst OPTIONAL(i) = do
        pRes <- pstep pst x
        case pRes of
            PR.SPartial 1 pst1 ->
                goExtract SPEC (List []) (List xs) pst1 OPTIONAL(i + 1)
            PR.SPartial m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC (List []) (List src) pst1 OPTIONAL(i + m)
            PR.SContinue 1 pst1 ->
                goExtract SPEC (List (x:getList buf)) (List xs) pst1 OPTIONAL(i + 1)
            PR.SContinue 0 pst1 ->
                goExtract SPEC buf (List (x:xs)) pst1 OPTIONAL(i)
            PR.SContinue m pst1 -> do
                let n = 1 - m
                assert (n <= length (x:getList buf)) (return ())
                let (src0, buf1) = splitAt n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                goExtract SPEC (List buf1) (List src) pst1 OPTIONAL(i + m)
            PR.SDone m b -> do
                let n = 1 - m
                assert (n <= length (x:getList buf)) (return ())
                let src0 = Prelude.take n (x:getList buf)
                    src  = Prelude.reverse src0 ++ xs
                return (Right b, fromList src)
            PR.SError err -> do
                let src = Prelude.reverse (getList buf) ++ x:xs
                return (Left (PARSE_ERROR(i + 1) err), fromList src)

    -- This is simplified goExtract
    {-# INLINE goStop #-}
    goStop _ buf pst OPTIONAL(i) = do
        pRes <- extract pst
        case pRes of
            PR.FContinue 0 pst1 -> goStop SPEC buf pst1 OPTIONAL(i)
            PR.FContinue m pst1 -> do
                let n = (- m)
                assert (n <= length (getList buf)) (return ())
                let (src0, buf1) = splitAt n (getList buf)
                    src = Prelude.reverse src0
                goExtract SPEC (List buf1) (List src) pst1 OPTIONAL(i + m)
            PR.FDone 0 b -> return (Right b, StreamD.nil)
            PR.FDone m b -> do
                let n = (- m)
                assert (n <= length (getList buf)) (return ())
                let src0 = Prelude.take n (getList buf)
                    src  = Prelude.reverse src0
                return (Right b, fromList src)
            PR.FError err -> do
                let src  = Prelude.reverse $ getList buf
                return (Left (PARSE_ERROR(i) err), fromList src)

{-# INLINE_NORMAL PARSE_BREAK_STREAMK #-}
PARSE_BREAK_STREAMK
    :: forall m a b. Monad m
    => ParserK.ParserK a m b
    -> StreamK m a
    -> m (Either PARSE_ERROR_TYPE b, StreamK m a)
PARSE_BREAK_STREAMK parser input = do
    let parserk = ParserK.runParser parser ParserK.parserDone 0 0
     in go OPTIONAL(0) [] parserk input

    where

    {-# INLINE backtrck #-}
    -- backtrck :: Int -> [a] -> StreamK m a -> (StreamK m a, [a])
    backtrck n xs stream =
        let (pre, post) = Stream.splitAt "Data.StreamK.parseBreak" n xs
         in (StreamK.append (StreamK.fromList (Prelude.reverse pre)) stream, post)

    {-# INLINE goStop #-}
    {-
    goStop
        :: OPTIONAL(Int ->)
           [a]
        -> (ParserK.Input a -> m (ParserK.Step a m b))
        -> m (Either PARSE_ERROR_TYPE b, StreamK m a)
    -}
    goStop OPTIONAL(pos) backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go OPTIONAL(pos) [] cont1 StreamK.nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= length backBuf)
                let (s1, backBuf1) = backtrck n1 backBuf StreamK.nil
                 in go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go OPTIONAL(pos) backBuf cont1 StreamK.nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= length backBuf)
                let (s1, backBuf1) = backtrck n1 backBuf StreamK.nil
                 in go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, StreamK.nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= length backBuf)
                let (s1, _) = backtrck n1 backBuf StreamK.nil
                 in return (Right b, s1)
            ParserK.Error _n err ->
                let strm = StreamK.fromList (Prelude.reverse backBuf)
                -- XXX do we need to add n here?
                 in return (Left (PARSE_ERROR(pos + _n) err), strm)

    {-
    yieldk
        :: OPTIONAL(Int ->)
           [a]
        -> (ParserK.Input a -> m (ParserK.Step a m b))
        -> a
        -> StreamK m a
        -> m (Either PPARSE_ERROR_TYPE b, StreamK m a)
    -}
    yieldk OPTIONAL(pos) backBuf parserk element stream = do
        pRes <- parserk (ParserK.Chunk element)
        -- NOTE: factoring out "StreamK.cons element stream" in a let statement here
        -- cause big alloc regression.
        case pRes of
            ParserK.Partial 1 cont1 -> go OPTIONAL(pos + 1) [] cont1 stream
            ParserK.Partial 0 cont1 -> go OPTIONAL(pos) [] cont1 (StreamK.cons element stream)
            ParserK.Partial n cont1 -> do -- n < 0 case
                let n1 = negate n
                    bufLen = length backBuf
                    s = StreamK.cons element stream
                assertM(n1 >= 0 && n1 <= bufLen)
                let (s1, _) = backtrck n1 backBuf s
                go OPTIONAL(pos + n) [] cont1 s1
            ParserK.Continue 1 cont1 -> go OPTIONAL(pos + 1) (element:backBuf) cont1 stream
            ParserK.Continue 0 cont1 ->
                go OPTIONAL(pos) backBuf cont1 (StreamK.cons element stream)
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                    bufLen = length backBuf
                    s = StreamK.cons element stream
                assertM(n1 >= 0 && n1 <= bufLen)
                let (s1, backBuf1) = backtrck n1 backBuf s
                go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Done 1 b -> pure (Right b, stream)
            ParserK.Done 0 b -> pure (Right b, StreamK.cons element stream)
            ParserK.Done n b -> do
                let n1 = negate n
                    bufLen = length backBuf
                    s = StreamK.cons element stream
                assertM(n1 >= 0 && n1 <= bufLen)
                let (s1, _) = backtrck n1 backBuf s
                pure (Right b, s1)
            ParserK.Error _n err ->
                let strm =
                        StreamK.append
                            (StreamK.fromList (Prelude.reverse backBuf))
                            (StreamK.cons element stream)
                 in return (Left (PARSE_ERROR(pos + _n + 1) err), strm)

    {-
    go
        :: OPTIONAL(Int ->)
           [a]
        -> (ParserK.Input a -> m (ParserK.Step a m b))
        -> StreamK m a
        -> m (Either PARSE_ERROR_TYPE b, StreamK m a)
    -}
    go OPTIONAL(pos) backBuf parserk stream = do
        let stop = goStop OPTIONAL(pos) backBuf parserk
            single a = yieldk OPTIONAL(pos) backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk OPTIONAL(pos) backBuf parserk) single stop stream

{-# INLINE_NORMAL PARSE_BREAK_CHUNKS #-}
PARSE_BREAK_CHUNKS
    :: (Monad m, Unbox a)
    => ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either PARSE_ERROR_TYPE b, StreamK m (Array a))
PARSE_BREAK_CHUNKS parser input = do
    let parserk = ParserK.runParser parser ParserK.parserDone 0 0
     in go OPTIONAL(0) [] parserk input

    where

    {-# INLINE goStop #-}
    goStop OPTIONAL(pos) backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go OPTIONAL(pos) [] cont1 StreamK.nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map Array.length backBuf))
                let (s1, backBuf1) = backtrack n1 backBuf StreamK.nil
                 in go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go OPTIONAL(pos) backBuf cont1 StreamK.nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map Array.length backBuf))
                let (s1, backBuf1) = backtrack n1 backBuf StreamK.nil
                 in go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, StreamK.nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map Array.length backBuf))
                let (s1, _) = backtrack n1 backBuf StreamK.nil
                 in return (Right b, s1)
            ParserK.Error _n err -> do
                let s1 = Prelude.foldl (flip StreamK.cons) StreamK.nil backBuf
                return (Left (PARSE_ERROR(pos + _n) err), s1)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    yieldk OPTIONAL(pos) backBuf parserk arr stream = do
        pRes <- parserk (ParserK.Chunk arr)
        let len = Array.length arr
        case pRes of
            ParserK.Partial n cont1 ->
                case compare n len of
                    EQ -> go OPTIONAL(pos + n) [] cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk OPTIONAL(pos + n) [] cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map Array.length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, _) = backtrack n1 backBuf s
                            go OPTIONAL(pos + n) [] cont1 s1
                    GT -> seekErr n len
            ParserK.Continue n cont1 ->
                case compare n len of
                    EQ -> go OPTIONAL(pos + n) (arr:backBuf) cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk OPTIONAL(pos + n) backBuf cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map Array.length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, backBuf1) = backtrack n1 backBuf s
                            go OPTIONAL(pos + n) backBuf1 cont1 s1
                    GT -> seekErr n len
            ParserK.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (Prelude.map Array.length (arr:backBuf)))
                let (s1, _) = backtrack n1 (arr:backBuf) stream
                 in return (Right b, s1)
            ParserK.Error _n err -> do
                let s1 = Prelude.foldl (flip StreamK.cons) stream (arr:backBuf)
                return (Left (PARSE_ERROR(pos + _n + 1) err), s1)

    go OPTIONAL(pos) backBuf parserk stream = do
        let stop = goStop OPTIONAL(pos) backBuf parserk
            single a = yieldk OPTIONAL(pos) backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk OPTIONAL(pos) backBuf parserk) single stop stream

{-# INLINE_NORMAL PARSE_BREAK_CHUNKS_GENERIC #-}
PARSE_BREAK_CHUNKS_GENERIC
    :: forall m a b. Monad m
    => ParserK.ParserK (GArray.Array a) m b
    -> StreamK m (GArray.Array a)
    -> m (Either PARSE_ERROR_TYPE b, StreamK m (GArray.Array a))
PARSE_BREAK_CHUNKS_GENERIC parser input = do
    let parserk = ParserK.runParser parser ParserK.parserDone 0 0
     in go OPTIONAL(0) [] parserk input

    where

    {-# INLINE goStop #-}
    {-
    goStop
        :: OPTIONAL(Int ->)
           [GArray.Array a]
        -> (ParserK.Input (GArray.Array a)
                -> m (ParserK.Step (GArray.Array a) m b))
        -> m (Either PARSE_ERROR_TYPE b, StreamK m (GArray.Array a))
    -}
    goStop OPTIONAL(pos) backBuf parserk = do
        pRes <- parserk ParserK.None
        case pRes of
            -- If we stop in an alternative, it will try calling the next
            -- parser, the next parser may call initial returning Partial and
            -- then immediately we have to call extract on it.
            ParserK.Partial 0 cont1 ->
                 go OPTIONAL(pos) [] cont1 StreamK.nil
            ParserK.Partial n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map GArray.length backBuf))
                let (s1, backBuf1) = backtrackGeneric n1 backBuf StreamK.nil
                 in go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Continue 0 cont1 ->
                go OPTIONAL(pos) backBuf cont1 StreamK.nil
            ParserK.Continue n cont1 -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map GArray.length backBuf))
                let (s1, backBuf1) = backtrackGeneric n1 backBuf StreamK.nil
                 in go OPTIONAL(pos + n) backBuf1 cont1 s1
            ParserK.Done 0 b ->
                return (Right b, StreamK.nil)
            ParserK.Done n b -> do
                let n1 = negate n
                assertM(n1 >= 0 && n1 <= sum (Prelude.map GArray.length backBuf))
                let (s1, _) = backtrackGeneric n1 backBuf StreamK.nil
                 in return (Right b, s1)
            ParserK.Error _n err ->
                let strm = Prelude.foldl (flip StreamK.cons) StreamK.nil backBuf
                 in return (Left (PARSE_ERROR(pos + _n) err), strm)

    seekErr n len =
        error $ "parseBreak: Partial: forward seek not implemented n = "
            ++ show n ++ " len = " ++ show len

    {-
    yieldk
        :: OPTIONAL(Int ->)
           [GArray.Array a]
        -> (ParserK.Input (GArray.Array a)
                -> m (ParserK.Step (GArray.Array a) m b))
        -> Array a
        -> StreamK m (GArray.Array a)
        -> m (Either PARSE_ERROR_TYPE b, StreamK m (GArray.Array a))
    -}
    yieldk OPTIONAL(pos) backBuf parserk arr stream = do
        pRes <- parserk (ParserK.Chunk arr)
        let len = GArray.length arr
        case pRes of
            ParserK.Partial n cont1 ->
                case compare n len of
                    EQ -> go OPTIONAL(pos + n) [] cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk OPTIONAL(pos + n) [] cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map GArray.length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, _) = backtrackGeneric n1 backBuf s
                            go OPTIONAL(pos + n) [] cont1 s1
                    GT -> seekErr n len
            ParserK.Continue n cont1 ->
                case compare n len of
                    EQ -> go OPTIONAL(pos + n) (arr:backBuf) cont1 stream
                    LT -> do
                        if n >= 0
                        then yieldk OPTIONAL(pos + n) backBuf cont1 arr stream
                        else do
                            let n1 = negate n
                                bufLen = sum (Prelude.map GArray.length backBuf)
                                s = StreamK.cons arr stream
                            assertM(n1 >= 0 && n1 <= bufLen)
                            let (s1, backBuf1) = backtrackGeneric n1 backBuf s
                            go OPTIONAL(pos + n) backBuf1 cont1 s1
                    GT -> seekErr n len
            ParserK.Done n b -> do
                let n1 = len - n
                assertM(n1 <= sum (Prelude.map GArray.length (arr:backBuf)))
                let (s1, _) = backtrackGeneric n1 (arr:backBuf) stream
                 in return (Right b, s1)
            ParserK.Error _n err ->
                let strm = Prelude.foldl (flip StreamK.cons) stream (arr:backBuf)
                 in return (Left (PARSE_ERROR(pos + _n + 1) err), strm)

    {-
    go
        :: OPTIONAL(Int ->)
           [GArray.Array a]
        -> (ParserK.Input (GArray.Array a)
                -> m (ParserK.Step (GArray.Array a) m b))
        -> StreamK m (GArray.Array a)
        -> m (Either PARSE_ERROR_TYPE b, StreamK m (GArray.Array a))
    -}
    go OPTIONAL(pos) backBuf parserk stream = do
        let stop = goStop OPTIONAL(pos) backBuf parserk
            single a = yieldk OPTIONAL(pos) backBuf parserk a StreamK.nil
         in StreamK.foldStream
                defState (yieldk OPTIONAL(pos) backBuf parserk) single stop stream
