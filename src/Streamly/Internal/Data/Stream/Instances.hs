------------------------------------------------------------------------------
-- CPP macros for common instances
------------------------------------------------------------------------------

-- XXX use template haskell instead and include Monoid and IsStream instances
-- as well.

#define MONADPARALLEL , MonadAsync m

#define MONAD_COMMON_INSTANCES(STREAM,CONSTRAINT)                             \
instance Monad m => Functor (STREAM m) where {                                \
    {-# INLINE fmap #-};                                                      \
    fmap f (STREAM m) =                                                       \
        STREAM $ D.toStreamK $ D.mapM (return . f) $ D.fromStreamK m;         \
    {-# INLINE (<$) #-};                                                      \
    (<$) =  fmap . const };                                                   \
                                                                              \
instance (MonadIO m CONSTRAINT) => MonadIO (STREAM m) where {                 \
    liftIO x = STREAM $ K.fromEffect $ liftIO x };                            \
                                                                              \
instance (MonadThrow m CONSTRAINT) => MonadThrow (STREAM m) where {           \
    throwM x = STREAM $ K.fromEffect $ throwM x };                            \
                                                                              \
{- \
instance (MonadError e m CONSTRAINT) => MonadError e (STREAM m) where {       \
    throwError = lift . throwError;                                           \
    catchError m h =                                                          \
        fromStream $ withCatchError (toStream m) (\e -> toStream $ h e) };  \
-} \
                                                                              \
instance (MonadReader r m CONSTRAINT) => MonadReader r (STREAM m) where {     \
    ask = STREAM $ K.fromEffect ask;                                          \
    local f (STREAM m) = STREAM $ withLocal f m };                          \
                                                                              \
instance (MonadState s m CONSTRAINT) => MonadState s (STREAM m) where {       \
    {-# INLINE get #-}; \
    get = STREAM $ K.fromEffect get;                                          \
    {-# INLINE put #-}; \
    put x = STREAM $ K.fromEffect $ put x;                                    \
    {-# INLINE state #-}; \
    state k = STREAM $ K.fromEffect $ state k }

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- Serial streams can act like regular lists using the Identity monad

-- XXX Show instance is 10x slower compared to read, we can do much better.
-- The list show instance itself is really slow.

-- XXX The default definitions of "<" in the Ord instance etc. do not perform
-- well, because they do not get inlined. Need to add INLINE in Ord class in
-- base?

#define NFDATA1_INSTANCE(STREAM)                                              \
instance NFData1 (STREAM Identity) where {                                    \
    {-# INLINE liftRnf #-};                                                   \
    liftRnf f (STREAM xs) = runIdentity $ P.foldl' (\_ x -> f x) () xs}

#define LIST_INSTANCES(STREAM)                                                \
instance IsList (STREAM Identity a) where {                                   \
    type (Item (STREAM Identity a)) = a;                                      \
    {-# INLINE fromList #-};                                                  \
    fromList xs = STREAM $ P.fromList xs;                                     \
    {-# INLINE toList #-};                                                    \
    toList (STREAM xs) = runIdentity $ P.toList xs };                         \
                                                                              \
instance Eq a => Eq (STREAM Identity a) where {                               \
    {-# INLINE (==) #-};                                                      \
    (==) (STREAM xs) (STREAM ys) = runIdentity $ P.eqBy (==) xs ys };         \
                                                                              \
instance Ord a => Ord (STREAM Identity a) where {                             \
    {-# INLINE compare #-};                                                   \
    compare (STREAM xs) (STREAM ys) = runIdentity $ P.cmpBy compare xs ys;    \
    {-# INLINE (<) #-};                                                       \
    x <  y = case compare x y of { LT -> True;  _ -> False };                 \
    {-# INLINE (<=) #-};                                                      \
    x <= y = case compare x y of { GT -> False; _ -> True };                  \
    {-# INLINE (>) #-};                                                       \
    x >  y = case compare x y of { GT -> True;  _ -> False };                 \
    {-# INLINE (>=) #-};                                                      \
    x >= y = case compare x y of { LT -> False; _ -> True };                  \
    {-# INLINE max #-};                                                       \
    max x y = if x <= y then y else x;                                        \
    {-# INLINE min #-};                                                       \
    min x y = if x <= y then x else y; };                                     \
                                                                              \
instance Show a => Show (STREAM Identity a) where {                           \
    showsPrec p dl = showParen (p > 10) $                                     \
        showString "fromList " . shows (toList dl) };                         \
                                                                              \
instance Read a => Read (STREAM Identity a) where {                           \
    readPrec = parens $ prec 10 $ do {                                        \
        Ident "fromList" <- lexP;                                             \
        fromList <$> readPrec };                                              \
    readListPrec = readListPrecDefault };                                     \
                                                                              \
instance (a ~ Char) => IsString (STREAM Identity a) where {                   \
    {-# INLINE fromString #-};                                                \
    fromString xs = STREAM $ P.fromList xs };                                                \
                                                                              \
instance NFData a => NFData (STREAM Identity a) where {                       \
    {-# INLINE rnf #-};                                                       \
    rnf (STREAM xs) = runIdentity $ P.foldl' (\_ x -> rnf x) () xs};                        \

-------------------------------------------------------------------------------
-- Foldable
-------------------------------------------------------------------------------

-- The default Foldable instance has several issues:
-- 1) several definitions do not have INLINE on them, so we provide
--    re-implementations with INLINE pragmas.
-- 2) the definitions of sum/product/maximum/minimum are inefficient as they
--    use right folds, they cannot run in constant memory. We provide
--    implementations using strict left folds here.

#define FOLDABLE_INSTANCE(STREAM)                                             \
instance (Foldable m, Monad m) => Foldable (STREAM m) where {                 \
                                                                              \
    {-# INLINE foldMap #-};                                                   \
    foldMap f (STREAM xs) = fold $ P.foldr (mappend . f) mempty xs;           \
                                                                              \
    {-# INLINE foldr #-};                                                     \
    foldr f z t = appEndo (foldMap (Endo #. f) t) z;                          \
                                                                              \
    {-# INLINE foldl' #-};                                                    \
    foldl' f z0 xs = foldr f' id xs z0                                        \
        where {f' x k = oneShot $ \z -> k $! f z x};                          \
                                                                              \
    {-# INLINE length #-};                                                    \
    length = foldl' (\n _ -> n + 1) 0;                                        \
                                                                              \
    {-# INLINE elem #-};                                                      \
    elem = any . (==);                                                        \
                                                                              \
    {-# INLINE maximum #-};                                                   \
    maximum =                                                                 \
          fromMaybe (errorWithoutStackTrace "maximum: empty stream")        \
        . toMaybe                                                             \
        . foldl' getMax Nothing' where {                                      \
            getMax Nothing' x = Just' x;                                      \
            getMax (Just' mx) x = Just' $! max mx x };                        \
                                                                              \
    {-# INLINE minimum #-};                                                   \
    minimum =                                                                 \
          fromMaybe (errorWithoutStackTrace "minimum: empty stream")        \
        . toMaybe                                                             \
        . foldl' getMin Nothing' where {                                      \
            getMin Nothing' x = Just' x;                                      \
            getMin (Just' mn) x = Just' $! min mn x };                        \
                                                                              \
    {-# INLINE sum #-};                                                       \
    sum = foldl' (+) 0;                                                       \
                                                                              \
    {-# INLINE product #-};                                                   \
    product = foldl' (*) 1 }

-------------------------------------------------------------------------------
-- Traversable
-------------------------------------------------------------------------------

#define TRAVERSABLE_INSTANCE(STREAM)                                          \
instance Traversable (STREAM Identity) where {                                \
    {-# INLINE traverse #-};                                                  \
    traverse f (STREAM xs) =                                                  \
        fmap STREAM $ runIdentity $ P.foldr consA (pure mempty) xs            \
        where { consA x ys = liftA2 K.cons (f x) ys }}
