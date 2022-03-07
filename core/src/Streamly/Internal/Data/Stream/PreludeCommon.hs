------------------------------------------------------------------------------
-- Conversion to and from direct style stream
------------------------------------------------------------------------------

#ifdef USE_IS_STREAM
#define IS_STREAM IsStream t,
#define FROM_STREAM fromStream
#define TO_STREAM toStream
#define STREAM t
#else
#define IS_STREAM
#define FROM_STREAM id
#define TO_STREAM id
#define STREAM K.Stream
#endif

-- These definitions are dependent on what is imported as S
{-# INLINE fromStreamS #-}
fromStreamS :: (IS_STREAM Monad m) => S.Stream m a -> STREAM m a
fromStreamS = FROM_STREAM . S.toStreamK

{-# INLINE toStreamS #-}
toStreamS :: (IS_STREAM Monad m) => STREAM m a -> S.Stream m a
toStreamS = S.fromStreamK . TO_STREAM

{-# INLINE toStreamD #-}
toStreamD :: (IS_STREAM Monad m) => STREAM m a -> D.Stream m a
toStreamD = D.fromStreamK . TO_STREAM

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE_EARLY drain #-}
drain :: (IS_STREAM Monad m) => STREAM m a -> m ()
drain m = D.drain $ D.fromStreamK (TO_STREAM m)
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromList = 'Prelude.foldr' 'K.cons' 'K.nil'
-- @
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'K.fromFoldable' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromList #-}
#ifdef USE_IS_STREAM
fromList :: (Monad m, IsStream t) => [a] -> STREAM m a
#else
fromList :: Monad m => [a] -> STREAM m a
#endif
fromList = fromStreamS . S.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: (IS_STREAM Monad m) => STREAM m a -> m [a]
toList m = S.toList $ toStreamS m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: (IS_STREAM Monad m) => (a -> m b -> m b) -> m b -> STREAM m a -> m b
foldrM step acc m = S.foldrM step acc $ toStreamS m

{-# INLINE foldrMx #-}
foldrMx :: (IS_STREAM Monad m)
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> STREAM m a -> m b
foldrMx step final project m = D.foldrMx step final project $ toStreamD m

{-# INLINE foldr #-}
foldr :: (IS_STREAM Monad m) => (a -> b -> b) -> b -> STREAM m a -> m b
foldr f z = foldrM (\a b -> f a <$> b) (return z)

-- | Like 'foldlx'', but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE foldlMx' #-}
foldlMx' ::
    (IS_STREAM Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> STREAM m a -> m b
foldlMx' step begin done m = S.foldlMx' step begin done $ toStreamS m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.7.0
{-# INLINE foldlx' #-}
foldlx' ::
    (IS_STREAM Monad m) => (x -> a -> x) -> x -> (x -> b) -> STREAM m a -> m b
foldlx' step begin done m = S.foldlx' step begin done $ toStreamS m

-- | Strict left associative fold.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' ::
    (IS_STREAM Monad m) => (b -> a -> b) -> b -> STREAM m a -> m b
foldl' step begin m = S.foldl' step begin $ toStreamS m


{-# INLINE fold #-}
fold :: (IS_STREAM Monad m) => Fold m a b -> STREAM m a -> m b
fold fld m = S.fold fld $ toStreamS m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
-- @since 0.5.3
{-# INLINE eqBy #-}
eqBy :: (IS_STREAM Monad m) =>
    (a -> b -> Bool) -> STREAM m a -> STREAM m b -> m Bool
eqBy f m1 m2 = D.eqBy f (toStreamD m1) (toStreamD m2)

-- | Compare two streams
--
-- @since 0.5.3
{-# INLINE cmpBy #-}
cmpBy
    :: (IS_STREAM Monad m)
    => (a -> b -> Ordering) -> STREAM m a -> STREAM m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (toStreamD m1) (toStreamD m2)
