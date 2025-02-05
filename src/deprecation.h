#define RENAME(_old, _new)                          \
{-# DEPRECATED _old "Please use _new instead." #-}; \
{-# INLINE _old #-}; \
_old = _new

#define RENAME_PRIME(_old, _new)                       \
{-# DEPRECATED _old "Please use _new' instead." #-}; \
{-# INLINE _old #-}; \
_old = _new'
