-------------------------------------------------------------------------------
-- Macros to access array pointers
-------------------------------------------------------------------------------

-- The Storable instance of () has size 0. We ensure that the size is non-zero
-- to avoid a zero sized element and issues due to that.
-- See https://mail.haskell.org/pipermail/libraries/2022-January/thread.html
--
-- XXX Check the core to see if max can be statically eliminated. llvm can
-- eliminate the comparison, but not sure if GHC NCG can.
#define SIZE_OF(a) max 1 (sizeOf (undefined :: a))

-- Move the pointer to ith element of specified type. Type is specified as the
-- type variable in the signature of the function where this macro is used.
#define PTR_NEXT(ptr,a) ptr `plusPtr` SIZE_OF(a)
#define PTR_PREV(ptr,a) ptr `plusPtr` negate (SIZE_OF(a))

#define PTR_INDEX(ptr,i,a) ptr `plusPtr` (SIZE_OF(a) * i)
#define PTR_RINDEX(ptr,i,a) ptr `plusPtr` negate (SIZE_OF(a) * (i + 1))

-- XXX If we know that the array is guaranteed to have size multiples of the
-- element size then we can use a simpler check saying "ptr < end". Since we
-- always allocate in multiples of elem we can use the simpler check and assert
-- the rigorous check.
#define PTR_VALID(ptr,end,a) ptr `plusPtr` SIZE_OF(a) <= end
#define PTR_INVALID(ptr,end,a) ptr `plusPtr` SIZE_OF(a) > end

-------------------------------------------------------------------------------
-- Macros to access array indices
-------------------------------------------------------------------------------

#define INDEX_NEXT(i) i + 1
#define INDEX_PREV(i) i - 1

#define INDEX_OF(base,i) base + i
#define RINDEX_OF(base,i) base - (i + 1)

#define INDEX_VALID(i,end) i < end
#define INDEX_INVALID(i,end) i >= end
