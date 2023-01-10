-------------------------------------------------------------------------------
-- Macros to access Storable pointers
-------------------------------------------------------------------------------

-- The Storable instance of () has size 0. We ensure that the size is non-zero
-- to avoid a zero sized element and issues due to that.
-- See https://mail.haskell.org/pipermail/libraries/2022-January/thread.html
--
-- XXX Check the core to see if max can be statically eliminated. llvm can
-- eliminate the comparison, but not sure if GHC NCG can.
#define STORABLE_SIZE_OF(a) max 1 (sizeOf (undefined :: a))

-- Move the pointer to ith element of specified type. Type is specified as the
-- type variable in the signature of the function where this macro is used.
#define PTR_NEXT(ptr,a) ptr `plusPtr` STORABLE_SIZE_OF(a)
#define PTR_PREV(ptr,a) ptr `plusPtr` negate (STORABLE_SIZE_OF(a))

#define PTR_INDEX(ptr,i,a) ptr `plusPtr` (STORABLE_SIZE_OF(a) * i)
#define PTR_RINDEX(ptr,i,a) ptr `plusPtr` negate (STORABLE_SIZE_OF(a) * (i + 1))

-- XXX If we know that the array is guaranteed to have size multiples of the
-- element size then we can use a simpler check saying "ptr < end". Since we
-- always allocate in multiples of elem we can use the simpler check and assert
-- the rigorous check.
#define PTR_VALID(ptr,end,a) ptr `plusPtr` STORABLE_SIZE_OF(a) <= end
#define PTR_INVALID(ptr,end,a) ptr `plusPtr` STORABLE_SIZE_OF(a) > end

-------------------------------------------------------------------------------
-- Macros to access array indices (using Unbox type class)
-------------------------------------------------------------------------------

-- This macro was originally defined as a wrapper to sizeOf so that we can
-- avoid a sizeOf value of 0 and make it 1.
#define SIZE_OF(a) sizeOf (Proxy :: Proxy a)

#define INDEX_NEXT(i,a) i + SIZE_OF(a)
#define INDEX_PREV(i,a) i - SIZE_OF(a)

#define INDEX_OF(base,i,a) base + (SIZE_OF(a) * i)
#define RINDEX_OF(base,i,a) base - (SIZE_OF(a) * (i + 1))

#define INDEX_VALID(i,end,a) i + SIZE_OF(a) <= end
#define INDEX_INVALID(i,end,a) i + SIZE_OF(a) > end
