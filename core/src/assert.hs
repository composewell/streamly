-- A convenient macro to assert in a do block. We cannot define this as a
-- Haskell function because then the compiler reports the assert location
-- inside the wrapper function rather than the original location.

import Control.Exception (assert)
#define assertM(p) assert (p) (return ())
