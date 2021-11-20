module Chapter11.PityTheBool where

import Data.Int


-- 1. Given a datatype:
data BigSmall =
      Big Bool
    | Small Bool 
    deriving (Eq, Show)

-- What is its cardinality? Hint: We already know Bool’s cardinality. Show your work, as demonstrated earlier.
-- Big Bool = 2 
-- Small Bool = 2
-- BigSmall = 2 + 2 = 4

-- 2. Given a datatype:
-- bring Int8 in scope
data NumberOrBool =
    Numba Int8
    | BoolyBool Bool deriving (Eq, Show)

myNumba = Numba (-128)

-- What is the cardinality of NumberOrBool? What happens if you try to create a Numba with a numeric literal larger 
-- than 127? And with a numeric literal smaller than -128?

-- Numba Int8 = 256
-- BoolyBool Bool = 2
-- NumberOrBool = 256 + 2 = 258
-- Fail since Int8 won't support such values

-- If you choose (-128) for a value precisely, you’ll notice you get a spurious warning:
-- Prelude> n = Numba (-129)
-- Literal -129 is out of the
--   Int8 range -128..127

-- Now, since -128 is a perfectly valid Int8 value, you could choose to ignore this. What happens is that (-128) 
-- desugars into (negate 128). The compiler sees that you expect the type Int8, but Int8’s max boundary is 127. 
-- So even though you’re negating 128, it hasn’t done that step yet and immediately whines about 128 being larger 
-- than 127. One way to avoid that warning is the following:

-- Prelude> x = Numba n
-- Or you can use the NegativeLiterals extension as it recommends:
-- Prelude> :set -XNegativeLiterals
-- Prelude> n = Numba (-128)

-- Note that the negative literals extension doesn’t prevent the warning if you use negate directly.