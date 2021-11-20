-- Chapter 2: Basic expressions and functions
--
-- Exercise to translate let expressions into functions using where declarations
-- 
module LetAndWhere where

-- 1. let x = 3; y = 1000 in x * 3 + y
mult1 = x * 3 + y
    where x = 3
          y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
mult2 = x * 5
    where x = 10 * 5 + y
          y = 10

-- let x = 7
--     y = negate x
--     z = y * 10 in z / x + y
mult3 z = z / x + y
    where x = 7
          y = negate x
