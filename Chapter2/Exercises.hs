-- Chapter 2: Basic expressions and functions
--
-- End of chapter exercises
-- 
module Exercises where 


-- Exercise 1: play around with parenthesis
-- Solution: `noParenthesis` is the original expression; `withParenthesis` is my understanding of operators precedence

-- 1. 2 + 2 * 3 - 1
exercise1_1_noParenthesis = 2 + 2 * 3 - 1
exercise1_1_withParenthesis = 2 + (2 * 3) - 1

-- 2. (^) 10 $ 1 + 1
exercise1_2_noParenthesis = (^) 10 $ 1 + 1
exercise1_2_withParenthesis = (^) 10 (1 + 1)

-- 3. 2 ^ 2 * 4 ^ 5 + 1
exercise1_3_noParenthesis = 2 ^ 2 * 4 ^ 5 + 1
exercise1_3_withParenthesis = ((2 ^ 2) * (4 ^ 5)) + 1


-- Exercise 2: equivalent expressions
-- Solution: Aside from 4' they should all result in True

-- 1.  1 + 1
--     2
exercise2_1 = 1 + 1 == 2
exercise2_1' = 2 == 2

-- 2. 10 ^ 2
--    10 + 9 * 10
exercise2_2 = 10 ^ 2 == 100
exercise2_2' = 10 + 9 * 10 == 100

-- 3. 400 - 37 (-) 37 
--    400
exercise2_3 = 400 - 37 == 363 
exercise2_3' = (-) 37 400 == (-363)

-- 4. 100 `div` 3 
--    100 / 3
exercise2_4 = (100 `div` 3) == 33
exercise2_4' = (100 / 3)    -- == 33.3333333

-- 5. 2 * 5 + 18
--    2 * (5 + 18)
exercise2_5 = 2 * 5 + 18 == 28
exercise2_5' = 2 * (5 + 18) == 46


-- Exercise 3: More fun with functions (REPL vs file)
waxOn = x * 5
    where z = 7
          y = z+8
          x = y^2 

triple x = x * 3

waxOff x = triple x