module Chapter9.Map where 

import Data.Bool

-- 1. Will the following expression return a value or be ⊥? 
-- Solution: bottom
f1 = take 1 $ map (+1) [undefined, 2, 3]

-- 2. Will the following expression return a value?
-- Solution [2]
f2 = take 1 $ map (+1) [1, undefined, 3]

-- 3. Will the following expression return a value?
-- Solution: bottom
f3 = take 2 $ map (+1) [1, undefined, 3]

-- 4. What does the following mystery function do? What is its type? Describe it (to yourself or a loved one) in standard English and 
--    then test it out in the REPL to make sure you are correct:
--    Solution: itIsMistery :: [Char] -> [Bool]
--              indicates if each Char in the input is a vowel (lowercase)
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- 5. What will be the result of the following functions:
--  Solution: [1,4,8,16,32,64,128,256,512,1024]
f5a = map (^2) [1..10]

-- Solution [1, 10, 20]
f5b = map minimum [[1..10], [10..20], [20..30]] 
      -- n.b. minimum is not the same function as the min function that we used before

-- Solution: [15, 15, 15]
f5c = map sum [[1..5], [1..5], [1..5]]

--6. Back in Chapter 7, you wrote a function called foldBool. That function exists in a module known as Data.Bool and
--   is called bool. Write a function that does the same (or similar, if you wish) as the map if-then-else function you 
--   saw above but uses bool instead of the if-then-else syntax. Your first step should be bringing the bool function 
--   into scope by typing import Data.Bool at your REPL prompt.
foldBool' :: a -> a -> [Bool] -> [a]
foldBool' thenR elseR = map (bool elseR thenR)