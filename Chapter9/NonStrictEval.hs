module Chapter9.NonStrictEval where 

-- Exercise 1:
-- Will the following expressions return a value or be "bottom"?

-- commented ones will blow
-- 1. BLOW!
f1 = [x^y | x <- [1..5], y <- [2, undefined]]
-- 2.
f2 = take 1 $ 
    [x^y | x <- [1..5], y <- [2, undefined]]
-- 3. BLOW!
f3 = sum [1, undefined, 3]
-- 4. 
f4 = length [1, 2, undefined]
-- 5. BLOW!
f5 = length $ [1, 2, 3] ++ undefined
-- 6. 
f6 = take 1 $ filter even [1, 2, 3, undefined]
-- 7. BLOW!
f7 = take 1 $ filter even [1, 3, undefined]
-- 8. 
f8 = take 1 $ filter odd [1, 3, undefined]
-- 9. 
f9 = take 2 $ filter odd [1, 3, undefined]
-- 10. BLOW! 
f10 = take 3 $ filter odd [1, 3, undefined]


-- Exercise 2:
-- 1. [1, 2, 3, 4, 5]
--  NF
-- 2. 1 : 2 : 3 : 4 : _
-- WHNF
-- 3. enumFromTo 1 10
-- NONE
-- 4. length [1, 2, 3, 4, 5]
-- NONE
-- 5. sum (enumFromTo 1 10)
-- NONE
-- 6. ['a'..'m'] ++ ['n'..'z']
-- NONE
-- 7. (_, 'b')
-- WHNF
