module Chapter7.GrabBag where 

-- 1. Which (two or more) of the following are equivalent?
-- solution: all of them!
mTh0 x y z = x * y * z
mTh1 x y = \z -> x * y * z
mTh2 x = \y -> \z -> x * y * z
mTh3 = \x -> \y -> \z -> x * y * z 


-- 2. The type of mTh (above) is Num a => a -> a -> a -> a. Which is the type of mTh 3?
-- a) Integer -> Integer -> Integer 
-- b) Num a => a -> a -> a -> a
-- c) Num a => a -> a
-- d) Num a => a -> a -> a  <---- solution

-- Next, we’ll practice writing anonymous lambda syntax. For example, one could rewrite:
-- addOne x = x + 1
-- Into:
-- addOne = \x -> x + 1

-- Try to make it so it can still be loaded as a top-level definition by GHCi. This will 
--    make it easier to validate your answers.
-- a) Rewrite the f function in the where clause:

addOneIfOdd n = case odd n of 
    True -> f n
    False -> n
    where f n = n + 1

addOneIfOdd' n = case odd n of 
    True -> f n
    False -> n
    where f = \n -> n + 1

-- b) Rewrite the following to use anonymous lambda syntax:
addFive x y = (if x > y then y else x) + 5
addFive' = \x -> \y -> (if x > y then y else x) + 5

-- c) Rewrite the following so that it doesn’t use anonymous lambda syntax:
mflip f = \x -> \y -> f y x
mflip' f x y = f y x