module Chapter6.NeedsClarificationExercises where 

import sort from Data.List

-- Exercise 4:
-- 1. 
i :: Num a => a
i = 1

i' :: a
i' = undefined
-- i' = 1 fails, asking for a Num a => a! 
--    This happens b/c we provided a single implementation of i', one which only works for a Num (=1)
--    but the function was defined to work for any type. It's not straight forward to get this coming 
--    from a OO oriented background.
--    The best way I can describe is: in OO, the definition takes precedence. So as long as the implementation
--    can fit the definition you're fine. In FP, the implementation and definition of a function are more coupled
--    to each other, so if the definition says we can return any type then we should be able to find implementations
--    that support that.

-- 2. 
f :: Float
f = 1.0

f' :: Num a => a
f' = undefined
-- f' = 1.0 -- Same issue as above. f' definition says it should work for any Num but we only have implementations 
--    covering the Float/Double space. So the function definition should point that out by defiting it as Factional a => a.

-- 3. should work!
g :: Float 
g = 1.0

g' :: Fractional a => a
g' = 1.0

-- 4. 
h :: Float 
h = 1.0

h' :: RealFrac a => a
h' = 1.0

-- 5.
freud:: a -> a
freud x = x

freud' :: Ord a => a -> a
freud' x = x

-- 6.
freud2 :: a -> a 
freud2 x = x

freud2' :: Int -> Int 
freud2' x = x

-- 7.
myX = 1 :: Int

sigmund :: Int -> Int 
sigmund x = myX

sigmund' :: a -> a 
sigmund' x = myX

-- 8.
sigmund2 :: Int -> Int 
sigmund2 x = myX

sigmund2' :: Num a => a -> a
sigmund2' x = myX

-- 9.
jung :: Ord a => [a] -> a
jung xs = head (sort xs) 

jung' :: [Int] -> Int
jung' xs = head (sort xs) 

-- 10.
young :: [Char] -> Char 
young xs = head (sort xs)

young' :: Ord a => [a] -> a
young' xs = head (sort xs)

-- 11. 
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs) 

signifier' :: Ord a => [a] -> a
signifier' xs = head (mySort xs) 
