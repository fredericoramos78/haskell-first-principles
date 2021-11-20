module Chapter5.FunctionTypeInference where 

-- Exercises: Considering a type signature of general function, indicate how might that change 
-- when we apply it to a concrete value.

-- 1. (++) :: [a] -> [a] -> [a]
-- myConcat x = x ++ " yo"
-- myConcat x :: [Char] -> [Char]
myConcat x = x ++ " yo"

-- 2. (*) :: Num a => a -> a -> a 
-- myMult x = (x / 3) * 5
-- myMult :: Fractional a => a -> a
--   (/) only exists for Fractional 
myMult x = (x / 3) * 5


-- 3. take :: Int -> [a] -> [a] 
-- myTake x = take x "hey you"
-- myTake :: Int => [Char] -> [Char]
myTake x = take x "hey you"

-- 4. (>) :: Ord a => a -> a -> Bool 
-- myCom x = x > (length [1..10])
-- myCom Int a => a -> Bool
--    No need for Ord anymore since Int is also an Ord
myCom x = x > (length [1..10])

-- 5. (<) :: Ord a => a -> a -> Bool 
-- myAlph x = x < 'z'
-- myAlph :: Char -> Bool
myAlph x = x < 'z'