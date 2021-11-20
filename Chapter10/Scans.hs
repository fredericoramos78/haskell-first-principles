module Chapter10.Scans where 

fibs = 1 : scanl (+) 1 fibs 
fibsN x = fibs !! x

-- 1. Modify your fibs function to only return the first 20 Fibonacci numbers.
fibs20 = take 20 fibs 

-- 2. Modify fibs to return the Fibonacci numbers that are less than 100.
fibsLT100 = takeWhile (< 100) fibs

-- 3. Try to write the factorial function from Chapter 8 as a scan. You’ll want scanl again, and your start value will be 1.
--    Warning: this will also generate an infinite list, so you may want to pass it through a take function or similar.

factorial :: Int -> Integer
factorial n = last . take (n+1) $ scanl (*) 1 [1..]

factorial' :: Int -> Integer
factorial' n = scanl (*) 1 [1..] !! n