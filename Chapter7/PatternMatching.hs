module Chapter7.PatternMatching where 

-- 1. Given the following declarations:
k :: (a, b) -> a
k (x, y) = x
k1 :: Num a => a
k1 = k ((4-1), 10)
k2 :: [Char]
k2 = k ("three", (1 + 2)) 
k3 :: Num a => a
k3 = k (3, True)

-- a) What is the type of k?
-- b) What is the type of k2? Is it the same type as k1 or k3?
-- c) Of k1, k2, k3, which will return the number 3 as the result? 
-- solution: k3


-- 2. Fill in the definition of the following function:
f :: (a, b, c)
    -> (d, e, f)
    -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
