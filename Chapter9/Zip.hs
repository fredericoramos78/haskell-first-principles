module Chapter9.Zip where 

-- 1. Write your own version of zip, and ensure it behaves the same as the original:
zip' :: [a] -> [b] -> [(a, b)] 
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

-- 2. Do what you did for zip but now for zipWith:
zipWith' :: (a -> b -> c)
            -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

-- 3. Rewrite your zip in terms of the zipWith you wrote.
zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zipWith' (,)