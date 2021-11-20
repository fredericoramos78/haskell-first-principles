module Chapter5.TypeMatching where 

-- 
-- Functions:
-- a) not
-- b) length
-- c) concat 
-- d) head
-- e) (<)

--2. Type signatures:
-- a) _ :: [a] -> a
-- b) _ :: [[a]] -> [a] 
-- c) _ :: Bool -> Bool
-- d) _ :: [a] -> Int
-- e) _ :: Ord a => a -> a -> Bool


-- (a) && (c) not :: Bool -> Bool
-- (b) && (d) length :: [a] -> Int
-- (c) && (b) concat :: [[a]] -> [a]
-- (d) && (a) head :: [a] -> a
-- (e) && (e) (<) :: Ord a => a -> a -> Bool