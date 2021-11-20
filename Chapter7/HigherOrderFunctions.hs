module Chapter7.HigherOrderFunctions where 

dodgy :: Num a => a -> a -> a 
dodgy x y = x + y * 10 

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1 

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2


-- 2. 11
-- dodgy 1 1 
-- 3. 22
-- dodgy 2 2 
-- 4. 21
-- dodgy 1 2 
-- 5. 12
-- dodgy 2 1 
-- 6. 11
-- oneIsOne 1
-- 7. 21
-- oneIsOne 2 
-- 8. 21
-- oneIsTwo 1 
-- 9. 22
-- oneIsTwo 2
-- 10. 31
-- oneIsOne 3 
-- 11. 23
-- oneIsTwo 3

