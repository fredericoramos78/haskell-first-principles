module Chapter6.FromRedditThread where 

class Node n where  
-- address :: (Foldable f, Num a, Ord a) => n -> f a --> fails!
 address :: (Num a, Ord a) => n -> [a]
 root :: n  
 depth :: n -> Int  

test :: Node n => n -> Int  
test x = length (address x)

