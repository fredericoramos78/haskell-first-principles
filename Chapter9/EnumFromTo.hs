module Chapter9.EnumFromTo where 

eftBool :: Bool -> Bool -> [Bool] 
eftBool False True = [False, True]
eftBool True False = []
eftBool x _ = [x]

eftOrd :: Ordering
       -> Ordering
       -> [Ordering] 
eftOrd s e 
    | s > e = []
    | s == GT = [s]
    | otherwise = s : eftOrd (succ s) e

eftInt :: Int -> Int -> [Int] 
eftInt s e 
    | s > e = []
    | otherwise = s : eftInt (succ s) e

eftChar :: Char -> Char -> [Char] 
eftChar s e 
    | s > e = []
    | otherwise = s : eftChar (succ s) e