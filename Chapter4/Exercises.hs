module Chapter4.Exercises where 

-- let awesome = ["Papuchon", "curry", ":)"]
-- let also = ["Quake", "The Simons"]
-- let allAwesome = [awesome, also]

-- length :: [a] -> Integer
-- length [1, 2, 3, 4, 5] == 5
-- length [(1, 2), (2, 3), (3, 4)] == 3
-- length allAwesome == 2
-- length (concat allAwesome) == 5
-- length (concat allAwesome)


-- 6/3 ==> OK 
-- and
-- 6 / length [1, 2, 3] ==> Falty (6 `div` length [1, 2, 3])


-- length allAwesome == 2  ==> OK (True) 
-- length [1, 'a', 3, 'b'] ==> Falty (diff datatypes)
-- length allAwesome + length awesome ==> OK (5)
-- (8 == 8) && ('b' < 'a')  ==> OK (False)
-- (8 == 8) && 9 ==> Falty (&& with Bool and Int)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x


myAbs :: Integer -> Integer 
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f first second = ((snd first, snd second), (fst first, fst second))

-- Correcting syntax
x = (+)
lenPlus1 xs = x w 1
  where w = length xs

idFunction = (\x -> x)




