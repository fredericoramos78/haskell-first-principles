{-# LANGUAGE NoMonomorphismRestriction #-}

module Chapter5.Exercises where 

-- Exercise 1:
-- 1. (c) a list of elements that are all of some type a
-- 2. (b) take a list of strings as an argument
-- 3. (b) returns one element of type a from a list
-- 4. (c) takes a tuple argument and returns the first value


-- Exercise 2:
-- f :: Num a
f = (* 9) 6
-- g :: (Num, [Char])
g = head [(0,"doge"),(1,"kitteh")]
-- h :: (Integer, [Char])
h = head [(0 :: Integer ,"doge"),(1,"kitteh")] 
-- i :: Bool
i = if False then True else False
-- j :: Int
j = length [1, 2, 3, 4, 5]
-- k :: Bool
k = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- w :: Num
x = 5 
y = x + 5 
w = y * 10
-- z :: Num a => a -> a
z y = y * 10
-- f' :: Fractional a -> a
f' = 4 / y

x' = "Julie"
y' = " <3 "
z' = "Haskell"
-- ff :: [Char]
ff = x' ++ y' ++ z'


-- Exercise 3:
bigNum = (^) 5 $ 10 
wahoo = bigNum -- no need for $ 10 here

x1 = print
y1 = print "woohoo!" 
z1 = x1 "hello world"

a = (+) 
b = 5
c = a 10  -- changed (b) into (a)
d = c 200 -- fixes this too


c1 = 1 -- missing c1?
b1 = 10000 * c1 -- moved up!
a1 = 12 + b1


-- Exercise 4: Type variable or specific type constructor?

-- f :: zed -> Zed -> Blah
--      [1]                 fully polymorphic
--             [2]          Concrete
--                     [3]  Concrete

-- f :: Enum b => a -> b -> C
--               [1]           Fully polymorphic
--                    [2]      Constrained on Enum
--                         [3] Concrete

-- f :: f -> g -> C
--     [1]           Fully polymorphic
--          [2]      Fully polymorphic
--               [3] Concrete


-- Exercise 5: 


functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool 
functionC x y = if (x > y) then True else False 

functionS :: (a, b) -> b
functionS (x, y) = y



-- Exercise 6:

iff :: a -> a
iff a = a 

cf :: a -> b -> a 
cf a _ = a

-- alpha equivalent to function cf above
cf'' :: b -> a -> b 
cf'' b _ = b  

cf' :: a -> b -> b 
cf' _ b = b

r :: [a] -> [a] 
r [] = []
r (x:xs) = [x] ++ (r xs)

co :: (b -> c) -> (a -> b) -> a -> c 
co f1 f2 a = f1 $ f2 a

af :: (a -> c) -> a -> a 
af _ a = a

a' :: (a -> b) -> a -> b 
a' f1 a = f1 a 