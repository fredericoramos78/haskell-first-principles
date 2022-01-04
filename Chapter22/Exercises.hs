module Chapter22.Exercises where

import Control.Applicative
import Data.Maybe



x :: [Integer]
x = [1, 2, 3] 

y :: [Integer]
y = [4, 5, 6] 

z :: [Integer]
z = [7, 8, 9]

xy = zip x y 
yz = zip y z 
xz = zip x z

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer 
xs = lookup 3 xy   

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer 
ys = lookup 6 yz

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 xy

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer 
z' n = lookup n xz

x1 :: Maybe (Integer, Integer) 
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer) 
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'
-- also:
-- x3 n = let a = z' n
--        in (a, a)


-- uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c 
summed = uncurry (+)

bolt :: Integer -> Bool 
-- use &&, >3, <8
bolt = (&&) <$> f <*> g 
          where f = (>3)
                g = (<8)  

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs <*> ys) 

sequAFolded :: Integral a => a -> Bool 
sequAFolded = and . sequA

sequAToS' = sequA <$> s' 

boltToYS = bolt <$> ys 

main :: IO ()
main = do 
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y] 
    print $ sequenceA [xs, ys]
    print $ s'
    print $ fmap summed ((,) <$> xs <*> zs) 
    print $ bolt 7
    print $ fmap bolt z
    print $ sequA 7
    print "--------------"
    print $ sequAFolded 7 
    print sequAToS' 
    print boltToYS

