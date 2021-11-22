module Arbitraries where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)


-- nullary type
data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial 
trivialGen = return Trivial 

instance Arbitrary Trivial where 
    arbitrary = trivialGen


-- unary type
data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary 
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen 

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- product type
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary 
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where 
    arbitrary = pairGen 

pairGenIntString :: Gen (Pair Int String) 
pairGenIntString = pairGen


-- sum type
data Sum a b = First a | Second b deriving (Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
    a <- arbitrary 
    b <- arbitrary 
    oneof [ return $ First a, 
            return $ Second b ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where 
    arbitrary = sumGen 