module ThreeL where 

import Test.QuickCheck


data Three' a b = Three' a b b

instance Functor (Three' a) where 
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Show a, Show b) => Show (Three' a b) where 
    show (Three' a b b') = unwords ["Three'", show a, show b, show b']

instance (Eq a, Eq b) => Eq (Three' a b) where 
    (==) (Three' a1 b1 b'1) (Three' a2 b2 b'2) = a1 == a2 && b1 == b2 && b'1 == b'2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where 
    arbitrary = do 
        randomA <- arbitrary 
        randomB1 <- arbitrary 
        randomB2 <- arbitrary 
        return $ Three' randomA randomB1 randomB2