module Applics.ThreeL where

import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where 
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (Monoid a) => Applicative (Three' a) where 
    pure b = Three' mempty b b 
    (<*>) (Three' a bf bf') (Three' a' bx bx') = Three' (a <> a') (bf bx) (bf' bx')

-- for testing 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where 
    arbitrary = do 
        randomA <- arbitrary 
        randomB <- arbitrary 
        Three' randomA randomB <$> arbitrary 

instance (Eq a, Eq b) => EqProp (Three' a b) where 
    (=-=) = eq 


