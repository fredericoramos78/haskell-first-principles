module Applics.Two where

import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where 
    pure x = Two mempty x
    (<*>) (Two a1 f) (Two a2 x) = Two (a1 <> a2) (f x)

-- for testing 

instance (Eq a, Eq b) => EqProp (Two a b) where 
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do 
        randomA <- arbitrary 
        Two randomA <$> arbitrary 