module Applics.Three where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where 
    fmap f (Three a b x) = Three a b (f x)

instance (Monoid a, Monoid b) => Applicative (Three a b) where 
    pure = Three mempty mempty
    (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)

-- for testing 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do 
        randomA <- arbitrary 
        randomB <- arbitrary 
        Three randomA randomB <$> arbitrary 

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where 
    (=-=) = eq 


