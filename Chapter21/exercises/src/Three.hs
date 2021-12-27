module Three where

import Data.Traversable

import Test.QuickCheck
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers 

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b $ f c 

instance Foldable (Three a b) where 
    foldr f i (Three a b c) = f c i 

instance Traversable (Three a b) where 
    traverse f (Three x y z) = p <$> f z 
                                where p = Three x y 

-- for testing 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        Three a b <$> arbitrary 

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where 
    (=-=) = eq 