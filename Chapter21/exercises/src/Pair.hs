module Pair where

import Data.Traversable

import Test.QuickCheck
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers 

data Pair a b = Pair a b deriving (Show, Eq)

instance Functor (Pair a) where 
    fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where 
    foldr f i (Pair _ b) = f b i 

instance Traversable (Pair a) where 
    traverse f (Pair a b) = p <$> f b 
                            where p = Pair a 

-- for testing 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where 
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where 
    (=-=) = eq