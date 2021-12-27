module Identity where

import Data.Traversable

import Test.QuickCheck 
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


newtype Identity a = Identity a deriving (Eq, Ord, Show) 

instance Functor Identity where 
    fmap f (Identity a) = Identity b 
                            where b = f a

instance Foldable Identity where 
    foldr f i (Identity a) = f a i 

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a 


-- for testing 
instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = Identity <$> arbitrary 

instance Eq a => EqProp (Identity a) where 
    (=-=) = eq
