module SkiFree where

import Data.Traversable
import Control.Applicative 

import Test.QuickCheck
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers 



data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where 
    fmap f (S na a) = S nb b 
                        where nb = f <$> na 
                              b = f a

instance Foldable n => Foldable (S n) where 
    foldr f i (S n a) = f a i 
                         where i' = foldr f i n 

instance (Functor n, Traversable n) => Traversable (S n) where 
    traverse f (S n a) =  S <$> nInB <*> bInA 
                         -- bInA :: (a -> f b) -> a -> f b 
                         where bInA = f a
                         -- nInB :: (a -> f b) -> (n a) -> f (n b)
                               nInB = traverse f n  

-- for testing 

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where 
    arbitrary = S <$> arbitrary <*> arbitrary
        
instance (Applicative n , Testable (n Property) , Eq a , Eq (n a) , EqProp a) => EqProp (S n a) where 
    (=-=) = eq


        