module Big where

import Data.Traversable
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers 



data Big a b = Big a b b deriving (Show, Eq)

instance Functor (Big a) where 
    fmap f (Big a b b') = Big a c c' 
                            where c = f b 
                                  c' = f b' 

instance Foldable (Big a) where 
    foldr f i (Big a b b') = f b i' 
                              where i' = f b' i 

instance Traversable (Big a) where 
    traverse f (Big a b b') =  let c  = f b
                                   c' = f b' 
                                   a' = pure a 
                                in liftA3 Big a' c c' 

-- for testing 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where 
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary 
        Big a b <$> arbitrary 

instance (Eq a, Eq b) => EqProp (Big a b) where 
    (=-=) = eq 