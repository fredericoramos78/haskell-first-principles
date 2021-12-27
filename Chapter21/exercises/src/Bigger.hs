module Bigger where

import Data.Traversable
import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers


data Bigger a b = Bigger a b b b deriving (Show, Eq)

instance Functor (Bigger a) where 
    fmap f (Bigger a b b' b'') = Bigger a c c' c'' 
                                   where c = f b 
                                         c' = f b' 
                                         c'' = f b'' 

instance Foldable (Bigger a) where 
    foldr f i (Bigger a b b' b'') = foldr f i [b, b', b'']

instance Traversable (Bigger a) where 
    traverse f (Bigger a b b' b'') = liftA3 (Bigger a) c c' c'' 
                                        where c = f b 
                                              c' = f b' 
                                              c'' = f b'' 

-- for testing 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where 
    arbitrary = do 
        a <- arbitrary 
        b <- arbitrary 
        b' <- arbitrary
        Bigger a b b' <$> arbitrary 

instance (Eq a, Eq b) => EqProp (Bigger a b) where 
    (=-=) = eq 
