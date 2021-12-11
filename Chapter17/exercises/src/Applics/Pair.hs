module Applics.Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where 
    fmap f (Pair x1 x2) = Pair (f x1) (f x2)

instance Applicative Pair where 
    pure x = Pair x x 
    (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)

-- for testing 

instance (Arbitrary a) => Arbitrary (Pair a) where 
    arbitrary = do 
        randomA1 <- arbitrary 
        Pair randomA1 `fmap` arbitrary 

instance (Eq a) => EqProp (Pair a) where 
    (=-=) = eq

instance (Semigroup a) => Semigroup (Pair a) where 
    (<>) (Pair x1 x2) (Pair y1 y2) = Pair (x1 <> y1) (x2 <> y2)

instance (Monoid a) => Monoid (Pair a) where 
    mempty = Pair mempty mempty
    mappend = (<>)