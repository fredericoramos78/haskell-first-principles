module Applics.Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where 
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where 
    pure d = Four mempty mempty mempty d
    (<*>) (Four a b c f) (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)

-- for testing 

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where 
    arbitrary = do
        randomA <- arbitrary
        randomB <- arbitrary
        randomC <- arbitrary
        (<$>) (Four randomA randomB randomC) arbitrary 

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where 
    (=-=) = eq
