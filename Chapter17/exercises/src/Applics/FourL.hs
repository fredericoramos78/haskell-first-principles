module Applics.FourL where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where 
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where 
    pure = Four' mempty mempty mempty 
    (<*>) (Four' a1 a1' a1'' f) (Four' a2 a2' a2'' x) = Four' (a1 <> a2) (a1' <> a2') (a1'' <> a2'') (f x)

-- for testing 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where 
    arbitrary = do 
        randomA <- arbitrary 
        randomA' <- arbitrary 
        randomA'' <- arbitrary 
        Four' randomA randomA' randomA'' <$> arbitrary 

instance (Eq a, Eq b) => EqProp (Four' a b) where 
    (=-=) = eq 
