module Validations where


import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Validation e a = 
      Failure e
    | Success a 
    deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where 
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

-- This is different
instance Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) = applyF

applyF :: (Semigroup e) => Validation e (a -> b) -> Validation e a -> Validation e b
applyF (Failure e) (Failure e') = Failure (e <> e')
applyF (Failure e) _ = Failure e
applyF _ (Failure e) = Failure e
applyF (Success f) (Success x) = Success (f x)

instance (Semigroup a, Semigroup e) => Semigroup (Validation e a) where 
    (<>) = appendF

appendF :: (Semigroup a, Semigroup e) => Validation e a -> Validation e a -> Validation e a 
appendF (Failure e) (Failure e') = Failure (e <> e')
appendF _ (Failure e) = Failure e
appendF (Failure e) _ = Failure e
appendF (Success a) (Success a') = Success (a <> a')

instance (Monoid a, Monoid e) => Monoid (Validation e a) where 
    mempty = Success mempty 
    mappend = (<>)

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where 
    arbitrary = do 
        randomA <- arbitrary 
        randomE <- arbitrary 
        frequency [(1, return $ Failure randomE), 
                   (1, return $ Success randomA)]    

instance (Eq a, Eq e) => EqProp (Validation e a) where 
    (=-=) = eq