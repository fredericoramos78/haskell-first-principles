module BahEither where


import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- NOTE: Either is inverted so Left has the value and Right has the error
data BahEither b a = PLeft a | PRight b deriving (Show, Eq)

instance Functor (BahEither b) where 
    fmap _ (PRight b) = PRight b 
    fmap f (PLeft a) = PLeft (f a)

instance Applicative (BahEither b) where 
    pure = PLeft 
    (<*>) _ (PRight b) = PRight b
    (<*>) (PRight b) _ = PRight b
    (<*>) (PLeft f) a = fmap f a

instance Monad (BahEither b) where 
    return = pure 
    (>>=) (PRight b) _ = PRight b
    (>>=) (PLeft a) f = f a 

-- for testing 
instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where 
    arbitrary = 
        frequency [ (8, PLeft <$> arbitrary), 
                    (2, PRight <$> arbitrary) ]

instance (Eq a, Eq b) => EqProp (BahEither a b) where 
    (=-=) = eq 