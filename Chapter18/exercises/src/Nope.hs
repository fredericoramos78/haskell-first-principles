module Nope where


import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where 
    fmap _ _ = NopeDotJpg

instance Applicative Nope where 
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where 
    return = pure
    (>>=) _ _ = NopeDotJpg

-- for testing 

instance Arbitrary (Nope a) where 
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where 
    (=-=) = eq 
