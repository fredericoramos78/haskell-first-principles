module Identity where


import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity a) = Identity $ f a 

instance Applicative Identity where 
    pure = Identity
    (<*>) (Identity f) a = fmap f a 

instance Monad Identity where 
    return = pure
    (>>=) (Identity a) f = f a 

-- for testing 

instance (Arbitrary a) => Arbitrary (Identity a) where 
    arbitrary = return <$> arbitrary 

instance (Eq a) => EqProp (Identity a) where 
    (=-=) = eq 