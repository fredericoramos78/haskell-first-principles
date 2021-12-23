module BadMonad where


import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data CountMe a = CountMe Integer a deriving (Show, Eq) 

instance Functor CountMe where 
    fmap f (CountMe n a) = CountMe (n+1) $ f a

instance Applicative CountMe where 
    pure = CountMe 0 
    (<*>) (CountMe n f) (CountMe n' a) = CountMe (n + n') $ f a 

instance Monad CountMe where 
    return = pure 
    (>>=) (CountMe n a) f = 
        -- Is this using scope/shadowing to create a function called CountMe?
        let CountMe _ b = f a 
        in CountMe (n+1) b 


-- for testing 
instance (Arbitrary a) => Arbitrary (CountMe a) where 
    arbitrary = CountMe <$> arbitrary <*> arbitrary 

instance (Eq a) => EqProp (CountMe a) where 
    (=-=) = eq 
