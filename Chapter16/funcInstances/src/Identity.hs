module Identity where 

import Test.QuickCheck 

newtype Identity a = Identity a 

instance Functor Identity where 
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do
        a <- arbitrary 
        return $ Identity a 

instance Show a => Show (Identity a) where 
    show (Identity a) = "Identity " ++ (show a)

instance Eq a => Eq (Identity a) where 
    (==) (Identity a) (Identity a') = a == a' 

