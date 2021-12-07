module Two where 

import Test.QuickCheck 



data Two a b = Two a b

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b)

instance (Show a, Show b) => Show (Two a b) where 
    show (Two a b) = unwords ["Two", show a, show b]

instance (Eq a, Eq b) => Eq (Two a b) where 
    (==) (Two a b) (Two a' b') = a == a' && b == b' 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do
        randomA <- arbitrary 
        randomB <- arbitrary 
        return $ Two randomA randomB