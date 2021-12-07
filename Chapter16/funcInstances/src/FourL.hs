module FourL where 

import Test.QuickCheck 



data Four' a b = Four' a a a b

instance Functor (Four' a) where 
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Show a, Show b) => Show (Four' a b) where 
    show (Four' a1 a2 a3 b) = unwords ["Four'", show a1, show a2, show a3, show b]

instance (Eq a, Eq b) => Eq (Four' a b) where 
    (==) (Four' a1 a2 a3 b) (Four' a'1 a'2 a'3 b') = a1 == a'1 && a2 == a'2 && a3 == a'3 && b == b' 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where 
    arbitrary = do
        randomA1 <- arbitrary
        randomA2 <- arbitrary
        randomA3 <- arbitrary
        randomB <- arbitrary
        return $ Four' randomA1 randomA2 randomA3 randomB