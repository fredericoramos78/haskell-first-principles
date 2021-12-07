module Three where 

import Test.QuickCheck 


data Three a b c = Three a b c

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

instance (Show a, Show b, Show c) => Show (Three a b c) where 
    show (Three a b c) = unwords ["Three", show a, show b, show c]

instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where 
    (==) (Three a b c) (Three a' b' c') = a == a' && b == b' && c == c' 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do
        randomA <- arbitrary
        randomB <- arbitrary
        randomC <- arbitrary
        return $ Three randomA randomB randomC 
