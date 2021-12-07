module Four where 

import Test.QuickCheck



data Four a b c d = Four a b c d

instance Functor (Four a b c) where 
    fmap f (Four a b c d) = Four a b c (f d)

instance (Show a, Show b, Show c, Show d) => Show (Four a b c d) where 
    show (Four a b c d) = unwords ["Four", show a, show b, show c, show d]

instance (Eq a, Eq b, Eq c, Eq d) => Eq (Four a b c d) where 
    (==) (Four a b c d) (Four a' b' c' d') = a == a' && b == b' && c == c' && d == d' 

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where 
    arbitrary = do 
        randomA <- arbitrary
        randomB <- arbitrary
        randomC <- arbitrary
        randomD <- arbitrary
        return $ Four randomA randomB randomC randomD 