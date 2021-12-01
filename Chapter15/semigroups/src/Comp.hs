module Comp where 

import Test.QuickCheck
--
-- exercise 10
--
newtype Comp a = Comp { unComp :: a -> a } 

instance Semigroup (Comp a) where 
    (<>) (Comp f1) (Comp f2) = Comp f where 
                                f = f1 . f2

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where 
    arbitrary = do
        f <- arbitrary 
        return $ Comp f 

instance (Show a) => Show (Comp a) where 
    show (Comp f) = "Comp f()"

instance (Eq a) => Eq (Comp a) where
    (==) (Comp a1) (Comp a2) = True