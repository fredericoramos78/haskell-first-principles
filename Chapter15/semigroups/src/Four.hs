module Four where 

import Test.QuickCheck
--
-- exercise 5: a four args data type
-- 
data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where 
    (<>) (Four a1 b1 c1 d1) (Four a2 b2 c2 d2) = Four a b c d 
                                                    where a = a1 <> a2
                                                          b = b1 <> b2 
                                                          c = c1 <> c2 
                                                          d = d1 <> d2 

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where 
    arbitrary = do 
        randomA <- arbitrary
        randomB <- arbitrary
        randomC <- arbitrary
        randomD <- arbitrary
        return $ Four randomA randomB randomC randomD 