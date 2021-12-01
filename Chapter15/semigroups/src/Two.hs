module Two where 


import Test.QuickCheck

--
-- exercise 3: a Product data type
--
data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (<>) (Two a1 b1) (Two a2 b2) = Two a b
                                    where a = a1 <> a2 
                                          b = b1 <> b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do
        randomA <- arbitrary 
        randomB <- arbitrary 
        return $ Two randomA randomB 