module Identity where 

import Test.QuickCheck

--
-- exercise 2: Identity data type
--
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (<>) a b = a

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do
        a <- arbitrary 
        return $ Identity a