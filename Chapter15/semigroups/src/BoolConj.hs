module BoolConj where 

import Test.QuickCheck
--
-- exercise 6
--
newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
    arbitrary = do
        bool <- arbitrary 
        return $ BoolConj bool