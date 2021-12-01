module BoolDisj where 


import Test.QuickCheck
--
-- exercise 7
--
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where 
    (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where 
    arbitrary = do
        bool <- arbitrary 
        return $ BoolDisj bool 