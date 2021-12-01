module Three where 
    
import Test.QuickCheck
--
-- exercise 4: a triple arg data type
--
data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where 
    (<>) (Three a1 b1 c1) (Three a2 b2 c2) = Three a b c 
                                                where a = a1 <> a2 
                                                      b = b1 <> b2
                                                      c = c1 <> c2

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do 
        randomA <- arbitrary
        randomB <- arbitrary
        randomC <- arbitrary
        return $ Three randomA randomB randomC 
