module Or where 

import Test.QuickCheck
--
-- exercise 8
--
data Or a b = 
    Fst a 
  | Snd b
  deriving (Show, Eq)

assocF :: Or a b -> Or a b -> Or a b
assocF a@(Snd _) _ = a 
assocF (Fst _) b@(Snd _) = b 
assocF _ b = b 

instance Semigroup (Or a b) where 
  (<>) = assocF 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    oneof [ return $ Fst a, 
            return $ Snd b ]

