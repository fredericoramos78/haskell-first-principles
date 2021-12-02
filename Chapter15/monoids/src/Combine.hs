module Combine where 


import Test.QuickCheck
--
-- exercise 9
-- 
newtype Combine a b = Combine { unCombine :: a -> b }

-- To be able to `<>` the result of the functions, `b` must be of type `Semigroup` 
instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f1) (Combine f2) = Combine { unCombine = f } where 
                                        f a = f1 a <> f2 a

instance (Monoid b) => Monoid (Combine a b) where 
    mempty = Combine (\a -> mempty)
    mappend = (<>)

-- Unsure about the `CoArbitrary` but that was GHC hint and it worked out
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where 
    arbitrary = do
        f <- arbitrary 
        return $ Combine f 

-- IDK how to build a general show & eq, as asked by quickCheck
instance (Show a, Show b) => Show (Combine a b) where 
    show (Combine f) = "Combine f(a) = b"

instance (Eq a, Eq b) => Eq (Combine a b) where 
    (==) (Combine f1) (Combine f2) = True  