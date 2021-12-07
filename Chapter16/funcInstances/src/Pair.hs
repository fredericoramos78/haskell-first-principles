module Pair where 

import Test.QuickCheck 

-- Pair is still of kind `* -> *`, although it holds two instances of `a` 
data Pair a = Pair a a

instance Functor Pair where 
    -- Have to apply for both vars as they are of the same (single) type arg "a"
    fmap f (Pair a b) = Pair (f a) (f b)

instance Show a => Show (Pair a) where 
    show (Pair a1 a2) = unwords ["Pair", show a1, show a2]

instance Eq a => Eq (Pair a) where 
    (==) (Pair a1 a2) (Pair b1 b2) = a1 == b1 && a2 == b2 

instance Arbitrary a => Arbitrary (Pair a) where 
    arbitrary = do 
        a <- arbitrary 
        return $ Pair a a