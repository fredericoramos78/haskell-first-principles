module Optional where

import Data.Traversable

import Test.QuickCheck 
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers 


data Optional a = 
      Nada 
    | Yep a
    deriving (Show, Eq)


instance Functor Optional where 
    fmap _ Nada = Nada 
    fmap f (Yep a) = Yep $ f a 

instance Foldable Optional where 
    foldr _ i Nada = i 
    foldr f i (Yep a) = f a i

instance Traversable Optional where 
    traverse _ Nada = pure Nada 
    traverse f (Yep a) = Yep <$> f a 

-- for testing 

instance Arbitrary a => Arbitrary (Optional a) where 
    arbitrary = frequency [ (1, return Nada),
                            (9, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where 
    (=-=) = eq 