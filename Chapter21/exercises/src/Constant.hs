module Constant where

import Data.Traversable

import Test.QuickCheck 
import Test.QuickCheck.Classes 
import Test.QuickCheck.Checkers 

-- Just pass-through (=skip) any function application since 
-- it should all operate on `b` but it's a ghost type variable
newtype Constant a b = Constant { getConstant :: a } deriving (Show, Eq)

instance Functor (Constant a) where 
    fmap _ a = Constant $ getConstant a 

instance Foldable (Constant a) where 
    foldr _ i (Constant a) = i 

instance Traversable (Constant a) where 
    traverse _ (Constant a) = pure $ Constant a

-- for testing 
instance Arbitrary a => Arbitrary (Constant a b) where 
    arbitrary = Constant <$> arbitrary 

instance Eq a => EqProp (Constant a b) where 
    (=-=) = eq 

