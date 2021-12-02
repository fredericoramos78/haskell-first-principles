module Trivial where


import Data.Monoid
import Test.QuickCheck

--
-- exercise 1: Trivial data type
--
data Trivial = Trivial deriving (Show, Eq)

instance Semigroup Trivial where 
  (<>) _ _ = Trivial 

instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where 
  arbitrary = return Trivial 
 