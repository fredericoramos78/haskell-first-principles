module Main where


import Trivial
import Identity
import Two 
import Combine
import Comp
import BoolConj
import BoolDisj
import Mem

import Test.QuickCheck
import Data.Monoid (Sum)


-- monoid law tests
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

-- concretization of types for functions
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentitySum = Identity (Sum Int)
type IdentitySumAssoc = IdentitySum -> IdentitySum -> IdentitySum -> Bool 

type TwoIntString = Two (Sum Int) String 
type TwoIntStringAssoc = TwoIntString -> TwoIntString -> TwoIntString -> Bool 

type CombineIntString = Combine Int String 
type CombineIntStringAssoc = CombineIntString -> CombineIntString -> CombineIntString -> Bool 

type CompInt = Comp (Sum Int)
type CompIntAssoc = CompInt -> CompInt -> CompInt -> Bool 

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool 
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool 

type MemIntString = Mem Int String
type MemIntStringAssoc = MemIntString -> MemIntString -> MemIntString -> Bool 

main :: IO () 
main = do
  quickCheck (monoidAssoc :: TrivAssoc) 
  quickCheck (monoidLeftIdentity :: Trivial -> Bool) 
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentitySumAssoc) 
  quickCheck (monoidLeftIdentity :: IdentitySum -> Bool) 
  quickCheck (monoidRightIdentity :: IdentitySum -> Bool)
  quickCheck (monoidAssoc :: TwoIntStringAssoc) 
  quickCheck (monoidLeftIdentity :: TwoIntString -> Bool) 
  quickCheck (monoidRightIdentity :: TwoIntString -> Bool)
  quickCheck (monoidAssoc :: CombineIntStringAssoc) 
  quickCheck (monoidLeftIdentity :: CombineIntString -> Bool) 
  quickCheck (monoidRightIdentity :: CombineIntString -> Bool)
  quickCheck (monoidAssoc :: CompIntAssoc) 
  quickCheck (monoidLeftIdentity :: CompInt -> Bool) 
  quickCheck (monoidRightIdentity :: CompInt -> Bool)
  quickCheck (monoidAssoc :: BoolConjAssoc) 
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool) 
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolDisjAssoc) 
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool) 
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidAssoc :: MemIntStringAssoc) 
  quickCheck (monoidLeftIdentity :: MemIntString -> Bool) 
  quickCheck (monoidRightIdentity :: MemIntString -> Bool)
