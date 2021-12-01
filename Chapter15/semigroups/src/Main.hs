module Main where

import Trivial
import Identity
import Two
import Three
import Four 
import BoolConj
import BoolDisj
import Or 
import Combine
import Comp 
import Validation

import Data.Semigroup (Sum, Product)
import Test.QuickCheck 


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == (a <> b) <> c

-- typing functions
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IntSum = Sum Int
type IntProd = Product Int

type IdentityInt = Identity IntSum
type IdentityIntAssoc = IdentityInt -> IdentityInt -> IdentityInt -> Bool 

type TwoIntString = Two IntSum String
type TwoIntStringAssoc = TwoIntString -> TwoIntString -> TwoIntString -> Bool 

type ThreeIntStringString = Three IntSum String String 
type ThreeIntStringStringAssoc = ThreeIntStringString -> ThreeIntStringString -> ThreeIntStringString -> Bool

type FourIntIntIntString = Four IntSum IntSum IntProd String 
type FourIntIntIntStringAssoc = FourIntIntIntString -> FourIntIntIntString -> FourIntIntIntString -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool 

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrIntString = Or Int String 
type OrIntStringAssoc = OrIntString -> OrIntString -> OrIntString -> Bool

type CombineIntString = Combine Int String 
type CombineIntStringAssoc = CombineIntString -> CombineIntString -> CombineIntString -> Bool

type CompInt = Comp Int 
type CompIntAssoc = CompInt -> CompInt -> CompInt -> Bool

type ValidationStringInt = Validation String (Sum Int)
type ValidationStringIntAssoc = ValidationStringInt -> ValidationStringInt -> ValidationStringInt -> Bool 

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityIntAssoc)
  quickCheck (semigroupAssoc :: TwoIntStringAssoc)
  quickCheck (semigroupAssoc :: ThreeIntStringStringAssoc)
  quickCheck (semigroupAssoc :: FourIntIntIntStringAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrIntStringAssoc)
  quickCheck (semigroupAssoc :: CombineIntStringAssoc)
  quickCheck (semigroupAssoc :: CompIntAssoc)
  quickCheck (semigroupAssoc :: ValidationStringIntAssoc)
