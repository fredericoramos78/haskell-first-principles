module Main where
  
import Applics.Pair 
import Applics.Two
import Applics.Three
import Applics.ThreeL
import Applics.Four
import Applics.FourL

import Combinations

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

identityLaw :: (Applicative v, Eq (v a)) => v a -> Bool 
identityLaw v = (pure id <*> v) == v 

compositionLaw :: (Applicative u, Eq (u c)) => u (b -> c) -> u (a -> b) -> u a -> Bool 
compositionLaw u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

-- homomorphismLaw :: (Eq (u b)) => (a -> b) -> a -> Bool 
-- homomorphismLaw f a = (pure f <*> pure a) == (pure (f a))

interchangeLaw :: (Applicative u, Eq (u b)) => u (a -> b) -> a -> Bool 
interchangeLaw u v = (u <*> pure v) == (pure ($ v) <*> u)

type PairIntType = Pair (Sum Int)
type PairPlusOneType = Pair (Sum Int -> Sum Int) 

type TwoIntStringType = Two (Sum Int) String 
type TwoConcatStringType = Two (Sum Int) (String -> String)

type ThreeIntStringIntType = Three (Sum Int) String (Product Int)
type ThreeProductIntType = Three (Sum Int) String (Product Int -> Product Int)

type ThreeLStringIntType = Three' String (Product Int)
type ThreeLProductIntType = Three' String (Product Int -> Product Int)

type FourStringStringStringIntType = Four String String String (Sum Int)
type FourPlusOneType = Four String String String (Sum Int -> Sum Int)

type FourLStringIntType = Four' String (Sum Int)
type FourLPlusOneType = Four' String (Sum Int -> Sum Int)



main :: IO ()
main = do
  putStrLn "" 
  putStr "Validating Pair a"
  let p = Pair ("a", "b", 1 :: Integer) ("a", "b", 1 :: Integer)
  quickBatch(applicative p)
  quickCheck (identityLaw :: PairIntType -> Bool)
  quickCheck (compositionLaw :: PairPlusOneType -> PairPlusOneType -> PairIntType -> Bool)
  quickCheck (interchangeLaw :: PairPlusOneType -> Sum Int -> Bool)
  putStrLn "" 
  putStr "Validating Two a b"
  let t = Two ("", "", 1 :: Sum Int) (1 :: Sum Int, 1 :: Product Int, "")
  quickBatch(applicative t)
  quickCheck (identityLaw :: TwoIntStringType -> Bool)
  quickCheck (compositionLaw :: TwoConcatStringType -> TwoConcatStringType -> TwoIntStringType -> Bool)
  quickCheck (interchangeLaw :: TwoConcatStringType -> [Char] -> Bool)
  putStr "Validating Three a b c"
  let th = Three ("", "", 1 :: Sum Int) (1 :: Sum Int, 1 :: Product Int, "") ("", 1 :: Sum Int, 1 :: Product Int)
  quickBatch(applicative th)
  quickCheck (identityLaw :: ThreeIntStringIntType -> Bool)
  quickCheck (compositionLaw :: ThreeProductIntType -> ThreeProductIntType -> ThreeIntStringIntType -> Bool)
  quickCheck (interchangeLaw :: ThreeProductIntType -> Product Int -> Bool)
  putStr "Validating Three' a b"
  let thL = Three' ("", "", 1 :: Sum Int) (1 :: Sum Int, 1 :: Product Int, "a") (3 :: Sum Int, 5 :: Product Int, "b")
  quickBatch(applicative thL)
  quickCheck (identityLaw :: ThreeLStringIntType -> Bool)
  quickCheck (compositionLaw :: ThreeLProductIntType -> ThreeLProductIntType -> ThreeLStringIntType -> Bool)
  quickCheck (interchangeLaw :: ThreeLProductIntType -> Product Int -> Bool)
  putStr "Validating Four a b c d"
  let fr = Four ("", "", 1 :: Sum Int) (1 :: Sum Int, 1 :: Product Int, "") ("", 1 :: Sum Int, 1 :: Product Int) ("", "", "")
  quickBatch(applicative fr)
  quickCheck (identityLaw :: FourStringStringStringIntType -> Bool)
  quickCheck (compositionLaw :: FourPlusOneType -> FourPlusOneType -> FourStringStringStringIntType -> Bool)
  quickCheck (interchangeLaw :: FourPlusOneType -> Sum Int -> Bool)
  putStr "Validating Four' a b"
  let frL = Four' ("", "", 1 :: Sum Int) ("", "", 1 :: Sum Int) ("", "", 1 :: Sum Int) ("", "", "")
  quickBatch(applicative frL)
  quickCheck (identityLaw :: FourLStringIntType -> Bool)
  quickCheck (compositionLaw :: FourLPlusOneType -> FourLPlusOneType -> FourLStringIntType -> Bool)
  quickCheck (interchangeLaw :: FourLPlusOneType -> Sum Int -> Bool)
  
  