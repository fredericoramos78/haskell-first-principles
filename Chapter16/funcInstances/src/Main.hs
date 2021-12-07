module Main where

import Identity 
import Pair 
import Two 
import Three 
import ThreeL 
import Four
import FourL 

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool 
functorIdentity a = fmap id a == a 

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g a = fmap g (fmap f a) == (fmap (g . f) a)


-- realizing types 
type IdentityType = Identity Int 
type IdentityTypeFunc = IdentityType -> Bool

type PairType = Pair Int
type PairTypeFunc = PairType -> Bool

type TwoIntStringType = Two Int String 
type TwoIntStringTypeFunc = TwoIntStringType -> Bool 

type ThreeStringStringInt = Three String String Int 
type ThreeStringStringIntFunc = ThreeStringStringInt -> Bool 

type ThreeLStringInt = Three' String Int 
type ThreeLStringIntFunc = ThreeLStringInt -> Bool 

type FourStringStringStringInt = Four String String String Int 
type FourStringStringStringIntFunc = FourStringStringStringInt -> Bool

type FourLStringInt = Four' String Int 
type FourLStringIntFunc = FourLStringInt -> Bool

-- 8. Can you implement one for this type? Why? Why not?
-- Solution: Cannot implement b/c Trivial kind is `*` and Functor requires types of `* -> *`. In other
--   words, Trivial is not a "value container" so there's nothing to unwrap and transform (=fmap)
data Trivial = Trivial

main :: IO ()
main = do
  putStrLn "a => Identity a"
  quickCheck (functorIdentity :: IdentityTypeFunc)
  quickCheck (functorCompose (+1) (1-) :: IdentityTypeFunc)
  putStrLn "a => Pair a a"
  quickCheck (functorIdentity :: PairTypeFunc)
  quickCheck (functorCompose (+1) (1-) :: PairTypeFunc)
  putStrLn "a b => Two a b"
  quickCheck (functorIdentity :: TwoIntStringTypeFunc)
  quickCheck (functorCompose ("["++) (++"]") :: TwoIntStringTypeFunc)
  putStrLn "a b c => Three a b c"
  quickCheck (functorIdentity :: ThreeStringStringIntFunc)
  quickCheck (functorCompose (+1) (1-) :: ThreeStringStringIntFunc)
  putStrLn "a b => Three' a b b"
  quickCheck (functorIdentity :: ThreeLStringIntFunc)
  quickCheck (functorCompose (+1) (1-) :: ThreeLStringIntFunc)
  putStrLn "a b c d => Four a b c d"
  quickCheck (functorIdentity :: FourStringStringStringIntFunc)
  quickCheck (functorCompose (+1) (1-) :: FourStringStringStringIntFunc)
  putStrLn "a b => Four' a a a b"
  quickCheck (functorIdentity :: FourLStringIntFunc)
  quickCheck (functorCompose (+1) (1-) :: FourLStringIntFunc)