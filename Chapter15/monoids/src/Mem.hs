module Mem where


import Data.Monoid
import Test.QuickCheck

--
-- exercise 8: Mem
-- This next exercise will involve doing something that will still feel a bit unnatural, and you may 
--    find it difficult. If you get it, and you haven’t done much FP or Haskell before, get yourself 
--    a nice beverage. We’re going to toss you the instance declaration, so you don’t churn on a missing 
--    Monoid constraint you didn’t know you need:
--
newtype Mem s a = Mem {
  runMem :: s -> (a,s) 
 }

-- run f1 with the initial s, then take the s resulting and feed into f2
-- resulting a's should be <>
assocF :: (Semigroup a) => (s -> (a, s)) -> (s -> (a, s)) -> s -> (a, s)
assocF f1 f2 s = 
  let 
    (a1, s1) = f1 s
    (a2, s2) = f2 s1 
  in (a1 <> a2, s2)

instance Semigroup a => Semigroup (Mem s a) where 
  (<>) (Mem f1) (Mem f2) = Mem $ assocF f1 f2

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

-- IDK how to build a general show & eq, as asked by quickCheck
instance Show (Mem s a) where 
    show (Mem f) = "Mem f(s) = (a,s)"

instance Eq (Mem s a) where 
    (==) (Mem f1) (Mem f2) = True 

instance (Arbitrary a, Arbitrary s) => Arbitrary (Mem a s) where 
  arbitrary = do 
    randomA <- arbitrary
    randomS <- arbitrary 
    return $ Mem $ const (randomA, randomS)

f' = Mem $ \s -> ("hi", s + 1)

runIt = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0 
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int)) 
  print $ rmleft == runMem f' 0 
  print $ rmright == runMem f' 0
 