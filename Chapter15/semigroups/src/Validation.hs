module Validation where 


import Test.QuickCheck hiding (Success, Failure)
--
-- exercise 11
-- 
data Validation a b = 
    Failure a 
  | Success b deriving (Eq, Show)

assocF :: (Semigroup a) => Validation a b -> Validation a b -> Validation a b 
assocF s@(Success _) _ = s
assocF _ s@(Success _) = s
assocF (Failure a1) (Failure a2) = Failure $ a1 <> a2

instance (Semigroup a) => Semigroup (Validation a b) where 
  (<>) = assocF

instance (Arbitrary a, Arbitrary b, Semigroup a) => Arbitrary (Validation a b) where
  arbitrary = do 
    randomA <- arbitrary 
    randomB <- arbitrary 
    oneof [ return $ Failure randomA, 
            return $ Success randomB ] 

runIt = do
  let failure :: String -> Validation String Int 
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah" 
  print $ failure "woot" <> failure "blah" 
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2