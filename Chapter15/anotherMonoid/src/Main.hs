module Main where

import Optional
import Test.QuickCheck



newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup a => Semigroup (First' a) where 
    (<>) (First' a) (First' b) = First' (a <> b)

instance Semigroup a => Monoid (First' a) where 
    mempty = First' Nada 

firstMappend :: Semigroup a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

instance (Arbitrary a) => Arbitrary (First' a) where 
  arbitrary = do
    v <- arbitrary
    frequency [ (9, return $ First' (Only v)),
                (1, return $ First' Nada) ]

-- monoid law tests
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

main :: IO () 

-- this didn't work for Num/Int/Integer
main = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FstId) 
    quickCheck (monoidRightIdentity :: FstId)
    let onlyOne = First' (Only "1")
    let onlyTwo = First' (Only "2")
    let nada = First' Nada
    print $ (onlyOne `mappend` nada)
    print $ ((nada `mappend` nada) :: (First' String))
    print $ nada `mappend` onlyTwo
    print $ onlyOne `mappend` onlyTwo



