module ZipLists where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes



newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where 
    xs =-= ys = xs' `eq` ys'
                 where xs' = let (ZipList' l) = xs in take 3000 l
                       ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where 
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
    pure x = ZipList' $ [x] 
    (<*>) (ZipList' []) (ZipList' _) = ZipList' []
    (<*>) (ZipList' _) (ZipList' []) = ZipList' []
    (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ applyF fs xs

applyF :: [a -> b] -> [a] -> [b]
applyF [] _ = []
applyF _ [] = []
applyF [f] (x:xs) = f x : applyF [f] xs
applyF (f:fs) [x] = f x : applyF fs [x]
applyF (f:fs) (x:xs) = f x : applyF fs xs

-- for testing 

instance (Arbitrary a) => Arbitrary (ZipList' a) where 
    arbitrary = do
        randomA <- arbitrary 
        return $ ZipList' randomA

instance (Semigroup a) => Semigroup (ZipList' a) where 
    (<>) (ZipList' x) (ZipList' y) = ZipList' (x <> y)

instance (Monoid a) => Monoid (ZipList' a) where 
    mempty = ZipList' mempty 
    mappend = (<>)


