module Lists where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where 
    fmap _ Nil = Nil 
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where 
    pure a = Cons a Nil  
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = fmap f xs `append` (fs <*> xs)

append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys


-- for testing

instance (Arbitrary a) => Arbitrary (List a) where 
    arbitrary = do 
        randomA <- arbitrary 
        frequency [ (8, return $ Cons randomA Nil),
                    (2, return Nil)]

instance (Eq a) => EqProp (List a) where 
    (=-=) = eq

instance (Semigroup a) => Semigroup (List a) where 
    (<>) (Cons x xs) (Cons y ys) = Cons (x <> y) (xs <> ys)
    (<>) Nil x = x 
    (<>) x Nil = x 

instance (Monoid a) => Monoid (List a) where 
    mempty = Nil
    mappend = (<>)