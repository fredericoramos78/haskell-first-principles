module List where

import Data.Traversable
import Control.Applicative


import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where 
    fmap _ Nil = Nil 
    fmap f (Cons x xs) = Cons y (fmap f xs)
                         where y = f x 

instance Foldable List where 
    foldr _ i Nil = i 
    foldr f i (Cons x xs) = f x ys
                            where ys = foldr f i xs 

-- lift Cons constructor applying to y (=f b) and ys (=f (List b))
instance Traversable List where 
    traverse _ Nil = pure Nil 
    traverse f (Cons x xs) = liftA2 Cons y ys 
                                where y = f x 
                                      ys = traverse f xs 

-- for testing

instance Arbitrary a => Arbitrary (List a) where 
    arbitrary = frequency[ (1, return Nil),
                           (3, flip Cons Nil <$> arbitrary),
                           (6, (Cons <$> arbitrary) <*> (flip Cons Nil <$> arbitrary))]

instance Eq a => EqProp (List a) where 
    (=-=) = eq 
