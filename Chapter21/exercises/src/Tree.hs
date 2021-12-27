module Tree where

import Control.Applicative
import Data.Traversable

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Tree a = 
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where 
    fmap _ Empty = Empty 
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr, too, for extra credit.
instance Foldable Tree where
    foldMap f Empty = mempty 
    foldMap f (Leaf a) = f a
    foldMap f (Node l a r) = l' <> f a <> r'
                               where l' = foldMap f l
                                     r' = foldMap f r 

instance Traversable Tree where
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a 
    traverse f (Node l a r) = liftA3 Node l' (f a) r'
                               where l' = traverse f l
                                     r' = traverse f r 


-- for testing

instance (Arbitrary a) => Arbitrary (Tree a) where 
    arbitrary = do 
        l <- arbitrary 
        a <- arbitrary
        r <- arbitrary 
        frequency [(1, return Empty),
                   (4, return $ Leaf a),
                   (5, return $ Node (Leaf l) a (Leaf r))]

instance (Eq a) => EqProp (Tree a) where 
    (=-=) = eq 
