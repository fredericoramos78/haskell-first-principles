module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- This one should be easier than the Applicative instance was. Remember to use 
-- the Functor that Monad requires, then see where the chips fall:

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons y ys 
                           where y = f x 
                                 ys = fmap f xs 

append :: List a -> List a -> List a
append Nil v = v 
append v Nil = v 
append (Cons x xs) y = Cons x $ xs `append` y

instance Applicative List where 
    pure = flip Cons Nil 
    -- no functions to apply
    (<*>) Nil _ = Nil
    -- no values to apply, then it doesn't really matter what functions we have 
    (<*>) _ Nil = Nil
    -- apply each function to all values
    (<*>) (Cons f fs) xs = ys `append` zs
                              -- values applied to `f` 
                        where ys = fmap f xs
                              -- values applied to the remaining functions
                              zs = fs <*> xs

instance Monad List where 
    return = pure 
    (>>=) Nil _ = Nil 
    (>>=) (Cons x xs) f = Cons y ys 
                            where (Cons y _) = f x 
                                  ys = xs >>= f 

-- for testing 

instance (Arbitrary a) => Arbitrary (List a) where 
    arbitrary = do
        randomA <- arbitrary 
        frequency [ (9, return $ Cons randomA Nil),
                    (1, return Nil) ]

instance (Eq a) => EqProp (List a) where 
    (=-=) = eq 