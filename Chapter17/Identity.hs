module Chapter17.Identity where

-- Write an Applicative instance for Identity: 
newtype Identity a = Identity a deriving (Eq, Ord, Show) 

instance Functor Identity where
    fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where 
    pure x = Identity x
    (<*>) (Identity f) (Identity y) = Identity (f y)
