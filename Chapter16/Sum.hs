module Chapter16.Sum where

data Sum a b = 
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where 
    fmap = applyIfSecond

applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
applyIfSecond _ (First a) = First a
applyIfSecond f (Second b) = Second (f b)

-- Why is a Functor instance that applies a function only to First, Either’s Left, 
-- impossible? We covered this earlier.
-- Solution: I don't think there's a way to fix the second type variable and vary on the first one