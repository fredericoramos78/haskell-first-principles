module Functions where

-- Write the following functions using the methods provided by Monad and Functor. Using 
-- stuff like identity and composition is fine, but it has to type check with the 
-- types provided:

import Control.Monad

-- 1. this is join
j :: Monad m => m (m a) -> m a 
j mma = do 
    ma <- mma
    ma 
--Expecting the following behavior:
--     Prelude> j [[1, 2], [], [3]]
--     [1,2,3]
--     Prelude> j (Just (Just 1))
--     Just 1
--     Prelude> j (Just Nothing)
--     Nothing
--     Prelude> j Nothing
--     Nothing

--2. this is fmap / liftM
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = do 
    a <- ma 
    return (f a)

-- 3. This is liftM2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a = (<*>) fInA
         where fInA = fmap f a 

-- 4. 
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5. You’ll need recursion for this one:
meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = do
  y <- f x 
  ys <- meh xs f  
  return (y : ys)

-- 6. Hint: reuse meh:
flipType :: (Monad m) => [m a] -> m [a]
flipType = undefined 
