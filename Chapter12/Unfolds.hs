module Chapter12.Unfolds where

-- 1. Write the function myIterate using direct recursion. Compare the behavior with the built-in iterate to 
-- gauge correctness. Do not look at the source or any examples of iterate, so that you are forced to do this yourself:
myIterate :: (a -> a) -> a -> [a] 
myIterate f i = i : myIterate f (f i)

-- 2. Write the function myUnfoldr using direct recursion. Compare with the built-in unfoldr to check your implementation. 
-- Again, don’t look at implementations of unfoldr, so that you figure it out yourself:
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
myUnfoldr f i = case f i of
                  Nothing -> []
                  Just (v, n) -> v : myUnfoldr f n

-- 3. Rewrite myIterate into betterIterate using myUnfoldr. A hint—we use unfoldr to produce the same results as iterate 
-- above. Do this with different functions, and see if you can abstract the structure out.
-- It helps to have the types in front of you:

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a' -> Just (a', f a'))
