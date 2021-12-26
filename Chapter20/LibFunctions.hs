module Chapter20.LibFunctions where 

-- Implement the functions in terms of `foldMap` or `foldr` from `Foldable`, then 
--   try them out with multiple types that have `Foldable` instances.

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' v = foldr (\x c -> x == v || c) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\c m -> case m of 
                            Nothing -> Just c 
                            Just n  | n > c -> Just c 
                            _ -> m) Nothing 

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\c m -> case m of 
                            Nothing -> Just c 
                            Just n | n < c -> Just c 
                            _  -> m) Nothing 


null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> True) False

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ a -> a + 1) 0 

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr (mappend) mempty 

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty 

