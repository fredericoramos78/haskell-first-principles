module Chapter20.Exercises where

-- 1. 
data Constant a b = Constant b 

instance Foldable (Constant a) where 
    foldr f i (Constant x) = f x i

-- 2. 
data Two a b = Two a b
instance Foldable (Two a) where 
    foldr f i (Two x y) = f y i

-- 3. 
data Three a b c = Three a b c
instance Foldable (Three a b) where 
    foldr f i (Three x y z) = f z i

-- 4.
data Three' a b = Three' a b b
instance Foldable (Three' a) where 
    foldr f i (Three' _ x x') = f x $ f x' i

-- 5. 
data Four' a b = Four' a b b b
instance Foldable (Four' a) where 
    foldr f i (Four' _ x x' x'') = foldr f i [x, x', x'']

-- Thinking cap time. Write a filter function for Foldable types using the foldMap function:
filterF :: (Applicative f, Foldable t, Monoid (f a), Monoid a) => (a -> Bool) -> t a -> f a
filterF f t = pure $ mconcat filtered 
               where filtered = foldMap (mergeElement f) t 

mergeElement :: (a -> Bool) -> a -> [a]
mergeElement f e = [e | f e]