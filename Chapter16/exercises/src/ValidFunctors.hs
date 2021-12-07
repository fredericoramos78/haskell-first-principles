module ValidFunctors where

import GHC.Arr 

-- 1. 
-- Solution: Cannot since kind is `*`
data Bool = False | True

-- 2. 
-- Solution: This can have a Functor
data BoolAndSomethingElse a = False' a | True' a

fmapFunc :: (a -> b) -> BoolAndSomethingElse a -> BoolAndSomethingElse b
fmapFunc f (False' v) = False' (f v)
fmapFunc f (True' v) = True' (f v)
instance Functor BoolAndSomethingElse where 
    fmap = fmapFunc 

-- 3.
-- Solution: Still possible 
data BoolAndMaybeSomethingElse a = Falsish | Truish a

fmapFunc' :: (a -> b) -> BoolAndMaybeSomethingElse a -> BoolAndMaybeSomethingElse b
fmapFunc' _ Falsish = Falsish 
fmapFunc' f (Truish a) = Truish (f a)
instance Functor BoolAndMaybeSomethingElse where 
    fmap = fmapFunc' 

-- 4.
-- Use the kinds to guide you on this one—don’t get too hung up on the details:
-- Solution: Nope. Kind here is `* -> * -> *` since `f` is a function (`* -> *`)
newtype Mu f = InF { outF :: f (Mu f) }

-- 5. 
-- Again, follow the kinds, and ignore the unfamiliar parts:
-- Solution: Nope since `D` is kind `*` 
data D = D (Array Word Word) Int Int