module Chapter20.Optional where 

import Data.Monoid

data Optional a = Nada | Yep a deriving (Show, Eq)

instance Semigroup a => Semigroup (Optional a) where 
    (<>) (Yep a) Nada = Yep a
    (<>) Nada (Yep a) = Yep a
    (<>) _ _ = Nada 

instance Monoid a => Monoid (Optional a) where 
    mempty = Nada 
    mappend = (<>)

-- foldable instance 

instance Foldable Optional where 
    foldr _ i Nada = i 
    foldr f i (Yep a) = f a i 

    foldl _ i Nada = i
    foldl f i (Yep a) = f i a 

    foldMap _ Nada = mempty 
    foldMap f (Yep a) = f a 