module Chapter15.Optional where

import Data.Monoid

data Optional a = 
    Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where 
    (<>) (Only x) (Only y) = Only (x <> y)
    (<>) Nada     y = y
    (<>) x     Nada = x


instance Monoid a => Monoid (Optional a) where
    mempty = Nada 
    mappend = (<>)

