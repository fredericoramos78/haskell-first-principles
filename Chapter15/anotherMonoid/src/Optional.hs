module Optional where

import Data.Monoid
import Control.Monad
import Test.QuickCheck

data Optional a = 
    Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where 
    (<>) Nada     y = y
    (<>) x        _ = x


instance Monoid a => Monoid (Optional a) where
    mempty = Nada 
