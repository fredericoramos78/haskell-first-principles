module Chapter16.Possibly where

data Possibly a = 
      LolNope
    | Yeppers a 
    deriving (Eq, Show)

instance Functor Possibly where 
    fmap = applyIfJust

applyIfJust :: (a -> b) -> Possibly a -> Possibly b
applyIfJust _ LolNope = LolNope
applyIfJust f (Yeppers a) = Yeppers (f a)