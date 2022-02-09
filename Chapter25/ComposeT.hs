{-# LANGUAGE InstanceSigs #-}

module Chapter25.ComposeT where

newtype Compose f g a = Compose { runCompose :: f (g a) } deriving (Show, Eq)


instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga 


-- Didn't get this! :(
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a 
    -- g :: a -> b
    -- f :: b -> c
    --     Compose    f    ( g a )
    pure = Compose . pure . pure 

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a 
    -- https://stackoverflow.com/questions/58054217/composing-applicatives


-- Write the Compose Foldable instance.
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    -- `foldMap :: (a -> b) -> (t a) -> b`
    --   `f` is of type `a -> m`
    --   `(t a)` is of type `f (g a)` with `f` being a `Foldable`
    --    ==> The values (=`a`) inside the Foldable `t` are of type `(g a)`
    --    ==> function `f` must be `(g a) -> b` 
    --   `g` is also a `Foldable` and we can fold it via `foldMap (a -> b) (g a)` 
    --    ==> partially applying `foldMap f` is a function of `t a -> b` where `t` is a Foldable
    --    ==> exactly what we need to build the function that can fold `f (g a)` 
    foldMap f (Compose fga) = foldMap (foldMap f) fga        

-- Write the Compose Traversable instance:
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    -- first level  ==> `traverse ((g a) -> A b) -> f (g a) -> A (f b)
    -- second level ==> `traverse (a -> A b) ==> (g a) -> A (g b)
    traverse f (Compose fga) = Compose <$> traverse y fga
                                where y = traverse f  