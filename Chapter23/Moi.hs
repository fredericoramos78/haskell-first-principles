{-# LANGUAGE InstanceSigs #-}

module Chapter23.Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) } 

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b 
    fmap f (Moi g) = Moi (\s -> (f . fst . g $ s, s)) 
 
instance Applicative (Moi s) where 
    pure :: a -> Moi s a
    -- simulates `Moi (\s -> (a, s))` since `const` is `a -> b -> a` 
    pure a = Moi $ (,) a
    
    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi (\s -> ((fst . f $ s) (fst . g $ s), s))

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b 
    (Moi f) >>= g = Moi (\s -> (fst $ runMoi (g . fst . f $ s) s, s))