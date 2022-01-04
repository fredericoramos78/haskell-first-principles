{-# LANGUAGE InstanceSigs #-}

module Chapter22.Asks where

newtype Reader a b = Reader { runReader :: a -> b }

ask :: Reader a a 
ask = Reader id 

asks :: (r -> a) -> Reader r a 
asks = Reader

instance Functor (Reader r) where 
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where 
    pure :: a -> Reader r a
    pure a = Reader $ const a
    
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

