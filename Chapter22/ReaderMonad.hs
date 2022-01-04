{-# LANGUAGE InstanceSigs #-}
module Chapter22.ReaderMonad where

import Control.Monad (join)

newtype Reader a b = Reader { runReader :: a -> b }


instance Functor (Reader r) where 
    fmap f (Reader ra) = Reader $ \r -> f (ra r) 

instance Applicative (Reader r) where 
    pure x = Reader $ const x
    (<*>) (Reader rBinA) (Reader ra) = Reader $ \r -> rBinA r (ra r)

instance Monad (Reader r) where 
    return = pure
    
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b 
    (Reader ra) >>= aRb = join $ Reader $ \r -> aRb (ra r)
    --                    [Reader r] ([Reader r] b) --> join will make it [Reader r] == m, thus (m a) <==> [Reader r] b
                                     

newtype HumanName = HumanName String deriving (Show, Eq)
newtype DogName = DogName String deriving (Show, Eq)
newtype Address = Address String deriving (Show, Eq)

data Person = Person {
    humanName :: HumanName 
    , dogName :: DogName
    , address :: Address } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
    , dogsAddress :: Address } deriving (Eq, Show)

-- Now for monads
--    r is Person
getDogRM :: Reader Person Dog 
getDogRM =  nameReader >>= dogBuilder 
              where nameReader = Reader $ \p -> dogName p                     
                    dogBuilder name = Reader $ \p -> Dog name $ address p

getDogRM' :: Person -> Dog
getDogRM' = runReader reader
  where
    reader =
      Reader dogName >>= (\dn -> Reader address >>= \adr -> return $ Dog dn adr)