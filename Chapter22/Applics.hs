module Chapter22.Applics where

import Control.Applicative

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

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person 
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog 
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
--        (a -> b) <$> f a -> f b
--        (DogName -> Address -> Dog) <$> (Person -> DogName)
--                                         f = (->) Person 
--                                         a = DogName
--        a = DogName
--        b = (Address -> Dog)
--        <$> ==> (Person -> Address -> Dog)

--        f (a -> b) -> f a -> f b 
--        (Person -> Address -> Dog) <*> (Person -> Address)
--        f = (->) Person 
--        a = Address 
--        b = Dog 
--                                         f = (->) Person
--                                         a = Address
--        <*> ==> Person -> Dog 

-- with Reader, alternative
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address 

-- Here’s the type of liftA2:
-- liftA2 :: Applicative f => (a -> b -> c)                             ->       f a           ->       f b           -> f c
--                            Dog :: DogName -> Address -> Dog          -> (Person -> DogName) -> (Person -> Address) -> (Person -> Dog)



myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
myLiftA2 f a b = 
    f <$> a -- = f (b -> c) 
    <*> b   -- = f c  


