module Chapter11.DogTypes where 

data Doggies a = 
      Husky a
    | Mastiff a 
    deriving (Eq, Show)

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge


-- Given the datatypes defined in the above sections:
-- 1. Is Doggies a type constructor or a data constructor? 
-- It's a type constructor

-- 2. What is the kind of Doggies?
-- * -> *

-- 3. What is the kind of Doggies String?
-- *

-- 4. What is the type of Husky 10?
-- Num a => Doggies a

-- 5. What is the type of Husky (10 :: Integer)? 
-- Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?
-- Doggies [Char]

-- 7. Is DogueDeBordeaux a type constructor or a data constructor? 
-- both

-- 8. What is the type of DogueDeBordeaux?
-- a => DogueDeBordeaux a

-- 9. What is the type of DogueDeBordeaux "doggie!"
-- DogueDeBordeaux [Char]