module Chapter6.Exercises where 

-- Exercise 1:
-- 1. (c)
-- 2. (a), (b)
-- 3. (a)
-- 4. (c) tuple is (div, mod)
-- 5. (a)

-- Exercise 2:
-- 1. (missing "deriving Show")
data Person = Person Bool deriving Show 
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. (missing "deriving Eq")
data Mood = Blah
          | Woot deriving (Show, Eq)
settleDown x = if x == Woot 
                  then Blah
                  else x

-- 3. 
-- a) What values are acceptable inputs to that function? (Blah or Woot)
-- b) What will happen if you try to run settleDown 9? Why? (Fail type check since 9 is not a Mood)
-- c) What will happen if you try to run Blah > Woot? Why? (Fail since Mood doesn't derive Ord)

-- 4. (type checks fine but note that s1 is a partially applied function while s2 is a "complete" application of Sentence)
type Subject = String
type Verb = String
type Object = String

data Sentence =
     Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Exercise 3:

data Rocks = 
    Rocks String deriving (Eq, Show)

data Yeah = 
    Yeah Bool deriving (Eq, Show)

data Papu = 
    Papu Rocks Yeah deriving (Eq, Show) 

-- 1. Won't type check since arg types are not correct
-- phew = Papu "chases" True

-- 2. Yep!
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3. Yep!
equalityForall :: Papu -> Papu -> Bool 
equalityForall p p' = p == p'

-- 4. Won't type check since Papu has no instance for the Ord type class
-- comparePapus :: Papu -> Papu -> Bool 
-- comparePapus p p' = p > p'


-- Exercise 4: --> Moved to NeedsClarificationExercises.hs

-- Exercise 5: 
-- 1. 
chk :: Eq b => (a -> b) -> a -> b -> Bool 
chk fa2b a1 b2 = fa2b a1 == b2 

-- 2. Hint: use some arithmetic operation to combine values of type b. Pick one:
arith :: Num b 
    => (a -> b)
    -> Integer 
    -> a
    -> b
arith fa2b aInt a = (fa2b a) + (fromInteger aInt)