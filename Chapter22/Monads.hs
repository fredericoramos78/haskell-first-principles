module Chapter22.Monads where

foo :: (Functor f, Num a) => f a -> f a 
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar t f = (t, length f)

-- one function to do both things: increment and len
froot :: Num a => [a] -> ([a], Int)
froot f = (map (+1) f, length f)

-- now using both functions
barPlus :: (Foldable f, Functor f, Num a) => f a -> (f a, Int)
barPlus f = (foo f, length f)

-- now using (foo f) as the first arg to bar
frooty :: (Foldable f, Functor f, Num a) => f a -> (f a, Int)
frooty f = bar (foo f) f

--         bar :: t -> f a' -> (t, Int)
--                t = (f a -> f a) applied to f a, thus f a
--                     f a' -> input (== f a)
--                              (f a, Int)

-- abstracting to any 2 functions
--fooBind :: f1 -> f2 -> f3
-- ouput of f1 is the first arg for f2 
--                    second arg of f2 is the same input from f1
fooBind :: (i1 -> o1) -> (o1 -> i1 -> o2) -> (i1 -> o2)
fooBind f1 f2 r = f2 (f1 r) r

-- now comparing with >>=
-- fooBind :: (i1 -> o1)    -> (o1 -> i1 -> o2)      -> (i1 -> o2)
-- (>>=)   ::  m a          -> (a -> m b)            ->  m b

--             (i1 -> o1)
--             m = i1 ->
--             a = o1
--
--                             (o1 -> [i1 ->] b)
--                             a = o1
--                             m = i1 ->
--                             b = b
--
--                                                    (i1 -> b)
--                                                    m = i1 ->
--                                                    b = b




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
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName  -- this is Person -> String, so the monadic context becomes "Person ->" (or (->) Person)
    addy <- address  -- same monadic context 
    return $ Dog name addy -- return :: a -> m a ==> Dog -> [Person ->] Dog