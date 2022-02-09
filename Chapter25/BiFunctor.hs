module Chapter25.BiFunctor where

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c 
    first f = bimap f id
    second :: (b -> c) -> p a b -> p a c 
    second = bimap id

--1. 
data Deux a b = Deux a b

instance Bifunctor Deux where 
    bimap f g (Deux a b) = Deux c d 
                  where c = f a 
                        d = g b 

-- 2. 
data Const a b = Const a

instance Bifunctor Const where 
    bimap f _ (Const a) = Const (f a)

-- 3. 
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where 
    bimap f g (Drei a b c) = Drei a d e 
                                where d = f b 
                                      e = g c

-- 4. 
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where 
    bimap f _ (SuperDrei a b) = SuperDrei a d 
                                where d = f b


-- 5. 
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where 
    bimap _ _ (SemiDrei a) = SemiDrei a

-- 6. 
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where 
    bimap f g (Quadzzz a b c d) = Quadzzz a b c' d' 
                                    where c' = f c 
                                          d' = g d 

-- 7. 
data EitherA a b = LeftA a | RightA b

instance Bifunctor EitherA where 
    bimap f _ (LeftA a) = LeftA $ f a
    bimap _ g (RightA b) = RightA $ g b
