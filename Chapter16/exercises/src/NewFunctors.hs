{-# LANGUAGE FlexibleInstances #-}

module NewFunctors where

--1. 
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where 
    fmap _ Finance = Finance 
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2. No, it’s not interesting by itself:
data K a b = K a

instance Functor (K a) where 
    fmap f (K a) = K a

-- 3. 
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

-- This should remind you of an instance you've written before 
instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip (K' (f a)) 

-- 4. 
data EvilGoateeConst a b = GoatyConst b
     -- You thought you'd escaped the goats
     -- by now didn't you? Nope.

instance Functor (EvilGoateeConst a) where 
    fmap f (GoatyConst b) = GoatyConst (f b)   

-- 5. Do you need something extra to make the instance work?
data LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where 
    fmap f (LiftItOut v) = LiftItOut (fmap f v)

-- 6. 
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa v v') = DaWrappa (fmap f v) (fmap f v')


-- 7. Don’t ask for more type class instances than you need. You can let GHC tell you what to do:
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething s v) = IgnoringSomething s (fmap f v)

-- 8. 
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where 
    fmap f (Notorious s s' v) = Notorious s s' (fmap f v)

-- 9. You’ll need to use recursion:
data List a = Nil | Cons a (List a)

instance Functor List where 
    fmap _ Nil = Nil 
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10. A tree of goats forms the Goat-Lord, a fearsome poly-creature:
data GoatLord a = 
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
-- A VERITABLE HYDRA OF GOATS

instance Functor GoatLord where 
    fmap _ NoGoat = NoGoat 
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- 11. You’ll use an extra functor for this one, although your solution might do it 
-- monomorphically without using fmap. Keep in mind that you will probably not be 
-- able to validate this one in the usual manner. Do your best to make it work:

data TalkToMe a = 
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where 
    fmap _ Halt = Halt 
    fmap f (Print s v) = Print s (f v)
    -- f . f' will take the `a` result from f' and input it into the fmap `f` 
    --    function to produce an instance of `b`
    fmap f (Read f') = Read (f . f')