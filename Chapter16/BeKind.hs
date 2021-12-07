module Chapter16.BeKind where

-- Given a type signature, determine the kinds of each type variable:

-- 1. What’s the kind of a?
-- Solution: * -> *
f1 :: a -> a
f1 a = undefined 

-- 2. What are the kinds of b and T? (The T is capitalized on purpose!)
-- Solution: * -> (* -> *) -> (* -> *)
newtype T f = T f

f2 :: a -> b a -> T (b a)
f2 _ _ = undefined 

-- 3. What’s the kind of c?
-- Solution: (* -> * -> *) -> (* -> * -> *)
f3 :: c a b -> c b a
f3 _ = undefined 
