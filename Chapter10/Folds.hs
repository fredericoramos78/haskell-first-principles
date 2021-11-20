module Chapter10.Folds where 
import Text.Printf (IsChar(toChar))

-- 1. foldr (*) 1 [1..5] will return the same result as which of the following?
-- Solution (b) and (c)
-- a) flip (*) 1 [1..5]
-- b) foldl (flip (*)) 1 [1..5]
-- c) foldl (*) 1 [1..5]

-- 2. Write out the evaluation steps for:
-- foldl (flip (*)) 1 [1..3]
-- = foldl f 1 [1,2,3]
-- = foldl f (1*1) [2,3] => foldl f 1 [2,3]
-- = foldl f (2*1) [3] => foldl f 2 [3]
-- = foldl f (3*2) [] => foldr f 6 []
-- = 6

-- foldr (flip (*)) 1 [1..3]
-- = foldr f 1 [1,2,3]
-- = f 1 (foldr f 1 [2,3])          => 1 * (foldr f 1 [2,3])
-- = 1 * (f 2 (foldr f 1 [3]))      => 1 * (2 * (foldr f 1 [3]))
-- = 1 * (2 * (f 3 (foldr f 1 []))) => 1 * (2 * (3 * (foldr f 1 [])))
-- = 1 * (2 * (3 * (1)))            => 1 * (2 * (3))
-- = 1 * (2 * (3))                  => 1 * (6)
-- = 6

-- 3. One difference between foldr and foldl is:
-- Solution (c)
-- a) foldr, but not foldl, traverses the spine of a list from right to left.
-- b) foldr, but not foldl, always forces the rest of the fold.
-- c) foldr, but not foldl, associates to the right.
-- d) foldr, but not foldl, is recursive.

-- 4. Folds are catamorphisms, which means they are generally used to:
-- Solution: (a) 
-- a) Reduce structure. 
-- b) Expand structure.
-- c) Render you catatonic.
-- d) Generate infinite data structures.

-- 5. The following are simple folds very similar to what you’ve already seen, but each has 
--    at least one error. Please fix and test them in your REPL:
-- a) foldr (++) ["woot", "WOOT", "woot"]
f1 = foldr (++) "" ["woot", "WOOT", "woot"]

-- b) foldr max [] "fear is the little death"
f2 = foldr max ' ' "fear is the little death"

-- c) foldr and True [False, True]
f3 = foldr (&&) True [False, True]

-- d) This one is more subtle than the previous. Can it ever return a different answer?
--    foldr (||) True [False, True]
-- Solution: will always say True
f4 = foldr (||) True [False, True]

-- e) foldl ((++) . show) "" [1..5]
-- alternatively use ff5
ff5 :: Show a => [Char] -> a -> [Char]
ff5 x = (++) x . show
f5 = foldl (\x y -> (++) x $ show y) "" [1..5]

-- f) foldr const 'a' [1..5]
-- foldr result type must be the same as z, but const ignores the 2nd arg and keeps resulting
--   in the first one which in this case is each Num in the array. We must make it in a way that
--   foldr ends up in a Char. 
-- Solution: Flipping const will make it return the second arg, which is the Char!
f6 = foldr (flip const) 'a' [1..5]

-- g) foldr const 0 "tacos"
-- Same as (f) but this time I'm fixing on the z arg type
f7 = foldr const '0' "tacos"

-- h) foldl (flip const) 0 "burritos"
f8 = foldl (flip const) '0' "burritos"

-- i) foldl (flip const) 'z' [1..5]
f9 = foldl const 'z' [1..5]

