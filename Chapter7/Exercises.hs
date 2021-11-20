module Chapter7.Exercises where 
    
-- Exercise 1

-- 1. A polymorphic function:
-- Solution: (d)
-- a) Changes things into sheep when invoked.
-- b) Has multiple arguments.
-- c) Has a concrete type.
-- d) May resolve to values of different types, depending on inputs. 

-- 2. Two functions named f and g have types Char -> String and String -> [String], respectively. The composed function g . f has the type:
-- Solution: (b)
-- a) Char -> String 
-- b) Char -> [String]
-- c) [[String]]
-- d) Char -> String -> [String]

-- 3. A function f has the type Ord a => a -> a -> Bool, and we apply it to one numeric value. What is the type now?
-- Solution: (d)
-- a) Ord a => a -> Bool 
-- b) Num -> Num -> Bool
-- c) Ord a => a -> a -> Integer 
-- d) (Ord a, Num a) => a -> Bool

-- 4. A function with the type (a -> b) -> c:
-- Solution: (b)
-- a) Requires values of three different types. 
-- b) Is a higher-order function.
-- c) Must take a tuple as its first argument.
-- d) Has its parameters in alphabetical order.

-- 5. Given the following definition of f, what is the type of f True? 
-- f :: a -> a 
-- f x = x
-- Solution: (c)
-- a) f True :: Bool 
-- b) f True :: String
-- c) f True :: Bool -> Bool 
-- d) f True :: a


-- Exercise 2:
tensDigit :: Integral a => a -> a 
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

-- a) First, rewrite it using divMod.
tensDigit' :: Integral a => a -> a 
tensDigit' x = snd . divMod d $ 10
    where d = fst . divMod x $ 10

-- b) Does the divMod version have the same type as the original version?
-- Solution: Yes

-- c) change it so that we’re getting the hundreds digit
hunsD x = snd . divMod d $ 10 
      where d = fst . divMod x $ 100

-- d)  Implement the following function of the type a -> a -> Bool -> a once using a case expression and once with a guard:
foldBool :: a -> a -> Bool -> a 
foldBool thenR elseR cond = case cond of 
    True -> thenR 
    _ -> elseR 

foldBool' :: a -> a -> Bool -> a 
foldBool' thenR elseR cond
    | cond = thenR
    | otherwise = elseR

-- 3. Fill in the definition. Note that the first argument to our function is also a function that can be applied to values. 
-- Your second argument is a tuple, which can be used for pattern matching:
g :: (a -> b) -> (a, c) -> (b, c) 
g f (tupA, tupB) = (f tupA, tupB)
