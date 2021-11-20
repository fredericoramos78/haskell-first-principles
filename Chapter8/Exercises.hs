module Chapter8.Exercises where 

import Data.List (intersperse)
import Data.Char (digitToInt)


-- Exercise 1:

-- 1. (d) [[Bool]]
-- 2. (b) [[3 == 3], [6 > 5], [3 < 4]]
-- 3. (d) All of the above
-- 4. (b) func "Hello" "World"

-- Exercise 2:
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy = flip cattyConny
appedCatty = cattyConny "woops" 
frappe = flippy "haha"

-- 1. What is the value of appedCatty "woohoo!"? Try to determine the answer for yourself, then test it in the REPL.
--   "woops mrow woohoo!"
-- 2. frappe "1"
--   "1 mrow haha"
-- 3. frappe (appedCatty "2")
--    "woops mrow 2 mrow haha"
-- 4. appedCatty (frappe "blue")
--    "woops mrow blue mrow haha"
-- 5. cattyConny (frappe "pink")
--               (cattyConny "green"
--                   (appedCatty "blue"))
--   "pink mrow haha mrow green mrow woops mrow blue"
-- 6. cattyConny (flippy "Pugs" "are") "awesome"
--   "are mrow Pugs mrow awesome"


-- Exercise 3:
-- 1. Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.
-- dividedBy 15 2 = go 15 2 0
--     go (15-2) 2 0+1 -> go 13 2 1
--     go (13-2) 2 1+1 -> go 11 2 2
--     go (11-2) 2 2+1 -> go  9 2 3   
--     go (9-2) 2 3+1  -> go  7 2 4
--     go (7-2) 2 4+1  -> go  5 2 5
--     go (5-2) 2 5+1  -> go  3 2 6
--     go (3-2) 2 6+1  -> go  1 2 7
--     (7, 1)

-- 2. Write a function that recursively sums all numbers from 1 to n, n being the argument. So if n is 5, you’d add 1+2+3+4+5 to get 15.
-- The type should be (Eq a, Num a) => a -> a.
recSum :: (Eq a, Num a) => a -> a
recSum n = go n 0 0
    where go limit num acc
            | num == limit = acc + num
            | otherwise = go limit (num+1) (acc+num)

-- 3. Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a.
recMult :: (Integral a) => a -> a -> a
recMult a b 
   | b == 0 = 0
   | b == 1 = a
   | otherwise = a + recMult a (b-1)

-- Exercise 4:
data DivisionResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integer -> Integer -> DivisionResult
dividedBy num 0 = DividedByZero
dividedBy num denom = Result . coditionalNeg $ (go (abs num) (abs denom) 0)
    where coditionalNeg = conditionalNegate shouldNegate
          shouldNegate = divisionIsNegative num denom
          go n d count
            | n < d = count
            | otherwise = go (n - d) d (count + 1)

conditionalNegate :: Bool -> Integer -> Integer
conditionalNegate flag num = if flag then negate num else num 

divisionIsNegative :: Integer -> Integer -> Bool
divisionIsNegative n d = (n * d) < 0

-- Exercise 5: 
m91 :: Integer -> Integer
m91 n 
    | n > 100 = n - 10
    | otherwise = m91 . m91 $ n + 11

-- Exercise 6:
digitToWord :: Int -> String 
digitToWord n = ["zero","one","two","three","four","five","six","seven","eight","nine"]!!n

digitsToWords :: [Int] -> [String]
digitsToWords = map digitToWord

digits :: Int -> [Int] 
digits n = map digitToInt $ show n

concatWithHiphen :: [String] -> [String]
concatWithHiphen = intersperse "-"

wordNumber :: Int -> String 
wordNumber = concat . concatWithHiphen . digitsToWords . digits