module Chapter9.Exercises where 

import Data.Char


-- Exercise 1

-- 1. inspect the type of isUpper and toUpper
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2. Write that function such that, given the input "HbEfLrLxO", your function will return "HELLO".
uppersOnly :: [Char] -> [Char]
uppersOnly = filter isUpper

-- 3. Write a function that will capitalize the first letter of a string and return the entire string
capz :: [Char] -> [Char]
capz [] = []
capz (x:xs) = toUpper x : xs

-- 4. Now make a new version of that function that is recursive, such that if you give it the input "woot", it will holler back at you "WOOT".
allCapz :: [Char] -> [Char]
allCapz [] = []
allCapz (x:xs) = toUpper x : allCapz xs

-- 5. Now write a function that will capitalize the first letter of a String and return only that letter as the result.
headCapz :: [Char] -> Char
headCapz = toUpper . head


-- Exercise 2 (check Cipher.hs)

-- Exercise 3

-- 1. myOr returns True if any Bool in the list is True:
myOr :: [Bool] -> Bool 
myOr s = True `elem` s

-- 2. myAny returns True if a -> Bool applied to any of the values in the list returns True:
myAny :: (a -> Bool) -> [a] -> Bool 
myAny f = myOr . map f

-- 3. After you write the recursive myElem, write another version that uses any. 
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (x':xs) = x == x' || myElem x xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (x ==)

-- 4. Implement myReverse: 
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5. squish flattens a list of lists into a list: 
squish :: [[a]] -> [a]
-- this is the same as `concat`
squish xs = [x | x' <- xs, x <- x']

-- 6. squishMap maps a function over a list and concatenates the results:
squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f xs = [x | x' <- xs, x <- f x']

-- 7. squishAgain flattens a list of lists into a list. This time, re-use the squishMap function:
squishAgain :: [[a]] -> [a] 
squishAgain = squishMap id 

-- 8. myMaximumBy takes a comparison function and a list and returns the greatest element of the 
--    list based on the last value that the comparison returns GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = case f x maxOfXs of 
                          GT -> x 
                          _ -> maxOfXs
                       where maxOfXs = myMaximumBy f xs

 -- 9. myMinimumBy takes a comparison function and a list and returns the least element of the 
 --    list based on the last value that the comparison returns LT for
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = case f x minOfXs of 
                          LT -> x 
                          _ -> minOfXs
                       where minOfXs = myMinimumBy f xs

-- 10. Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum.
myMaximum :: (Ord a) => [a] -> a 
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a 
myMinimum = myMinimumBy compare
