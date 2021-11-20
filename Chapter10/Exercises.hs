module Chapter10.Exercises where

-- Exercise 1: 
-- Given the following sets of consonants and vowels:
stops  = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. 
-- These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of 
-- them will.
svs = [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]

-- b) Modify that function so that it only returns the combinations that begin with a p.
svsP = filter startsWithP svs
        where startsWithP (p, _, _) = p == 'p' 

-- c) Now set up lists of nouns and verbs (instead of stops and vowels), and modify the function to make tuples representing 
--    possible noun-verb-noun sentences.
nouns = ["n1", "n2"]
verbs = ["verb1", "verb2"]
nvn = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns ]


-- Exercise 2:
-- What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it 
--   in the REPL to verify it:
-- This computes the average number of letters per word in the inputted String
seekritFunc :: String -> Int 
seekritFunc x = 
    div (sum (map length (words x)))
           (length (words x))


-- Exercise 3: 
-- We’d really like the answer to be more precise. Can you rewrite that using fractional division?
seekritFunc' :: String -> Double 
seekritFunc' x = fromIntegral sumW / fromIntegral lenW
                where sumW = sum (map length (words x))
                      lenW = length (words x)


-- Exerise 4:
-- In the previous chapter, you wrote these functions using direct recursion over lists. The goal now is to rewrite them 
--  using folds. Where possible, to gain a deeper understanding of folding, try rewriting the fold version so that it is 
--  point-free.

-- a. myOr returns True if any Bool in the list is True: 
myOr :: [Bool] -> Bool
myOr = foldr (||) False 

-- b. myAny returns True if a -> Bool applied to any of the values in the list returns True:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False 


-- c. Write two versions of myElem. One version should use folding and the other should use any:
myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (a ==)) False 

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = myAny (a ==)

-- d. Implement myReverse. Don’t worry about trying to make it lazy: 
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- e. Write myMap in terms of foldr. It should have the same behavior as the built-in map:
myMap :: (a -> b) -> [a] -> [b] 
myMap f = foldr ((:) . f) []

-- f. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter:
myFilterF :: (a -> Bool) -> a -> [a]
myFilterF f a = [a | f a]
myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter f = foldr (\a z -> if f a then a : z else z) []

myFilter' :: (a -> Bool) -> [a] -> [a] 
myFilter' f = foldr ((++) . (myFilterF f)) []

-- g. squish flattens a list of lists into a list: 
squish :: [[a]] -> [a]
squish = foldr (++) []

-- h. squishMap maps a function over a list and concatenates the result:
squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f = foldr ((++) . f) []

-- i. squishAgain flattens a list of lists into a list. This time, re-use the squishMap function: 
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (:[]) . concat

-- j. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the 
--    last value that the comparison returns GT for:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f l 
    | length l < 2 = error "empty must have at least 2 elems"
    | otherwise = foldr (\x y -> if f x y == GT then x else y) (head l) (tail l)

-- k. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last 
-- value that the comparison returns LT for:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
myMinimumBy f l 
    | length l < 2 = error "empty must have at least 2 elems"
    | otherwise = foldr (\x y -> if f x y == LT then x else y) (head l) (tail l)