module Chapter12.Exercises where

import Data.Maybe
import Data.List
-- Exercise 1

-- Determine the kinds given. What is the kind of a?

-- `a` is *
myId :: a -> a
myId = undefined 

-- `a` is *
-- `f` is * -> *
r :: a -> f a
r = undefined 


-- Exercise 2 

-- 1. Write a recursive function named replaceThe that takes a text/string, breaks it into words, and replaces each 
-- instance of "the" with "a". It should only replace exactly the word "the". `notThe` is a suggested helper function 
-- for accomplishing this:

notThe :: String -> Maybe String 
notThe "the" = Nothing 
notThe a = Just a 

replaceThe :: String -> String 
replaceThe = unwords . map (f . notThe) . words 
                where f =  fromMaybe "a"

-- 2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of 
-- "the" followed by a vowel-initial word:


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = snd . foldl cFunction (False, 0) . words

cFunction :: (Bool, Integer) -> String -> (Bool, Integer)
cFunction (False, c) []    = (False, c)
cFunction (False, c) s     = (s == "the", c)
cFunction (True,  c) []    = (False, c)
cFunction (True,  c) (x:_) = (False, c + if isSubsequenceOf [x] "aeiou" then 1 else 0)

-- 3. Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. Add any helper func- tions necessary to achieve your objectives:
-- a) Test for vowel-hood.
-- b) Return the vowels of a string.
-- c) Count the number of elements returned.
countVowels :: String -> Integer 
countVowels = sum . map vowelTo1

vowelTo1 :: Char -> Integer 
vowelTo1 c = if c `elem` "aeiou" then 1 else 0

-- Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. 
-- If the number of vowels exceeds the number of consonants, the function returns Nothing. In many human languages, 
-- vowels rarely exceed the number of consonants, so when they do, it may indicate the input isn’t a word (that is, 
-- a valid input to your dataset):
newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word' 
mkWord s = let 
             vowelCount = countVowels s
             consonantCount = toInteger (length s) - vowelCount
           in
               if vowelCount > consonantCount then Nothing else Just (Word' s)


-- You’ll be presented with a datatype to represent the natural numbers. The only values representable with the naturals 
-- are whole numbers from zero to infinity. Your task will be to implement functions to convert natural numbers to integers 
-- and integers to naturals. The conversion from Nat to Integer won’t return Maybe, because—as you know—Integer is a strict 
-- superset of Nat. Any Nat can be represented by an Integer, but the same is not true of any Integer. Negative numbers are 
-- not valid natural numbers:
data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat 
integerToNat i
    | i < 0 = Nothing 
    | i == 0 = Just Zero
    | otherwise = Just (foldl (\x _ -> Succ x) Zero [1..i]) 