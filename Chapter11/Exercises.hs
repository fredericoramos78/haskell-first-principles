module Chapter11.Exercises where

import Data.Char
import Data.List
import Data.Maybe

import Control.Monad (forever)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)


-- Exercise 1

-- 1. Given the following datatype:
data Weekday =
         Monday
       | Tuesday
       | Wednesday
       | Thursday
       | Friday

-- Which of the following is true?
-- Solution: (a)
-- a) Weekday is a type with five data constructors. 
-- b) Weekday is a tree with five branches.
-- c) Weekday is a product type.
-- d) Weekday takes five arguments.

-- With the same datatype definition in mind, what is the type of the following function, f?
-- f Friday = "Miller Time"
-- Solution: (c)
-- a) f :: [Char]
-- b) f :: String -> String
-- c) f :: Weekday -> String 
-- d) f :: Day -> Beer

-- Types defined with the data keyword:
-- Solution: (b) b/c types must begin with a capital letter
-- a) Must have at least one argument. 
-- b) Must begin with a capital letter.
-- c) Must be polymorphic.
-- d) Cannot be imported from modules.

-- The function g xs = xs !! (length xs - 1):
-- Solution: (c)
-- a) Is recursive and may not terminate. 
-- b) Returns the head of xs.
-- c) Returns the final element of xs. 
-- d) Has the same type as xs.


-- Exercise 2
-- In Chapter 9, on lists, you wrote a Caesar cipher. Now, we want to expand on that 
-- idea by writing a Vigenère cipher.
-- test scenario: "MEET AT DAWN" encoded with the keyword "ALLY" should compute “MPPR AE OYWY”

vigenereIt :: String -> String -> String
vigenereIt k s = let encodedMessage = applyKeyword k s 0
                     shiftPerChar = appliedKeywordToShifts encodedMessage
                 in zipWith shiftChar s shiftPerChar

-- maps the keyword over the message
applyKeyword :: String -> String -> Int -> String
applyKeyword _ [] _ = ""
applyKeyword k (' ':xs) p = ' ' : applyKeyword k xs p
applyKeyword k (x:xs) p = k !! (p `mod` length k) : applyKeyword k xs (p+1)

selectCharSet :: Char -> [Char]
selectCharSet c 
    | isUpper c = ['A'..'Z']
    | isLower c = ['a'..'z']
    | otherwise = []

appliedKeywordToShifts :: String -> [Int]
appliedKeywordToShifts = map s2i
    where s2i c = fromMaybe 0 (elemIndex c $ selectCharSet c)

shiftChar :: Char -> Int -> Char 
shiftChar c 0 = c
shiftChar c p = last . take (p+1) $ dropWhile (/= c) $ cycle . selectCharSet $ c

vigenereRun :: IO ()
vigenereRun = forever $ do
    hSetBuffering stdout NoBuffering 
    putStrLn "Type what you want to encrypt:"
    line <- getLine
    putStrLn "Which encrypt key to use?"
    encryptKey <- getLine
    let encrypted = vigenereIt encryptKey line 
    putStrLn $ "Your message encrypted is: " ++ encrypted


-- Exercise 3

-- Use as-patterns to implement the following functions:
-- 1. This should return True if (and only if) all the values in the first list appear in the second 
--    list, though they need not be contiguous:
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True 
isSubseqOf (x:_) [] = False  
isSubseqOf k@(x:xs) s = case elemIndex x s of
                        Just p -> isSubseqOf xs $ drop p s
                        _ -> False 

-- Split a sentence into words, then tuple each one with its capital- ized form:
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\x@(c:cs) -> (x, toUpper c : cs)) $ words s 


-- Exercise 4

upperChars = ['A'..'Z']
lowerChars = ['a'..'z']

-- Write a function that capitalizes a word
capitalizeWord :: String -> String 
capitalizeWord [] = ""
capitalizeWord s@(x:xs) = case elemIndex x lowerChars of  
                            Just p -> upperChars !! p : xs 
                            _ -> s

-- Write a function that capitalizes sentences in a paragraph. Rec- ognize when a new sentence has 
-- begun by checking for periods. Reuse the capitalizeWord function:
capitalizeParagraph :: String -> String 
capitalizeParagraph [] = ""
capitalizeParagraph p = unwords $ map capitalizeSentence $ toSentences p [] 

toSentences :: String -> [String] -> [String]
toSentences "" s = s 
toSentences p s = case elemIndex '.' p of 
                    Just i -> take (i+1) p : toSentences (drop (i+2) p) s
                    _      -> p : s

capitalizeSentence :: String -> String
capitalizeSentence s = unwords (capitalizeWord firstW : restOfSentence)
                          where firstW = head . words $ s
                                restOfSentence = tail . words $ s
