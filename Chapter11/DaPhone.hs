{-# LANGUAGE TupleSections #-}
module Chapter11.DaPhone where

import Data.Maybe
import Data.List
import Data.Char
import Data.Map (fromListWith, toList)

-- This exercise by Twitter user @geophf6 was originally for 1HaskellA- Day.7 Thank you for letting us use this exercise!
-- Remember old-fashioned phone inputs for writing text, where you had to press a button multiple times to get different 
-- letters to come up? You may still have to do this when you try to search for a movie to watch using your television 
-- remote control. You’re going to write code to translate sequences of button presses into strings and vice versa.
-- So! Here is the layout of the phone:
--
-- --------------------------
-- | 1     | 2 ABC  | 3 DEF  | 
-- | 4 GHI | 5 JKL  | 6 MNO  | 
-- | 7 PQRS| 8 TUV  | 9 WXYZ | 
-- | * ^   | 0 + _  | # . ,  | 
-- ---------------------------
--
-- The star (*) capitalizes the current letter, and 0 is your space bar. To represent the digit itself, you press that 
-- digit once more than the letters it represents. If you press a button one more than is required to type the digit, 
-- it wraps around to the first letter. For example:
--
-- 2     -> 'A'
-- 22    -> 'B'
-- 222   -> 'C'
-- 2222  -> '2'
-- 22222 -> 'A'
--
-- So on and so forth. We’re going to kick this around.
-- 1. Create a data structure that captures the phone layout above. The data structure should be able to express enough 
-- of how the layout works that you can use it to dictate the behavior of the functions in the following exercises:
--
-- fill in the rest
data DaPhone = DaPhone [PhoneKey]

data PhoneKey = PhoneKey Digit String

phoneKeys :: DaPhone
phoneKeys = DaPhone [ PhoneKey '1' "",
              PhoneKey '2' "abc2" ,
              PhoneKey '3' "def3" ,
              PhoneKey '4' "ghi4" ,
              PhoneKey '5' "jkl5" ,
              PhoneKey '6' "mno6" ,
              PhoneKey '7' "pqrs7",
              PhoneKey '8' "tuv8" ,
              PhoneKey '9' "wxyz9",
              PhoneKey '*' "^*"   ,
              PhoneKey '0' "+ 0"  ,
              PhoneKey '#' ".,#"
            ]

toUpperKey :: (Digit, Presses)
toUpperKey = ('*', 1)
-- 2. Convert the following conversations into the key presses required to express them. We’re going to suggest types 
-- and functions to complete, in order to accomplish the goal, but they’re not obligatory. If you want to do it differently, 
-- go right ahead:

convo :: [String] 
convo =
       ["Wanna play 20 questions",
       -- Expected:
       -- [W = ('*', 1) ('9', 1)
       --  a = ('2', 1)
       --  n = ('6', 2)
       --  n = ('6', 2)
       --  a = ('2', 1)
       --    = ('0', 2)
       -- p  = ('7', 1)
       -- l  = ('5', 3)
       -- a  = ('2', 1)
       -- y  = ('9', 3)
       --    = ('0', 2)
       -- 2  = ('2', 4)
       -- 0  = ('0', 3)
       --    = ('0', 2)
       -- q  = ('7', 2)
       -- u  = ('8', 2)
       -- e  = ('3', 2)
       -- s  = ('7', 4)
       -- t  = ('8', 1)
       -- i  = ('4', 3)
       -- o  = ('6', 3)
       -- n  = ('6', 2)
       -- s  = ('7', 4)
       -- Output: [
       --   [('*',1),('9',1),('2',1),('6',2),('6',2),('2',1),('0',2),
       --    ('7',1),('5',3),('2',1),('9',3),('0',2),
       --    ('2',4),('0',3),('0',2),
       --    ('7',2),('8',2),('3',2),('7',4),('8',1),('4',3),('6',3),('6',2),('7',4)]
        "Ya",
        "U 1st haha",
        "Lol OK. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "OK. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone
                 -> Char
                 -> [(Digit, Presses)] 
reverseTaps d@(DaPhone k) c 
    | isUpper c = toUpperKey : reverseTaps d (toLower c)
    | otherwise = case filter (\(PhoneKey d xs) -> isJust (elemIndex c xs)) k of 
                    ((PhoneKey d xs):_) -> [(d, fromMaybe 0 (elemIndex c xs) + 1)]
                    _ -> []

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone
                    -> String 
                    -> [(Digit, Presses)] 
cellPhonesDead d = concatMap (reverseTaps d)

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses 
fingerTaps = sum . map snd

-- 4. What is the most popular letter for each message? What was its cost? 
-- You’ll want to combine reverseTaps and fingerTaps to figure out what it costs in taps. 
-- reverseTaps is a list, because you need to press a different button in order to get capitals.
-- mostPopularLetter :: String -> Char 
mostPopularLetter :: [Char] -> Char
mostPopularLetter = fst . mostPopular

mostPopularCost :: [Char] -> Presses
mostPopularCost = snd . mostPopular

mostPopular :: String -> (Char, Presses)
mostPopular = head . calcCharCost

calcCharCost :: String -> [(Char, Presses)]
calcCharCost s = map sumPressesForChar sortedByOcc
                where sortedByOcc = sortGroupedPerPopularity groupedChars
                      groupedChars = filter (isAlpha . fst) $ groupCostPerChar eachCharCost 
                      eachCharCost = map (\c -> (toLower c, [fingerTaps . reverseTaps phoneKeys $ c])) s 

groupCostPerChar :: Ord a => [(a, [Presses])] -> [(a, [Presses])]
groupCostPerChar s = toList $ fromListWith (++) s

sortGroupedPerPopularity :: Ord a => [(a, [Presses])] -> [(a, [Presses])]
sortGroupedPerPopularity = sortBy (flip (\x y -> compare (length . snd $ x) (length . snd $ y)))

sumPressesForChar :: (Char, [Presses]) -> (Char, Presses)
sumPressesForChar (x, y) = (x, sum y) 

fstOft3 :: (a, b, c) -> a
fstOft3 (a, _, _) = a

-- 5. What is the most popular letter overall? What is the overall most popular word?
coolestLtr :: [String] -> Char 
coolestLtr = fstOft3 . coolestOfAll

coolestWord :: [String] -> String 
coolestWord s = fst . head $ groupedBy
    where groupedBy = sortGroupedPerPopularity groupedWords 
          groupedWords = groupCostPerChar inWords
          inWords = map (\x -> (map toLower x, [1])) $ concatMap words s

-- Char with total cost and # it was used
coolestOfAll :: [String] -> (Char, Presses, Int)
coolestOfAll s =  (fst coolest, snd coolest, length $ snd coolestCharWithPresses)
               where coolest = sumPressesForChar coolestCharWithPresses
                     coolestCharWithPresses = head $ sortedByOcc
                     sortedByOcc = sortGroupedPerPopularity groupedChars
                     groupedChars = filter (isAlpha . fst) $ groupCostPerChar popularityOnAllSentences
                     popularityOnAllSentences = map (\(x, y) -> (x, [y])) $ concatMap calcCharCost s
