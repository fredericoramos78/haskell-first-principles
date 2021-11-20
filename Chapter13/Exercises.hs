module Chapter13.Exercises where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

-- Here is a very simple, short block of code. Notice it has a forever that will make it keep running, 
-- over and over again. Load it into your REPL, and test it out. Then, refer back to the chapter, and modify 
-- it to exit successfully after a False result:

palindrome :: IO () 
palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
        True -> putStrLn "It's a palindrome!" 
        False -> do  
            putStrLn "Nope!"
            exitSuccess

onlyChars :: String -> String 
onlyChars = filter (\c -> c `elem` ['a'..'z'])

palindromeSentence :: IO () 
palindromeSentence = forever $ do
    line1 <- getLine
    let sentence = onlyChars $ map toLower line1
    case (sentence == reverse sentence) of
        True -> putStrLn "It's a palindrome!" 
        False -> do  
            putStrLn "Nope!"
            exitSuccess


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = 
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow 
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO () 
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Type your name: "
    name <- getLine
    putStr "Now your age: "
    ageStr <- getLine
    let age = read ageStr
    case mkPerson name age of 
        (Right p) -> putStrLn $ "Yay! Successfully got a person: " ++ show p
        (Left e)  -> putStrLn $ "Ops! " ++ show e