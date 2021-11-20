module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import System.Random (randomRIO)


type WordList = [String]

-- list of all words contained in the dictionary file
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt" 
  return (lines dict)

-- words filtered according to word size limits
gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter onlyWithinLenRange aw)
    where onlyWithinLenRange :: String -> Bool 
          onlyWithinLenRange w = let wordLen = length w 
                                 in wordLen >= minWordLength && wordLen <= maxWordLength

-- puzzle word size limits 
minWordLength :: Int 
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- random logic to pull a word from the list
randomWord :: WordList -> IO String 
randomWord wl = do
                  randomIndex <- randomRIO (0, length wl - 1)
                  return $ wl !! randomIndex

randomWord' :: IO String
-- randomWord' = gameWords >>= randomWord
randomWord' = do
                wl <- gameWords 
                randomWord wl



-- gameplay 
data Puzzle = Puzzle String [Maybe Char] [Char]
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' $
      fmap renderPuzzleChar discovered
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle 
freshPuzzle s = Puzzle s (maskedWord s) []
                where maskedWord s = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool 
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool 
alreadyGuessed (Puzzle _ _ guessedChars) c = c `elem` guessedChars

renderPuzzleChar :: Maybe Char -> Char 
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledSoFar guessedChars) c =
  Puzzle word newFilledSoFar (c : guessedChars)
  where newFilledSoFar = zipWith (zipper c) word filledSoFar 
        zipper c wordChar guessedChar = 
          if c == wordChar 
            then Just c 
            else guessedChar 

handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True)  -> do
        putStrLn "You already guessed that character, pick something else!"
        return puzzle
    
    (foundChar, _)  -> do
      if foundChar
        then 
          putStrLn "This character was in the word, filling in the word accordingly"
        else
          putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 
    then 
      do putStrLn "You lose!"
         putStrLn $ "The word was: " ++ wordToGuess
         exitSuccess 
    else 
      return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar 
    then 
      do putStrLn "You win!"
         exitSuccess 
    else 
      return ()


runGame :: Puzzle -> IO () 
runGame puzzle = forever $ do
  gameOver puzzle 
  gameWin puzzle 
  putStrLn $ "Current puzzle is: " ++ show puzzle 
  putStr "Guess a letter: "
  guess <- getLine 
  case guess of
    [c] -> handleGuess puzzle c >>= runGame 
    _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering 
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
