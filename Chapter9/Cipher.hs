module Chapter9.Cipher where 

import Data.Char
import Data.List

import Control.Monad (forever)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)


shiftR :: Char -> Int -> (Int -> Int -> Int -> Int) -> [Char] -> Char 
shiftR c _ _ [] = c
shiftR c p f cs = cs!!s 
    where s = f pos p len
          pos = case elemIndex c cs of 
                Just p -> p
                Nothing -> error "not found in charset"
          len = length cs

selectCharSet :: Char -> [Char]
selectCharSet c 
    | isUpper c = ['A'..'Z']
    | isLower c = ['a'..'z']
    | otherwise = []

cipher :: [Char] -> Int -> (Int -> Int -> Int -> Int) -> [Char]
cipher [] _ _ = []
cipher s p f 
    | p < 1 = error "must define how much to shift"
    | otherwise = map (\c -> shiftR c p f $ selectCharSet c) s

rotateR :: Int -> Int -> Int -> Int
rotateR pos p = rem (pos + p) 

cipherR :: [Char] -> Int -> [Char]
cipherR s p = cipher s p rotateR

uncipherR :: [Char] -> Int -> [Char]
uncipherR = cipherL

rotateL pos p l = if p' < 0 then l + p' else p'
                  where p' = pos - p

cipherL :: [Char] -> Int -> [Char]
cipherL s p = cipher s p rotateL


uncipherL :: [Char] -> Int -> [Char]
uncipherL = cipherR

cipherRRun :: IO ()
cipherRRun = forever $ do
    hSetBuffering stdout NoBuffering 
    putStrLn "Type what you want to encrypt:"
    line <- getLine
    putStrLn "How many chars to push to the right?"
    encryptLen <- getLine
    let encrypted = cipherR line $ read encryptLen
    putStrLn $ "Your message encrypted is: " ++ encrypted