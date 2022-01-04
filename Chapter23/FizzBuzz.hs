module Chapter23.FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State


fizzBuzz :: Integer -> String

fizzBuzz n 
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod `5 == 0 = "Buzz" 
    | n `mod `3 == 0 = "Fizz" 
    | otherwise = show n 

main :: IO () 
main = mapM_ (putStrLn . fizzBuzz) [1..20]

-- with state
fizzbuzzList :: [Integer] -> [String] 
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] () 
addResult n = do
    xs <- get
    let result = fizzBuzz n 
    put (result : xs)

main' :: IO () 
main' = mapM_ putStrLn $ reverse $ fizzbuzzList [1..20]

-- no state
fizzbuzzFromTo :: Integer
               -> Integer 
               -> [String] 
fizzbuzzFromTo s e 
    | s <= e = fizzBuzz s : fizzbuzzFromTo (succ s) e
    | otherwise = [] 
    
main'' :: IO ()
main'' = mapM_ putStrLn $ fizzbuzzFromTo 1 20 