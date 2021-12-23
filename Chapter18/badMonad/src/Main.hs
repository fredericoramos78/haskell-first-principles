module Main where

import BadMonad 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined 
  quickBatch $ functor trigger 
  quickBatch $ applicative trigger 
  quickBatch $ monad trigger
