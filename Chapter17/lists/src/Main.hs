module Main where

import Lists 
import ZipLists
import Validations

import Data.Monoid
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes


main :: IO ()
main = do
  let v = Cons 1 (Cons 2 Nil) :: List (Sum Int)
  quickBatch (monoid v)
  let y = ZipList' [1,2,3] :: ZipList' (Sum Int)
  quickBatch (monoid y)
  let z = Success 10 :: Validation String (Sum Int)
  quickBatch (monoid z)

data Errors =
  DividedByZero
  | StackOverflow
  | MooglesChewedWires deriving (Eq, Show)

