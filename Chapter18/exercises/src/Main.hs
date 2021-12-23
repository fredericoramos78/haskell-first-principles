module Main where

import Nope
import BahEither
import Identity 
import List

import Functions

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Semigroup

main :: IO ()
main = do
  putStr "***** Checking Nope a type *****"
  let n = undefined :: Nope (Int, Int, Int)
  quickBatch $ functor n
  quickBatch $ applicative n
  quickBatch $ monad n
  putStr "***** Checking BahEither a b type *****"
  let e = undefined :: BahEither (Int, Int, Int) (Int, Int, Int)
  quickBatch $ functor e
  quickBatch $ applicative e
  quickBatch $ monad e
  putStr "***** Checking Identity a type *****"
  let i = undefined :: Identity (Int, Int, Int)
  quickBatch $ functor i
  quickBatch $ applicative i
  quickBatch $ monad i
  putStr "***** Checking List a type *****"
  let l = undefined :: List (Int, Int, Int)
  quickBatch $ functor l
  quickBatch $ applicative l
  quickBatch $ monad l