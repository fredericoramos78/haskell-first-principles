module Main where

import Identity 
import Constant 
import Optional 
import List 
import Pair 
import Three 
import Big 
import Bigger 
import SkiFree 
import Tree 

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



type SM a = S Maybe

main = do
  let identityI :: Identity (Int, Int, [Int])
      identityI = undefined 
  let identityI' :: Identity (Int, Int, [Int], Int, Int)
      identityI' = undefined
  putStr " ***** Testing Identity a *****" 
  quickBatch (functor identityI)
  quickBatch (foldable identityI')
  quickBatch (traversable identityI)
  --
  let constantI :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      constantI = undefined 
  let constantI' :: Constant (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int)
      constantI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing Constant a b *****" 
  quickBatch (functor constantI)
  quickBatch (foldable constantI')
  quickBatch (traversable constantI)
  --
  let optionalI :: Optional (Int, Int, [Int])
      optionalI = undefined 
  let optionalI' :: Optional (Int, Int, [Int], Int, Int)
      optionalI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing Optional a *****" 
  quickBatch (functor optionalI)
  quickBatch (foldable optionalI')
  quickBatch (traversable optionalI)  
  --
  let listI :: List (Int, Int, [Int])
      listI = undefined 
  let listI' :: List (Int, Int, [Int], Int, Int)
      listI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing List a ***** " 
  quickBatch (functor listI)
  quickBatch (foldable listI')
  quickBatch (traversable listI)  
  --
  let pairI :: Pair (Int, Int, [Int]) (Int, Int, [Int])
      pairI = undefined 
  let pairI' :: Pair (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int)
      pairI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing Pair a b  ***** " 
  quickBatch (functor pairI)
  quickBatch (foldable pairI')
  quickBatch (traversable pairI)  
  --
  let threeI :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
      threeI = undefined 
  let threeI' :: Three (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int)
      threeI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing Three a b c ***** " 
  quickBatch (functor threeI)
  quickBatch (foldable threeI')
  quickBatch (traversable threeI)  
  --
  let bigI :: Big (Int, Int, [Int]) (Int, Int, [Int]) 
      bigI = undefined 
  let bigI' :: Big (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int)
      bigI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing Big a b ***** " 
  quickBatch (functor bigI)
  quickBatch (foldable bigI')
  quickBatch (traversable bigI) 
  --
  let biggerI :: Bigger (Int, Int, [Int]) (Int, Int, [Int]) 
      biggerI = undefined 
  let biggerI' :: Bigger (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int)
      biggerI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing Bigger a b ***** " 
  quickBatch (functor biggerI)
  quickBatch (foldable biggerI')
  quickBatch (traversable biggerI)
  --
  let sI :: SM (Int, Int, [Int]) (Int, Int, [Int]) 
      sI = undefined 
  let sI' :: SM (Int, Int, [Int], Int, Int) (Int, Int, [Int], Int, Int)
      sI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Testing S n b ***** " 
  quickBatch (functor sI)
  quickBatch (foldable sI')
  quickBatch (traversable sI)
 --
  let treeI :: Tree (Int, Int, [Int])
      treeI = undefined 
  let treeI' :: Tree (Int, Int, [Int], Int, Int)
      treeI' = undefined
  putStrLn " "
  putStrLn " "
  putStr " ***** Tree a ***** " 
  quickBatch (functor treeI)
  quickBatch (foldable treeI')
  quickBatch (traversable treeI)