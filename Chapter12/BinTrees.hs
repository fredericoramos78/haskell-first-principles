module Chapter12.BinTrees where

import Data.Maybe

data BinaryTree a = Leaf
                    | Node (BinaryTree a) a (BinaryTree a) 
                    deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree:
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
unfold f i = case f i of 
                 Just (right, center, left) -> Node (unfold f right) center (unfold f left)
                 Nothing -> Leaf

-- 2. Make a tree builder. Using the unfold function you made for BinaryTree, write the following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild l = unfold (treeBuildF l) 0

-- treeBuildF increments the initial i, building Just instances up to the point where it hits the limit (l)
treeBuildF :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
treeBuildF l i
 | i < l = Just (i+1, i, i+1)
 | otherwise = Nothing 