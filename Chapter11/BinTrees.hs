module Chapter11.BinTrees where


data BinaryTree a = 
      Leaf 
    | Node (BinaryTree a) a (BinaryTree a) 
    deriving (Eq, Ord, Show)


mapTree :: (a -> b) 
    -> BinaryTree a 
    -> BinaryTree b 
    
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay = 
    if mapTree (+1) testTree' == mapExpected 
    then putStrLn "mapOkay: yup OK!"
    else putStrLn "mapOkay: test failed!"


preorder :: BinaryTree a -> [a] 
preorder Leaf = [] 
preorder (Node left a right) = (a : preorder left) ++ preorder right

inorder :: BinaryTree a -> [a] 
inorder Leaf =[]
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a] 
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO () 
testPreorder = 
    if preorder testTree == [2, 1, 3] 
    then putStrLn "preOrder: fine!" 
    else putStrLn "preOrder: bad news bears." 

testInorder :: IO () 
testInorder =
    if inorder testTree == [1, 2, 3] 
    then putStrLn "inOrder: fine!" 
    else putStrLn "inOrder bad news bears."

testPostorder :: IO () 
testPostorder = 
    if postorder testTree == [1, 3, 2] 
    then putStrLn "postorder: fine!" 
    else putStrLn "postorder: bad news bears"


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ i Leaf = i
foldTree f i (Node left a right) = foldTree f aPlusLeft right 
    where aPlusLeft = f a forLeft
          forLeft = foldTree f i left
          
testFoldTreeAdd :: IO () 
testFoldTreeAdd =
    if foldTree (*) 1 testTree == 6
    then putStrLn "foldTree: fine!" 
    else putStrLn "foldTree: bad news bears."
       

main :: IO () 
main = do
  mapOkay
  testPreorder
  testInorder
  testPostorder
  testFoldTreeAdd