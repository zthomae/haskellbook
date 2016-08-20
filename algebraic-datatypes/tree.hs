module Trees where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)
preorder _ = []

inorder :: BinaryTree a -> [a]
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)
inorder _ = []

postorder :: BinaryTree a -> [a]
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a] 

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f start tree = foldr f start (preorder tree)
