
data Tree = Node Int Tree Tree | Leaf
 deriving Show

singleton :: Int -> Tree
singleton x  = Node x Leaf Leaf

treeInsert :: Int -> Tree -> Tree
treeInsert x Leaf = singleton x
treeInsert x (Node n left right)
 | x == n = Node n left right
 | x < n = Node n (treeInsert x left) right
 | x > n = Node n left (treeInsert x right)

numbers = [5, 7, 2, 9, 11, 6]
numberTree = foldr treeInsert Leaf numbers

buildTree :: [Int] -> tree
buildTree xs = foldr treeInsert Leaf xs

mergeTrees :: Tree -> Tree -> Tree
mergeTrees xs ys = let zs = inorder (ys) in
 foldr treeInsert xs zs



