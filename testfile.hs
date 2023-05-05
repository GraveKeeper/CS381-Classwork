main :: IO ()
main = putStrLn "Hello, World!"


instance Eq Tree where
    (==) x y (inorder x) == (inorder y)

data Tree = Node Int Tree Tree | Leaf 
  deriving Show

myTree :: Tree
myTree = Node 5 (Node 3(Node 1 Leaf Leaf)(Node 4 Leaf Leaf))
    (Node 7 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))

sizeTree :: Tree -> Int
sizeTree Leaf = 0
sizeTree (Node x l r) = 1+ (sizeTree l) + (sizeTree r)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x 1 r) = x  + (treeSum 1) + (treeSum r)

max_value :: Int -> Int -> Int
max_value a b 
 | a >= b = a 
 | otherwise = b 

height :: Tree -> Int
height Leaf = -1
height (Node _ Leaf Leaf) = 0
height (Node _ l r) = max_value  ((height l)+1) ((height r)+1)

inorder :: Tree -> [Int]
inorder Leaf = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

treeInsert:: 
treeInsert x Leaf = singleton x
treeInsert x (Node n left right)
 | x==n = Node n left right
 | x<n = Node n (treeInsert x left) right
 | x>n = Node n left (treeInsert)

buildTree :: [Int] -> Tree
buildTree xs foldr treeInsert Leaf xs

removeValue :: Int -> Tree -> Tree 
removeValue n y = buildTree [x | x <- (inorder y),x/=n]


--HW3 Related:
ppNoun :: Noun -> String
ppNoun Dogs = "dogs"


--Semantics

