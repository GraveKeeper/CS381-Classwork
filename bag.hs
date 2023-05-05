import HW2types
import Data.List
import Data.char

bagToL :: (Ord a ) => Bag a -> [a]
bagToL [] = []
--base case for an empty list
bagToL ((x,n):ys)
    | n == 1 = x:(bagToL ys)
    | otherwise = x:(bagToL ((x,(n-1)):ys))
--x = some number
--n number of duplicates that need to be in list
--ys = the remainin items in the list
doubleB :: Bag a -> Bag a 
doubleB [] = []
doubleB ((x,n):ys) = (x,(n*2)):(doubleB ys)

-- double a Pair

dpair :: (a, Int) -> (a, Int)
dpair (x,n) = (x, (2*n))


doubleB2 :: Bag a -> Bag a 
doubleB2 ys = map dpair ys


doubleB3 :: Bag a -> Bag a
doubleB3 ys = [ (x,(2*n)) | (x,n) <- ys]


--Graphs 
g1 :: Graph
g1 = [(1,2),(3,4), (6,2), (1,4), (1,5)]

--create a list of first nodes of edges

firstN :: Graph -> [Node]
firstN [] = []
firstN ((n1,n2):ns) = n1:(firstN ns)
--OR this
firstN2 :: Graph -> [Node]
firstN2 g = [n1 | (n1,n2) <- g]
--Both do the same thing "in theory"
--fisrt number of graph returns list of nodes


--area of a shape
c1 = Circle (2,1) 5
r1 = Rect (0,0) 3 5
p1 = Pt (3,2) --point
c2 - Circle (1,1) 1

area :: Shape -> Float
area (Pt _) = 0.0 --area of point returns 0
area (Circle _ r) = pi* fromIntegral r
area (Rect _ s1 s2) = fromIntegral (s1*s2) --wants to return a float



--Inputs
--(x,y)
--xs->
--[(8,2),(3,4),(10,1)]
--[8,8,3,3,3,3,10]