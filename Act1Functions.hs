module Act1Functions where

  range :: Int -> [Int]
  range n 
    | n <= 0 = []
    | otherwise = n : range (n-1)
  
  --copies 1 xs = [xs]
  copies :: Int -> [Int] -> [[Int]]
  copies 0 [] = []
  copies 0 xs = []
  copies 1 xs = [xs]
  copies n xs = xs : copies (n -1) xs
  
  
  greaterList :: (Ord a) => a -> [a] -> [a]
  greaterList n [] = []
  greaterList n (x : xs)
    | x > n = x : (greaterList n xs)
    | otherwise = greaterList n xs
    
  
  allSame :: [Char] -> Bool
  allSame [] = False
  allSame [x] = True
  allSame (x : y : xs)
    | x == y = allSame (y : xs)
    | otherwise = False
  
  --[5,5] (x : y : xs)
  --5 : 5 : [] allSame (y:xs)
  --5 : []
  --5
  

  myMin :: [Int] -> Int
  myMin [] = 0
  myMin [a] = a
  myMin (x:xs)
    | x < myMin xs = x
    | otherwise = myMin xs
    
  myMax :: [Int] -> Int
  myMax [] = 0
  myMax [a] = a
  myMax (x:xs)
    | x > myMax xs = x
    | otherwise = myMax xs
  
  minmax :: [Int] -> (Int,Int)
  minmax xs = (myMin xs , myMax xs)
  
  --minmax2 :: [Int] -> (Int, Int)
  --minmax2 [] = (0,0)
  --minmax2 xs = (minimum xs , maximum xs)