
type Bag a = [(a,Int)]
bagToL :: (Ord a ) => Bag a -> [a]
bagToL [] = []

bagToL ((x,n):ys)
    | n == 1 = x:(bagToL ys)
    | otherwise = x:(bagToL ((x,(n-1)):ys))
    return ()